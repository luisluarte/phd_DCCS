# %% [markdown]
## Libs

# %%
# load all libs
import os
import pandas as pd
import nltk
import logging
import string
import pathlib
import itertools
from typing import List, Literal, Optional, Iterator, Tuple, Dict, Any
from sklearn.model_selection import StratifiedKFold
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import SVC
from sklearn.metrics import f1_score, accuracy_score, precision_score, recall_score
from joblib import Parallel, delayed

# %%
# global configs
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
)

# %% [markdown]
# # Function definitions


## %%
# f: filepath -> dataframe
def load_csv_to_dataframe(file_path: str) -> Optional[pd.DataFrame]:
    """
    Functional catch of csv reading errors
    """
    try:
        df = pd.read_csv(file_path)
        logging.info(f"{file_path} is data")
        return df
    except FileNotFoundError:
        logging.error(f"{file_path} not a valid path")
        return None
    except Exception as e:
        logging.error(f"unknown error: {e}")
        return None


## %%
# f: corpus -> pre_processed_corpus
def download_nltk_resources():
    """
    Download all required nltk list, function to catch errors
    """
    try:
        nltk.data.find("tokenizers/punkt")
    except LookupError:
        logging.info("punkt...")
        nltk.download("punkt", quiet=True)
    try:
        nltk.data.find("corpora/stopwords")
    except LookupError:
        logging.info("stopwords...")
        nltk.download("stopwords", quiet=True)
    try:
        nltk.data.find("corpora/wordnet")
    except LookupError:
        logging.info("lemmatizer...")
        nltk.download("wordnet", quiet=True)
    try:
        nltk.data.find("corpora/omw-1.4")
    except LookupError:
        logging.info("omw...")
        nltk.download("omw-1.4", quiet=True)
    logging.info("nltk stuff done")


# cleaning functions objects instatiation
STOP_WORDS = None
STEMMER = None
LEMMATIZER = None
PUNCT_TABLE = None


def initialize_nlp_constants():
    """
    this is just to avoid defining stuff from nltk before
    using the nltk downloader
    """
    global STOP_WORDS, PUNCT_TABLE, STEMMER, LEMMATIZER

    logging.info("init nltk resources...")

    def _check_and_download(resource_id, resource_path):
        try:
            nltk.data.find(resource_path)
            logging.info(f"NLTK resource '{resource_id}' already present")
            nltk.download(resource_id, quiet=True)
            logging.info(f"NLTK resource '{resource_id}' downloaded")
        except LookupError:
            logging.info(f"dowloanding nltk resource: '{resource_id}'...")
            nltk.download(resource_id, quiet=True)
            logging.info(f"nltk resource '{resource_id}' downloaded")

    _check_and_download("punkt", "tokenizers/punkt")
    _check_and_download("punkt_tab", "tokenizers/punkt")
    _check_and_download("stopwords", "corpora/stopwords")
    _check_and_download("wordnet", "corpora/wordnet")
    _check_and_download("omw-1.4", "corpora/omw-1.4")

    STOP_WORDS = set(nltk.corpus.stopwords.words("english"))
    PUNCT_TABLE = str.maketrans("", "", string.punctuation)
    STEMMER = nltk.stem.PorterStemmer()
    LEMMATIZER = nltk.stem.WordNetLemmatizer()

    logging.info("nltk constant initialized")


initialize_nlp_constants()


def _clean_document(
    doc: str,
    to_lower: bool,
    remove_punct: bool,
    remove_stopwords: bool,
    normalization: Literal["none", "stem", "lemmatize"],
) -> str:
    """
    Applies the chosen cleaning strategy to a document
    """

    # lowercasing
    if to_lower:
        doc = doc.lower()

    # punctuation removal
    if remove_punct:
        doc = doc.translate(PUNCT_TABLE)

    # tokenization
    if remove_stopwords or normalization != "none":
        tokens = nltk.word_tokenize(doc)
    else:
        return doc.strip()

    # stopword removal
    if remove_stopwords:
        tokens = [t for t in tokens if t not in STOP_WORDS]

    # normalization
    if normalization == "stem":
        tokens = [STEMMER.stem(t) for t in tokens]
    elif normalization == "lemmatize":
        tokens = [LEMMATIZER.lemmatize(t) for t in tokens]
    elif normalization_strategy == "none":
        pass
    else:
        raise ValueError(f"unknown normalization strat: {normalization_strategy}")

    # re-join everything into single string doc
    return " ".join(tokens)


def combine_text_fields(
    df: pd.DataFrame, strategy: Literal["desc_only", "name_plus_desc"]
) -> pd.Series:

    desc = df["Project description"].fillna("")

    if strategy == "desc_only":
        return desc
    elif strategy == "name_plus_desc":
        name = df["Name"].fillna("")
        return name + " " + desc
    else:
        raise ValueError(f"non valid strategy {strategy}")


def create_preprocessed_corpus(
    df: pd.DataFrame,
    combine_strategy: Literal["desc_only", "name_plus_desc"],
    to_lower: bool,
    remove_punct: bool,
    remove_stopwords: bool,
    normalization: Literal["none", "stem", "lemmatize"],
) -> pd.Series:
    """
    takes raw dataframe and relevant hyperparams and returns
    the preprocessed corpus
    """

    combined_series = combine_text_fields(df, strategy=combine_strategy)

    cleaned_series = combined_series.map(
        lambda doc: _clean_document(
            doc,
            to_lower=to_lower,
            remove_punct=remove_punct,
            remove_stopwords=remove_stopwords,
            normalization=normalization,
        )
    )

    return cleaned_series


def create_folds(
    X: pd.Series, y: pd.Series, k: int, random_state: int = 42
) -> Iterator[Tuple[pd.Index, pd.Index]]:
    """
    wrapper for StratifiedKFold to partition the dataset indices
    yields (train_indices, test_indices) per fold
    stratified to ensure class distribution in folds
    """

    skf = StratifiedKFold(n_splits=k, shuffle=True, random_state=random_state)

    base_indices = X.index

    for train_ilocs, test_ilocs in skf.split(X, y):
        train_indices = base_indices[train_ilocs]
        test_indices = base_indices[test_ilocs]
        yield train_indices, test_indices


def vectorize_fold_data(
    X_train: pd.Series, X_test: pd.Series, vectorizer_hyperparams: Optional[dict] = None
) -> Tuple[pd.DataFrame, pd.DataFrame, TfidfVectorizer]:
    """
    does the vectorization process

    hyperparameters:
        feature granularity: order of n-gram to use
        feature pruning: sort off 'low/high-pass' in term frequency
        weighting: log damping
    """

    if vectorizer_hyperparams is None:
        vectorizer_hyperparams = {}

    vectorizer = TfidfVectorizer(**vectorizer_hyperparams)

    X_train_vec = vectorizer.fit_transform(X_train)

    X_test_vec = vectorizer.transform(X_test)

    logging.info(f"Vectorizer fitted on {len(vectorizer.vocabulary_)} features.")

    return X_train_vec, X_test_vec, vectorizer


def train_kernel_svm_model(
    X_train_vec, y_train_fold: pd.Series, svm_hyperparams: Optional[dict] = None
) -> SVC:
    """
    suport vector machine
    """
    if svm_hyperparams is None:
        svm_hyperparams = {}

    model = SVC(**svm_hyperparams)
    model.fit(X_train_vec, y_train_fold)

    logging.info(f"SVC(kernel={svm_hyperparams.get('kernel', 'rbf')}') model trained")
    return model


def predict(model: SVC, X_test_vec) -> pd.Series:
    """
    predict function
    """
    y_pred = model.predict(X_test_vec)

    return pd.Series(y_pred, name="prediction")


def evaluate_fold(y_true: pd.Series, y_pred: pd.Series) -> Dict[str, float]:
    """
    standard classification metrics
    """
    metrics = {
        "accuracy": accuracy_score(y_true, y_pred),
        "f1": f1_score(y_true, y_pred, average="binary", zero_division=0),
        "precision": precision_score(y_true, y_pred, average="binary", zero_division=0),
        "recall": recall_score(y_true, y_pred, average="binary", zero_division=0),
    }
    return metrics


def aggregate_metrics(fold_metrics_list: List[Dict[str, float]]) -> pd.DataFrame:
    """
    final metrics df
    """
    df = pd.DataFrame(fold_metrics_list)

    mean_metrics = df.mean().rename("mean")
    std_metrics = df.std().rename("std_dev")

    agg_df = pd.concat([mean_metrics, std_metrics], axis=1)

    return agg_df


def generate_preprocessing_params() -> Iterator[Dict[str, Any]]:
    """
    generates the search of preproc options
    """
    combine_strategies = ["desc_only", "name_plus_desc"]
    normalization_strategies = ["none", "stem", "lemmatize"]
    fixed_params = {"to_lower": True, "remove_punct": True, "remove_stopwords": True}

    param_id = 0
    for combine_strat, norm_strat in itertools.product(
        combine_strategies, normalization_strategies
    ):
        param_id += 1
        params = {
            "id": f"P{param_id}",
            "combine_strategy": combine_strat,
            "normalization_strategy": norm_strat,
            **fixed_params,
        }
        yield params


def generate_vectorizer_params() -> Iterator[Dict[str, Any]]:
    """
    generates the vectorization options
    """
    ngram_ranges = [(1, 1), (1, 2), (1, 3)]
    min_dfs = [3, 5]
    fixed_params = {
        "lowercase": False,
        "max_df": 0.95,
        "sublinear_tf": True,
        "norm": "l2",
    }

    param_id = 0
    for ngram, min_df in itertools.product(ngram_ranges, min_dfs):
        param_id += 1
        params = {
            "id": f"V{param_id}",
            "ngram_range": ngram,
            "min_df": min_df,
            **fixed_params,
        }
        yield params


def generate_model_params() -> Iterator[Dict[str, Any]]:
    """
    generate the model options
    """
    kernels = ["linear", "rbf"]
    Cs = [0.1, 1, 10]
    fixed_params = {"class_weight": "balanced", "random_state": 42}

    param_id = 0
    for kernel, c in itertools.product(kernels, Cs):
        param_id += 1
        params = {"id": f"M{param_id}", "kernel": kernel, "C": c, **fixed_params}
        yield params


def run_experiment(
    corpus_df_clean: pd.DataFrame,
    y_labels_clean: pd.Series,
    preprocess_params: Dict[str, Any],
    vectorize_params: Dict[str, Any],
    model_params: Dict[str, Any],
) -> Tuple[str, Dict, pd.DataFrame]:
    """
    composition of the entire k-fold pipeline
    for a single set of hyperparameters
    """
    p_params_clean = preprocess_params.copy()
    v_params_clean = vectorize_params.copy()
    m_params_clean = model_params.copy()

    exp_id = f"{p_params_clean.pop('id')}-{v_params_clean.pop('id')}-{m_params_clean.pop('id')}"

    logging.info(f"starting experiment {exp_id}")

    try:
        X_corpus = create_preprocessed_corpus(corpus_df_clean, **p_params_clean)
        K_FOLDS = 5
        fold_generator = create_folds(X_corpus, y_labels_clean, k=K_FOLDS)

        all_fold_metrics = []

        for train_idx, test_idx in fold_generator:

            X_train_fold = X_corpus.loc[train_idx]
            y_train_fold = y_labels_clean.loc[train_idx]
            X_test_fold = X_corpus.loc[test_idx]
            y_test_fold = y_labels_clean.loc[test_idx]

            X_train_vec, X_test_vec, _ = vectorize_fold_data(
                X_train_fold, X_test_fold, vectorizer_hyperparams=v_params_clean
            )

            fold_model = train_kernel_svm_model(
                X_train_vec, y_train_fold, svm_hyperparams=m_params_clean
            )

            y_pred_dol = predict(fold_model, X_test_vec)

            y_pred_fold.index = y_test_fold.index
            fold_metrics = evaluate_fold(y_test_fold, y_pred_fold)
            all_fold_metric.append(fold_metrics)

        final_metrics = aggregate_metrics(all_fold_metrics)

        return (
            exp_id,
            {"p": preprocess_params, "v": vectorize_params, "m": model_params},
            final_metrics,
            "DONE",
        )

    except Exception as e:
        logging.error(f"failed experiment {exp_id}: {e}")
        return (
            exp_id,
            {"p": preprocess_params, "v": vectorize_params, "m": model_params},
            None,
            f"fail: {e}",
        )


# %% [markdown]
# # Main code execution

# %%
if __name__ == "__main__":

        # --- RUN-TIME PHASE (DATA I/O) ---
    logging.info("--- Grid Search Pipeline Initialized ---")
    
    cwd_path = os.getcwd()
    logging.info(f"Current Working Directory (CWD): {cwd_path}")
    BASE_PATH = pathlib.Path(__file__).parent.resolve()
    file_path = BASE_PATH / "data" / "dataset.csv"
    
    corpus_df = load_csv_to_dataframe(file_path)
    print(corpus_df)

    if corpus_df is not None:
        
        # --- FIX 4: Use correct label column (from your traceback) ---
        label_column_name = 'isEnviromental'
        required_cols = ['Name', 'Project description', label_column_name]
        
        if (False):
            logging.error(f"DataFrame is missing one or more required columns.")
            logging.error(f"Please check your CSV. Required: {required_cols}")
            
        else:
            logging.info("Required columns found. Proceeding.")
            
            # --- FIX 5: Clean Labels (Global Operation) ---
            # Use explicit mapping, not pd.to_numeric
            y_labels_raw = corpus_df[label_column_name]
            valid_indices = y_labels_raw.dropna().index
            
            corpus_df_clean = corpus_df.loc[valid_indices]
            y_labels_clean = y_labels_raw.loc[valid_indices].astype(int)
            
            if len(y_labels_clean.unique()) < 2:
                logging.error(f"ValueError: The number of classes in '{label_column_name}' is less than 2.")
                logging.error("StratifiedKFold cannot proceed. Check your 'label_map' dictionary.")
            else:
                
                # --- The f_grid_search Morphism ---
                logging.info("--- Entering Grid Search (Meta-Morphism) ---")
                
                # --- 1. Compute the Cartesian Product H_total ---
                h_preprocess = list(generate_preprocessing_params())
                h_vectorize = list(generate_vectorizer_params())
                h_model = list(generate_model_params())
                
                h_total = list(itertools.product(h_preprocess, h_vectorize, h_model))
                
                logging.info(f"Total experiments to run: {len(h_total)}")
                
                all_results = []
                
                # --- 2. Iterate (Map Phase) ---
                for (f_params, v_params, m_params) in h_total:
                    
                    # --- Apply f_pipeline ---
                    exp_id, params, metrics_df, status = run_experiment(
                        corpus_df_clean,
                        y_labels_clean,
                        f_params,
                        v_params,
                        m_params
                    )
                    
                    if status == "SUCCESS":
                        all_results.append((exp_id, params, metrics_df))
                
                # --- 3. Reduce Phase (Collate & Report) ---
                logging.info("\n\n" + "="*50)
                logging.info("   GRID SEARCH COMPLETE: TOP 5 RESULTS")
                logging.info("="*50)
                
                # Sort results by the 'mean' of the 'f1' score
                all_results.sort(
                    key=lambda res: res[2].loc['f1', 'mean'], 
                    reverse=True
                )
                
                for (exp_id, params, metrics_df) in all_results[:5]:
                    f_p = params['p']
                    v_p = params['v']
                    m_p = params['m']
                    
                    print(f"\n--- RANK {all_results.index((exp_id, params, metrics_df)) + 1} (ID: {exp_id}) ---")
                    print(f"  Preprocess: combine='{f_p['combine_strategy']}', norm='{f_p['normalization']}'")
                    print(f"  Vectorize:  ngram={v_p['ngram_range']}, min_df={v_p['min_df']}")
                    print(f"  Model:      C={m_p['C']}")
                    print("\n  Metrics:")
                    print(metrics_df.to_string(float_format="%.4f"))
                    print("-"*50)
