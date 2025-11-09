# %% [markdown]
## Libs

# %%
# load all libs
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import nltk
import logging
import string
import pathlib
import itertools
import random
import ast
import re
from typing import List, Literal, Optional, Iterator, Tuple, Dict, Any, Callable, Union
from sklearn.model_selection import StratifiedKFold, cross_val_predict
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.metrics import (
    f1_score,
    accuracy_score,
    precision_score,
    recall_score,
    precision_recall_curve,
)
from sklearn.inspection import PartialDependenceDisplay
from sklearn.preprocessing import OrdinalEncoder
from sklearn.pipeline import Pipeline
from joblib import Parallel, delayed

# BERT related
import torch
from sentence_transformers import SentenceTransformer

# %%
# global configs
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
)

# use GPU if available
DEVICE = "cuda" if torch.cuda.is_available() else "cpu"
logging.info(f"using device: {DEVICE}")

# %% [markdown]
# # Function definitions


# %%
def load_csv_to_dataframe(
    file_path: Union[str, pathlib.Path],
) -> Optional[pd.DataFrame]:
    """
    catch errors importing dataset
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


# %%
def download_nltk_resources():
    """
    download all required nltk list, function to catch errors
    retry downloading, prob is just a bad connection
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
        # download only if its not present
        try:
            nltk.data.find(resource_path)
            logging.info(f"NLTK resource '{resource_id}' already present")
        except LookupError:
            logging.info(f"dowloanding nltk resource: '{resource_id}'...")
            nltk.download(resource_id, quiet=True)
            logging.info(f"nltk resource '{resource_id}' downloaded")

    _check_and_download("punkt", "tokenizers/punkt")
    _check_and_download("stopwords", "corpora/stopwords")
    _check_and_download("wordnet", "corpora/wordnet")
    _check_and_download("omw-1.4", "corpora/omw-1.4")

    # now preproc variables are set
    STOP_WORDS = set(nltk.corpus.stopwords.words("english"))
    PUNCT_TABLE = str.maketrans("", "", string.punctuation)
    STEMMER = nltk.stem.PorterStemmer()
    LEMMATIZER = nltk.stem.WordNetLemmatizer()

    logging.info("nltk constant initialized")


# run the function, can't run it in the main body or we get errors
initialize_nlp_constants()


def _clean_document(
    doc: str,
    to_lower: bool,
    remove_punct: bool,
    remove_stopwords: bool,
    normalization_strategy: Literal["none", "stem", "lemmatize"],
) -> str:
    """
    applies the chosen cleaning strategy to a document
    """

    # lowercasing
    if to_lower:
        doc = doc.lower()

    # punctuation removal
    if remove_punct:
        doc = doc.translate(PUNCT_TABLE)

    # tokenization
    if remove_stopwords or normalization_strategy != "none":
        tokens = nltk.word_tokenize(doc)
    else:
        return doc.strip()

    # stopword removal
    if remove_stopwords:
        tokens = [t for t in tokens if t not in STOP_WORDS]

    # normalization
    if normalization_strategy == "stem":
        tokens = [STEMMER.stem(t) for t in tokens]
    elif normalization_strategy == "lemmatize":
        tokens = [LEMMATIZER.lemmatize(t) for t in tokens]
    elif normalization_strategy == "none":
        pass
    else:
        raise ValueError(
            f"unknown normalization_strategy strat: {normalization_strategy}"
        )

    # re-join everything into single string doc
    return " ".join(tokens)


def combine_text_fields(
    df: pd.DataFrame, strategy: Literal["desc_only", "name_plus_desc"]
) -> pd.Series:
    """
    name_plus_desc was not used, but the idea was that the project name could
    hold some meaningful information, in the dataset this was left with only 'a' to
    fill the gap
    """

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
    normalization_strategy: Literal["none", "stem", "lemmatize"],
) -> pd.Series:
    """
    takes raw dataframe and relevant hyperparams and returns
    the preprocessed corpus
    """

    combined_series = combine_text_fields(df, strategy=strategy)

    cleaned_series = combined_series.map(
        lambda doc: _clean_document(
            doc,
            to_lower=to_lower,
            remove_punct=remove_punct,
            remove_stopwords=remove_stopwords,
            normalization_strategy=normalization_strategy,
        )
    )

    return cleaned_series


def create_folds(
    X: Union[pd.Series, np.ndarray],
    y: Union[pd.Series, np.ndarray],
    k: int,
    random_state: int = 42,
) -> Iterator[Tuple[np.ndarray, np.ndarray]]:
    """
    wrapper for StratifiedKFold to partition the dataset indices
    yields (train_indices, test_indices) per fold
    stratified to ensure class distribution in folds
    """

    skf = StratifiedKFold(n_splits=k, shuffle=True, random_state=random_state)

    for train_ilocs, test_ilocs in skf.split(X, y):
        yield train_ilocs, test_ilocs


def vectorize_fold_data(
    X_train: pd.Series, X_test: pd.Series, vectorizer_hyperparams: Optional[dict] = None
) -> Tuple[pd.DataFrame, pd.DataFrame, TfidfVectorizer]:
    """
    does the vectorization process,
    this was not as good as BERT :c
    """

    if vectorizer_hyperparams is None:
        vectorizer_hyperparams = {}

    vectorizer = TfidfVectorizer(**vectorizer_hyperparams)

    X_train_vec = vectorizer.fit_transform(X_train)

    X_test_vec = vectorizer.transform(X_test)

    return X_train_vec, X_test_vec, vectorizer


def train_kernel_svm_model(
    X_train_vec: Union[pd.DataFrame, np.ndarray],
    y_train_fold: Union[pd.Series, np.ndarray],
    model_hyperparams: Optional[dict] = None,
) -> SVC:
    """
    suport vector machine wrapper to accept all the
    hyperparameters from experiments
    """
    if model_hyperparams is None:
        model_hyperparams = {}

    model_hyperparams["probability"] = True
    model = SVC(**model_hyperparams)
    model.fit(X_train_vec, y_train_fold)
    return model


def train_rf_model(
    X_train_vec: Union[pd.DataFrame, np.ndarray],
    y_train_fold: Union[pd.Series, np.ndarray],
    model_hyperparams: Optional[dict] = None,
) -> RandomForestClassifier:
    """
    same but for random forests
    """
    if model_hyperparams is None:
        model_hyperparams = {}
    model = RandomForestClassifier(**model_hyperparams)
    model.fit(X_train_vec, y_train_fold)
    return model


def predict(
    model: Union[SVC, RandomForestClassifier],
    X_test_vec: Union[pd.DataFrame, np.ndarray],
) -> pd.Series:
    """
    just a wrapper for the predict function
    """
    y_pred = model.predict(X_test_vec)
    return pd.Series(y_pred, name="prediction")


def evaluate_fold(
    y_true: Union[pd.Series, np.ndarray], y_pred: Union[pd.Series, np.ndarray]
) -> Dict[str, float]:
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
    remove_stopwords_opts = [True, False]
    fixed_params = {"to_lower": True, "remove_punct": True}

    param_id = 0
    for combine_strat, norm_strat, remove_sw in itertools.product(
        combine_strategies, normalization_strategies, remove_stopwords_opts
    ):
        param_id += 1
        params = {
            "id": f"P{param_id}",
            "combine_strategy": combine_strat,
            "normalization_strategy": norm_strat,
            "remove_stopwords": remove_sw,
            **fixed_params,
        }
        yield params


def generate_vectorizer_params() -> Iterator[Dict[str, Any]]:
    """
    generates the vectorization options
    """
    ngram_ranges = [(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6)]
    min_dfs = [5, 10, 15]
    max_dfs = [0.7, 0.8, 0.9]
    sublinear_tf_opts = [True, False]
    fixed_params = {
        "lowercase": False,
        "norm": "l2",
    }

    param_id = 0
    for ngram, min_df, max_df_val, sublinear_tf_val in itertools.product(
        ngram_ranges, min_dfs, max_dfs, sublinear_tf_opts
    ):
        param_id += 1
        params = {
            "id": f"V{param_id}",
            "ngram_range": ngram,
            "min_df": min_df,
            "max_df": max_df_val,
            "sublinear_tf": sublinear_tf_val,
            **fixed_params,
        }
        yield params


def generate_svm_params() -> Iterator[Dict[str, Any]]:
    """
    generates the model options
    """
    kernels = ["linear", "rbf"]
    Cs = [0.1, 1, 10, 100]
    gammas = ["scale", 0.1, 1]
    class_weight_opts = ["balanced", None]
    fixed_params = {"random_state": 42}

    param_id = 0
    for class_weight_val in class_weight_opts:
        for kernel in kernels:
            for c in Cs:
                common_params = {
                    "kernel": kernel,
                    "C": c,
                    "class_weight": class_weight_val,
                    **fixed_params,
                }

                if kernel == "linear":
                    param_id += 1
                    params = {"id": f"SVM{param_id}", **common_params}
                    yield params

                elif kernel == "rbf":
                    for gamma in gammas:
                        param_id += 1
                        params = {
                            "id": f"SVM{param_id}",
                            "gamma": gamma,
                            **common_params,
                        }
                        yield params


def generate_rf_params() -> Iterator[Dict[str, Any]]:
    """
    generates random forest search space
    """
    n_estimators = [
        100
    ]  # this is stable enough, tested with more not much difference in performance
    max_depths = [20, 30]
    min_samples_split = [5, 10]
    min_samples_leaf = [2, 5]
    max_features_opts = ["sqrt", 0.5, 0.9]
    class_weight_opts = ["balanced", None]

    fixed_params = {"random_state": 42, "n_jobs": 1}

    param_id = 0

    h_product = itertools.product(
        n_estimators,
        max_depths,
        min_samples_split,
        min_samples_leaf,
        max_features_opts,
        class_weight_opts,
    )

    for n, d, mss, msl, mf, cw in h_product:
        param_id += 1
        params = {
            "id": f"RF{param_id}",
            "n_estimators": n,
            "max_depth": d,
            "min_samples_split": mss,
            "min_samples_leaf": msl,
            "max_features": mf,
            "class_weight": cw,
            **fixed_params,
        }
        yield params


def run_experiment(
    corpus_df_clean: pd.DataFrame,
    y_labels_clean: pd.Series,
    train_model: Callable,
    preprocess_params: Dict[str, Any],
    vectorize_params: Dict[str, Any],
    model_params: Dict[str, Any],
) -> Tuple[str, Dict, pd.DataFrame, str]:
    """
    composition of the entire k-fold pipeline
    for a single set of hyperparameters (TF-IDF Path)
    """
    p_params_clean = preprocess_params.copy()
    v_params_clean = vectorize_params.copy()
    m_params_clean = model_params.copy()

    try:
        exp_id = f"{p_params_clean.pop('id')}-{v_params_clean.pop('id')}-{m_params_clean.pop('id')}"
        logging.info(f"starting experiment {exp_id}")
    except KeyError:
        logging.error("Failed to pop ID from params in run_experiment")
        exp_id = "unknown"

    try:
        X_corpus = create_preprocessed_corpus(corpus_df_clean, **p_params_clean)
        K_FOLDS = 5
        fold_generator = create_folds(X_corpus, y_labels_clean, k=K_FOLDS)

        all_fold_metrics = []

        X_corpus_iloc = X_corpus.iloc
        y_labels_iloc = y_labels_clean.iloc

        for train_ilocs, test_ilocs in fold_generator:

            X_train_fold = X_corpus_iloc[train_ilocs]
            y_train_fold = y_labels_iloc[train_ilocs]
            X_test_fold = X_corpus_iloc[test_ilocs]
            y_test_fold = y_labels_iloc[test_ilocs]

            X_train_vec, X_test_vec, _ = vectorize_fold_data(
                X_train_fold, X_test_fold, vectorizer_hyperparams=v_params_clean
            )

            fold_model = train_model(
                X_train_vec, y_train_fold, model_hyperparams=m_params_clean
            )

            y_pred_fold = predict(fold_model, X_test_vec)

            y_pred_fold.index = y_test_fold.index
            fold_metrics = evaluate_fold(y_test_fold, y_pred_fold)
            all_fold_metrics.append(fold_metrics)

        final_metrics = aggregate_metrics(all_fold_metrics)

        return (
            exp_id,
            {"p": preprocess_params, "v": vectorize_params, "m": model_params},
            final_metrics,
            "SUCCESS",
        )

    except Exception as e:
        logging.error(f"failed experiment {exp_id}: {e}")
        return (
            exp_id,
            {"p": preprocess_params, "v": vectorize_params, "m": model_params},
            None,
            f"fail: {e}",
        )


# BERT functions
def get_bert_embeddings(
    corpus: pd.Series, model_name: str = "all-MiniLM-L6-v2"
) -> np.ndarray:
    """
    load sentencetransformer and encodes the entire corpus
    """
    logging.info(f"loading sentencetransformer model: {model_name}...")
    # 384-dimensional vector
    model = SentenceTransformer(model_name, device=DEVICE)

    logging.info(f"Encoding {len(corpus)} documents")

    embeddings = model.encode(corpus.tolist(), show_progress_bar=True, batch_size=32)
    logging.info(f"Encoding complete. matrix shape: {embeddings.shape}")
    return embeddings


def run_experiment_bert(
    X_bert_full: np.ndarray,
    y_full: np.ndarray,
    train_idx: np.ndarray,
    test_idx: np.ndarray,
    train_model: Callable,
    model_params: Dict[str, Any],
    fold_id: int,
) -> Tuple[str, int, Dict[str, float], Dict[str, Any], str]:  # Added str for status
    """
    instead of vectorization (BERT Path)
    """
    m_params_clean = model_params.copy()
    exp_id = m_params_clean.pop("id")

    try:
        X_train_fold = X_bert_full[train_idx]
        y_train_fold = y_full[train_idx]
        X_test_fold = X_bert_full[test_idx]
        y_test_fold = y_full[test_idx]

        fold_model = train_model(
            X_train_fold, y_train_fold, model_hyperparams=m_params_clean
        )

        y_pred_fold = predict(fold_model, X_test_fold)
        # No index alignment needed for numpy
        fold_metrics = evaluate_fold(y_test_fold, y_pred_fold)

        return (exp_id, fold_id, fold_metrics, model_params, "SUCCESS")

    except Exception as e:
        logging.error(f"failed {exp_id} (fold {fold_id}): {e}")
        return (exp_id, fold_id, {}, model_params, f"fail: {e}")


def create_results_dataframe_bert(
    all_fold_results: List[Tuple[str, int, Dict[str, float], Dict[str, Any], str]],
) -> pd.DataFrame:
    """
    aggregates fold-level results into experiment level dataframe
    """
    df = pd.DataFrame(
        all_fold_results, columns=["exp_id", "fold_id", "metrics", "params", "status"]
    )
    metrics_df = df["metrics"].apply(pd.Series)
    df = pd.concat([df.drop("metrics", axis=1), metrics_df], axis=1)

    agg_df = df.groupby("exp_id").agg(
        f1_mean=("f1", "mean"),
        f1_std=("f1", "std"),
        accuracy_mean=("accuracy", "mean"),
        precision_mean=("precision", "mean"),
        recall_mean=("recall", "mean"),  # --- FIX: Fixed typo 'recal_mean' ---
        status=("status", lambda s: "FAIL" if "fail" in s.unique() else "SUCCESS"),
    )

    # Get params from one of the fold runs
    params_df = df.drop_duplicates(subset="exp_id").set_index("exp_id")["params"]

    final_df = agg_df.join(params_df.apply(pd.Series))

    return final_df.sort_values(by="f1_mean", ascending=False).reset_index()


def parse_best_params_bert(row: pd.Series, model_type: str) -> Dict:
    """
    same kind of parse but for BERT
    """
    m_params = {}
    row_dict = row.to_dict()

    reserved_cols = [
        "exp_id",
        "f1_mean",
        "f1_std",
        "accuracy_mean",
        "precision_mean",
        "recall_mean",
        "status",
        "id",
    ]
    param_keys = [k for k in row_dict.keys() if k not in reserved_cols]

    m_params = {k: row_dict[k] for k in param_keys}

    float_params = ["C", "gamma"]
    for p in float_params:
        if p in m_params:
            try:
                # deal with some hyperparams being str
                # an other numerics
                m_params[p] = float(m_params[p])
            except (ValueError, TypeError):
                pass

    if "max_depth" in m_params and pd.isna(m_params["max_depth"]):
        m_params["max_depth"] = None

    if model_type == "RF":
        m_params["n_jobs"] = -1
        if "max_features" in m_params and isinstance(m_params["max_features"], str):
            if not m_params["max_features"].isalpha():
                m_params["max_features"] = float(m_params["max_features"])

        m_params = {
            k: v for k, v in m_params.items() if (pd.notna(v) or k == "max_depth")
        }

    elif model_type == "SVM":
        m_params["probability"] = True
        m_params = {k: v for k, v in m_params.items() if pd.notna(v)}

    return m_params


def analyze_threshold_bert(
    model: Union[SVC, RandomForestClassifier],
    X_bert_embeddings: np.ndarray,
    y_labels: np.ndarray,
):
    """
    using the best BERT model, optimize the threshold for f1 score
    """
    try:
        s_oof = cross_val_predict(
            model, X_bert_embeddings, y_labels, cv=5, method="predict_proba", n_jobs=-1
        )[:, 1]
    except Exception as e:
        logging.error(f"failed to get oof predictions for BERT: {e}")
        return

    precisions, recalls, thresholds = precision_recall_curve(y_labels, s_oof)

    f1_scores = 2 * (precisions * recalls) / (precisions + recalls + 1e-9)
    valid_f1_scores = f1_scores[:-1]
    valid_thresholds = thresholds

    if len(valid_f1_scores) > 0:
        best_f1_idx = np.argmax(valid_f1_scores)
        optimal_threshold = valid_thresholds[best_f1_idx]
        max_f1 = valid_f1_scores[best_f1_idx]

        logging.info(f"max f1 score (BERT): {max_f1:.4f}")
        logging.info(f"optimal tau (BERT): {optimal_threshold:.4f}")

        plt.figure(figsize=(12, 8))
        plt.plot(
            valid_thresholds, valid_f1_scores, label="F1 Score", color="blue", lw=2
        )
        plt.plot(
            valid_thresholds, precisions[:-1], label="Precision", color="green", ls="--"
        )
        plt.plot(valid_thresholds, recalls[:-1], label="Recall", color="red", ls="--")

        plt.axvline(
            x=optimal_threshold,
            color="black",
            lw=2,
            ls=":",
            label=f"Optimal Threshold (tau* = {optimal_threshold:.2f})\nMax F1 = {max_f1:.2f}",
        )

        plt.title(
            "F1, Precision, and Recall vs. Decision Threshold (BERT)", fontsize=16
        )
        plt.xlabel("Decision Threshold (tau)", fontsize=12)
        plt.ylabel("Score", fontsize=12)
        plt.legend()
        plt.grid(True)
        plt.savefig("threshold_optimization_BERT.png")
        logging.info("saved BERT threshold plot to threshold_optimization_BERT.png")
        plt.close()
    else:
        logging.warning(
            "could not generate threshold plot for BERT: no valid F1 scores."
        )


def create_results_dataframe(
    all_results: List[Tuple[str, Dict, pd.DataFrame, str]],
) -> pd.DataFrame:
    """
    create reports on hyperparameter optimization
    """
    processed_rows = []
    for exp_id, params, metrics_df, status in all_results:
        row = {"exp_id": exp_id, "status": status}
        for p_key, p_val in params.get("p", {}).items():
            row[f"p_{p_key}"] = p_val

        for v_key, v_val in params.get("v", {}).items():
            row[f"v_{v_key}"] = str(v_val) if isinstance(v_val, tuple) else v_val

        for m_key, m_val in params.get("m", {}).items():
            row[f"m_{m_key}"] = m_val

        if status == "SUCCESS" and metrics_df is not None:
            for metric, stats in metrics_df.iterrows():
                row[f"{metric}_mean"] = stats["mean"]
                row[f"{metric}_std_dev"] = stats["std_dev"]

        processed_rows.append(row)

    return pd.DataFrame(processed_rows)


def parse_best_params(row: pd.Series, model_type: str) -> Tuple[Dict, Dict, Dict]:
    """
    extract and parses the best parameters from the grid search
    """
    p_params, v_params, m_params = {}, {}, {}
    row_dict = row.to_dict()

    for key, val in row_dict.items():
        if key.startswith("p_") and "id" not in key:
            p_params[key[2:]] = val
        elif key.startswith("v_") and "id" not in key:
            v_params[key[2:]] = val
        elif key.startswith("m_") and "id" not in key:
            m_params[key[2:]] = val

    # vectorizer params
    if "ngram_range" in v_params:
        if isinstance(v_params["ngram_range"], str):
            try:
                v_params["ngram_range"] = ast.literal_eval(v_params["ngram_range"])
            except (ValueError, SyntaxError):
                logging.error(f"could not parse ngram_range: {v_params['ngram_range']}")
                v_params["ngram_range"] = (1, 1)  # fall back

    if "max_depth" in m_params and pd.isna(m_params["max_depth"]):
        m_params["max_depth"] = None

    if model_type == "RF":
        m_params["n_jobs"] = -1
        if "max_features" in m_params and isinstance(m_params["max_features"], str):
            if not m_params["max_features"].isalpha():
                m_params["max_features"] = float(m_params["max_features"])

        m_params = {
            k: v for k, v in m_params.items() if (pd.notna(v) or k == "max_depth")
        }

    elif model_type == "SVM":
        m_params["probability"] = True
        m_params = {k: v for k, v in m_params.items() if pd.notna(v)}

    return p_params, v_params, m_params


def rebuild_best_model(v_params: Dict, m_params: Dict, model_type: str) -> Pipeline:
    """
    build a sickit-learn pipeline for the best vectorizer and model
    """
    vectorizer = TfidfVectorizer(**v_params)

    if model_type == "SVM":
        model = SVC(**m_params)
    elif model_type == "RF":
        model = RandomForestClassifier(**m_params)
    else:
        raise ValueError(f"unknown model type: {model_type}")

    pipeline = Pipeline([("tfidf", vectorizer), ("model", model)])

    return pipeline


def rebuild_best_model_bert(
    m_params: Dict, model_type: str
) -> Union[SVC, RandomForestClassifier]:
    """
    Builds the best model object (no pipeline) for BERT path.
    """
    if model_type == "SVM":
        model = SVC(**m_params)
    elif model_type == "RF":
        model = RandomForestClassifier(**m_params)
    else:
        raise ValueError(f"unknown model type: {model_type}")
    return model


def analyze_threshold(
    pipeline: Pipeline, X_corpus_preprocessed: pd.Series, y_labels: pd.Series
):
    """
    using the best model, optimize the threshold for f1 score
    """
    # out of fold
    try:
        s_oof = cross_val_predict(
            pipeline,
            X_corpus_preprocessed,
            y_labels,
            cv=5,
            method="predict_proba",
            n_jobs=-1,
        )[:, 1]
    except Exception as e:
        logging.error(f"failed to get oof predictions: {e}")
        return

    precisions, recalls, thresholds = precision_recall_curve(y_labels, s_oof)

    f1_scores = 2 * (precisions * recalls) / (precisions + recalls + 1e-9)  # Avoid 0/0
    f1_scores = np.nan_to_num(f1_scores)

    # thresholds is one element shorter
    valid_f1_scores = f1_scores[:-1]
    valid_thresholds = thresholds

    if len(valid_f1_scores) > 0:
        best_f1_idx = np.argmax(valid_f1_scores)
        optimal_threshold = valid_thresholds[best_f1_idx]
        max_f1 = valid_f1_scores[best_f1_idx]

        logging.info(f"max f1 score: {max_f1:.4f}")
        logging.info(f"optimal tau: {optimal_threshold:.4f}")

        plt.figure(figsize=(12, 8))
        plt.plot(
            valid_thresholds, valid_f1_scores, label="F1 Score", color="blue", lw=2
        )
        plt.plot(
            valid_thresholds, precisions[:-1], label="Precision", color="green", ls="--"
        )
        plt.plot(valid_thresholds, recalls[:-1], label="Recall", color="red", ls="--")

        plt.axvline(
            x=optimal_threshold,
            color="black",
            lw=2,
            ls=":",
            label=f"Optimal Threshold (tau* = {optimal_threshold:.2f})\nMax F1 = {max_f1:.2f}",
        )

        plt.title("F1, Precision, and Recall vs. Decision Threshold", fontsize=16)
        plt.xlabel("Decision Threshold (tau)", fontsize=12)
        plt.ylabel("Score", fontsize=12)
        plt.legend()
        plt.grid(True)
        plt.savefig("threshold_optimization_TFIDF.png")
        logging.info("Saved TF-IDF threshold plot to threshold_optimization_TFIDF.png")
        plt.close()
    else:
        logging.warning("Could not generate threshold plot: no valid F1 scores.")


def run_meta_analysis(df: pd.DataFrame, model_type: str, path_name: str = "TFIDF"):
    """
    trains a random forest on the optimization results
    to derive which parameter were the most important ones
    """
    logging.info(f"\nRunning meta-model analysis for {model_type} ({path_name})")

    Y = df["f1_mean"]

    # define prefixes based on path
    if path_name == "TFIDF":
        prefixes_to_check = ("p_", "v_", "m_")
    else:  # BERT
        # BERT results CSV does not have prefixes
        reserved_cols = [
            "exp_id",
            "f1_mean",
            "f1_std",
            "accuracy_mean",
            "precision_mean",
            "recall_mean",
            "status",
            "id",
        ]
        params_cols = [k for k in df.columns if k not in reserved_cols]

    if path_name == "TFIDF":
        params_cols = [
            col
            for col in df.columns
            if col.startswith(prefixes_to_check) and "id" not in col
        ]

    X = df[params_cols]

    if X.empty:
        logging.error(f"no parameter columns found for {model_type}")
        return

    categorical_cols = [col for col in X.columns if X[col].dtype == "object"]

    for col in X.columns:
        if col in categorical_cols:
            X[col] = X[col].fillna("None")
        else:
            X[col] = X[col].fillna(0)

    if categorical_cols:
        all_categories = [X[col].unique() for col in categorical_cols]
        encoder = OrdinalEncoder(
            categories=all_categories,
            handle_unknown="use_encoded_value",
            unknown_value=-1,
        )
        X[categorical_cols] = encoder.fit_transform(X[categorical_cols])

    logging.info(f"training meta-model on {len(X)} {model_type} experiments")
    meta_model = RandomForestRegressor(n_estimators=100, random_state=42, n_jobs=-1)
    meta_model.fit(X, Y)

    importances = meta_model.feature_importances_
    importance_series = pd.Series(importances, index=X.columns).sort_values(
        ascending=False
    )

    logging.info(f"Top 10 Important Features for {model_type} ({path_name}):")
    print(importance_series.head(10))

    logging.info("generating partial dependence plots")

    # use 6 most important features
    features_to_plot = importance_series.head(
        min(6, len(importance_series))
    ).index.tolist()
    if not features_to_plot:
        logging.warning("No features to plot for PDP.")
        return

    n_features = len(features_to_plot)
    ncols = min(3, n_features)
    nrows = int(np.ceil(n_features / ncols))
    if nrows == 0 or ncols == 0:
        logging.warning("No features to plot for PDP.")
        return

    fig, ax = plt.subplots(figsize=(ncols * 6, nrows * 5), nrows=nrows, ncols=ncols)

    try:
        display = PartialDependenceDisplay.from_estimator(
            meta_model,
            X,
            features_to_plot,
            categorical_features=categorical_cols,
            feature_names=X.columns,
            ax=ax,
            n_jobs=-1,
        )
        fig.suptitle(
            f"Partial Dependence Plots for {model_type} F1 score ({path_name})",
            fontsize=20,
        )
        plt.tight_layout(rect=[0, 0.03, 1, 0.95])
        plt.savefig(f"pdp_plot_{path_name}_{model_type}.png")
        logging.info(f"Saved PDP plot to pdp_plot_{path_name}_{model_type}.png")
        plt.close(fig)
    except Exception as e:
        logging.error(f"could not generate pdp: {e}")
        plt.close(fig)


def analyze_meta_model(results_df: pd.DataFrame, path_name: str = "TFIDF"):
    """
    splits optimization csv per model and fits the meta model
    """
    df_rf = results_df[results_df["exp_id"].str.contains("RF")].copy()
    df_svm = results_df[results_df["exp_id"].str.contains("SVM")].copy()

    if not df_rf.empty:
        run_meta_analysis(df_rf, "RandomForest", path_name)
    else:
        logging.warning("no randomforest experiments")

    if not df_svm.empty:
        run_meta_analysis(df_svm, "SVM", path_name)
    else:
        logging.warning("no SVM experiments")


# %% [markdown]
# # Hyperparameter optimization

# %%
if __name__ == "__main__":

    # --- MASTER CONTROL VARIABLES ---
    RUN_TFIDF_PATH = False  # run the full TF-IDF grid search
    RUN_BERT_PATH = True  # run the full BERT grid search
    RUN_ANALYSIS_PATH = True  # run the analysis block below

    # Set this to the path you want to analyze
    ANALYSIS_TARGET = "BERT"  # 'TFIDF' or 'BERT'

    SEARCH_MODE = "random"  # 'grid' or 'random'
    N_RANDOM_SAMPLES = 1000  # num samples if SEARCH_MODE == 'random'
    K_FOLDS = 5  # number of folds for all experiments
    # ---

    # load data
    # --- FIX: Use __file__.parent to get correct script path ---
    BASE_PATH = pathlib.Path(
        __file__
    ).parent.resolve()  # if using notebook remove __file__ and leave it blank pathlib.Path().parent.resolve()
    file_path = BASE_PATH / "data" / "dataset.csv"
    if not file_path.exists():
        file_path = pathlib.Path("dataset.csv")  # fallback
        if not file_path.exists():
            logging.error(
                f"Could not find 'dataset.csv' in {BASE_PATH / 'data'} or current directory."
            )
            exit()

    tmp_df = load_csv_to_dataframe(file_path)
    if tmp_df is None:
        logging.error("Dataframe could not be loaded. Exiting.")
        exit()

    corpus_df = tmp_df[tmp_df["isTraining"] == 0]  # leave validation set out

    # set data
    label_column_name = "isEnvironmental"
    y_labels_raw = corpus_df[label_column_name]
    valid_indices = y_labels_raw.dropna().index
    corpus_df_clean = corpus_df.loc[valid_indices]
    y_labels_clean = y_labels_raw.loc[valid_indices].astype(int)

    # set data for validation (used in TF-IDF analysis)
    validation_corpus_df = tmp_df[tmp_df["isTraining"] == 1]
    validation_y_labels_raw = validation_corpus_df[label_column_name]
    validation_valid_indices = validation_y_labels_raw.dropna().index
    validation_corpus_df_clean = validation_corpus_df.loc[validation_valid_indices]
    validation_y_labels_clean = validation_y_labels_raw.loc[
        validation_valid_indices
    ].astype(int)

    # %% [markdown]
    # # --- PATH 1: TF-IDF Grid Search ---
    # uses vectorization + optimized SVM/RF
    # %%

    if RUN_TFIDF_PATH:
        logging.info("--- STARTING PATH 1: TF-IDF GRID SEARCH ---")

        # define search space
        search_space = [
            ("SVM", train_kernel_svm_model, list(generate_svm_params())),
            ("RandomForest", train_rf_model, list(generate_rf_params())),
        ]

        h_preprocess = list(generate_preprocessing_params())
        h_vectorize = list(generate_vectorizer_params())

        all_tasks = []

        # compute the full cartesian product
        for model_name, train_func, h_model_list in search_space:
            h_product = itertools.product(h_preprocess, h_vectorize, h_model_list)
            for f_p, v_p, m_p in h_product:
                all_tasks.append(
                    delayed(run_experiment)(
                        corpus_df_clean, y_labels_clean, train_func, f_p, v_p, m_p
                    )
                )

        logging.warning(f"TF-IDF Path: {len(all_tasks)} total experiments to run")

        # handles random or grid search
        if SEARCH_MODE == "random":
            if N_RANDOM_SAMPLES >= len(all_tasks) or N_RANDOM_SAMPLES < 1:
                logging.warning(
                    f"N_RANDOM_SAMPLES ({N_RANDOM_SAMPLES}) is invalid. Running all tasks."
                )
                tasks_to_run = all_tasks
            else:
                logging.warning(
                    f"Running Random Search with n={N_RANDOM_SAMPLES} samples"
                )
                random.seed(42)
                tasks_to_run = random.sample(all_tasks, N_RANDOM_SAMPLES)
        elif SEARCH_MODE == "grid":
            tasks_to_run = all_tasks
        else:
            logging.error(f"Unknown SEARCH_MODE: {SEARCH_MODE}")
            tasks_to_run = []

        if len(tasks_to_run) > 0:
            os.environ["OMP_NUM_THREADS"] = "1"
            all_results = Parallel(n_jobs=-1, verbose=10)(tasks_to_run)
            logging.warning("TF-IDF grid search complete")

            # saving results to df
            results_df = create_results_dataframe(all_results)
            results_df.sort_values(by="f1_mean", ascending=False, inplace=True)
            output_path = BASE_PATH / f"grid_search_results_TFIDF_{SEARCH_MODE}.csv"
            results_df.to_csv(output_path, index=False, float_format="%.4f")
            logging.warning(f"TF-IDF results saved to: {output_path}")
        else:
            logging.warning("No TF-IDF tasks to run.")

    # %% [markdown]
    # # --- PATH 2: BERT Grid Search ---
    # generate features using BERT then use optimized SVM/RF
    # %%

    if RUN_BERT_PATH:
        logging.info("--- STARTING PATH 2: BERT GRID SEARCH ---")

        # 1. Pre-compute embeddings
        logging.info("Pre-computing BERT embeddings for training data...")
        raw_corpus = combine_text_fields(
            corpus_df_clean, strategy="name_plus_desc"
        )  # Use most info
        X_bert_embeddings = get_bert_embeddings(raw_corpus)
        y_full = y_labels_clean.values

        logging.info("Pre-computing BERT embeddings for validation data...")
        validation_raw_corpus = combine_text_fields(
            validation_corpus_df_clean, strategy="name_plus_desc"
        )
        validation_X_bert_embeddings = get_bert_embeddings(validation_raw_corpus)
        validation_y_full = validation_y_labels_clean.values

        # 2. Define search space (models only)
        search_space_bert = [
            ("SVM", train_kernel_svm_model, list(generate_svm_params())),
            ("RandomForest", train_rf_model, list(generate_rf_params())),
        ]

        all_model_params = []
        for model_name, train_func, h_model_list in search_space_bert:
            for m_p in h_model_list:
                m_p["model_name"] = model_name
                m_p["train_func"] = train_func
                all_model_params.append(m_p)

        logging.info(f"BERT Path: {len(all_model_params)} total model configs.")

        # 3. Handle Random vs Grid Search
        if SEARCH_MODE == "random":
            if N_RANDOM_SAMPLES >= len(all_model_params) or N_RANDOM_SAMPLES < 1:
                logging.warning(
                    f"N_RANDOM_SAMPLES ({N_RANDOM_SAMPLES}) is invalid. Running all models."
                )
                params_to_run = all_model_params
            else:
                logging.warning(
                    f"Running Random Search with n={N_RANDOM_SAMPLES} model samples"
                )
                random.seed(42)
                params_to_run = random.sample(all_model_params, N_RANDOM_SAMPLES)
        elif SEARCH_MODE == "grid":
            params_to_run = all_model_params
        else:
            logging.error(f"Unknown SEARCH_MODE: {SEARCH_MODE}")
            params_to_run = []

        # 4. Build task list
        all_tasks = []
        fold_generator = list(create_folds(X_bert_embeddings, y_full, k=K_FOLDS))

        for m_params in params_to_run:
            # Create a copy to avoid mutation during parallel execution
            params_copy = m_params.copy()
            train_func = params_copy.pop("train_func")
            params_copy.pop("model_name")

            for fold_id, (train_idx, test_idx) in enumerate(fold_generator):
                all_tasks.append(
                    delayed(run_experiment_bert)(
                        X_bert_embeddings,
                        y_full,
                        train_idx,
                        test_idx,
                        train_func,
                        params_copy,
                        fold_id,
                    )
                )

        logging.warning(
            f"BERT Path: {len(all_tasks)} total tasks to run (N_Params * K_Folds)"
        )

        # 5. Run tasks
        if len(all_tasks) > 0:
            os.environ["OMP_NUM_THREADS"] = "1"
            all_fold_results = Parallel(n_jobs=-1, verbose=10)(all_tasks)
            logging.warning("BERT grid search complete")

            # saving results to df
            results_df = create_results_dataframe_bert(all_fold_results)
            output_path = BASE_PATH / f"grid_search_results_BERT_{SEARCH_MODE}.csv"
            results_df.to_csv(output_path, index=False, float_format="%.4f")
            logging.warning(f"BERT results saved to: {output_path}")
        else:
            logging.warning("No BERT tasks to run.")

    # %% [markdown]
    ## Results Analysis
    # %%

    if RUN_ANALYSIS_PATH:
        logging.info(f"\n--- STARTING ANALYSIS PATH FOR: {ANALYSIS_TARGET} ---")

        results_file_name = f"grid_search_results_{ANALYSIS_TARGET}_{SEARCH_MODE}.csv"
        results_df = load_csv_to_dataframe(BASE_PATH / results_file_name)

        if results_df is None:
            logging.error(
                f"Could not load results file: {results_file_name}. Exiting analysis."
            )
            exit()

        results_df.dropna(subset=["f1_mean"], inplace=True)
        if results_df.empty:
            logging.error(
                "No successful experiments found in results file. Exiting analysis."
            )
            exit()

        best_run = results_df.sort_values(by="f1_mean", ascending=False).iloc[0]

        logging.info(f"\nBest {ANALYSIS_TARGET} model found")
        logging.info(f"experiment ID: {best_run['exp_id']}")
        logging.info(f"F1 Mean (at tau = 0.5): {best_run['f1_mean']:.4f}")

        model_type = "SVM" if "SVM" in best_run["exp_id"] else "RF"

        if ANALYSIS_TARGET == "TFIDF":
            logging.info("\nre-specifying best TF-IDF model")
            p_params, v_params, m_params = parse_best_params(best_run, model_type)

            # check parameters
            required_keys = [
                "combine_strategy",
                "to_lower",
                "remove_punct",
                "remove_stopwords",
                "normalization_strategy",
            ]
            missing_keys = [key for key in required_keys if key not in p_params]
            if missing_keys:
                logging.error(f"failed, missing keys: {missing_keys}")

            # Use VALIDATION data for threshold analysis
            X_corpus_preprocessed = create_preprocessed_corpus(
                validation_corpus_df_clean, **p_params
            )
            pipeline = rebuild_best_model(v_params, m_params, model_type)
            logging.info(f"rebuilt best model pipeline, see specs: {pipeline}")

            logging.info(f"\nThreshold analysis for TF-IDF")
            analyze_threshold(
                pipeline, X_corpus_preprocessed, validation_y_labels_clean
            )

            logging.info(f"\nMeta-model hyperparameter analysis for TF-IDF")
            analyze_meta_model(results_df, "TFIDF")

        elif ANALYSIS_TARGET == "BERT":
            logging.info("\nre-specifying best BERT model")

            # pre-computed embeddings for the validation set
            if "validation_X_bert_embeddings" not in locals():
                logging.info("Pre-computing BERT embeddings for validation data...")
                validation_raw_corpus = combine_text_fields(
                    validation_corpus_df_clean, strategy="name_plus_desc"
                )
                validation_X_bert_embeddings = get_bert_embeddings(
                    validation_raw_corpus
                )
                validation_y_full = validation_y_labels_clean.values

            # parse only model params
            m_params = parse_best_params_bert(best_run, model_type)

            model = rebuild_best_model_bert(m_params, model_type)
            logging.info(f"rebuilt best model, see specs: {model}")

            logging.info(f"\nThreshold analysis for BERT")
            # run analysis on the validation embeddings
            analyze_threshold_bert(
                model, validation_X_bert_embeddings, validation_y_full
            )

            logging.info(f"\nMeta-model hyperparameter analysis for BERT")
            analyze_meta_model(results_df, "BERT")

    logging.info("--- Main script execution finished ---")
