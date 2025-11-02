# %% [markdown]
## Libs

# %%
# load all libs
import os
import pandas as pd
import nltk
import logging
import string
from typing import List, Literal, Optional

# %%
# global configs
logging.basicConfig(
	level = logging.INFO,
	format = '%(asctime)s - %(levelname)s - %(message)s'
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
		nltk.data.find('tokenizers/punkt')
	except LookupError:
		logging.info("punkt...")
		nltk.download('punkt', quiet = True)
	try:
		nltk.data.find('corpora/stopwords')
	except LookupError:
		logging.info('stopwords...')
		nltk.download('stopwords', quiet = True)
	try:
		nltk.data.find('corpora/wordnet')
	except LookupError:
		logging.info('lemmatizer...')
		nltk.download('wordnet', quiet = True)
	try:
		nltk.data.find('corpora/omw-1.4')
	except LookupError:
		logging.info('omw...')
		nltk.download('omw-1.4', quiet = True)
	logging.info("nltk stuff done")


# cleaning functions objects instatiation
STOP_WORDS = set(nltk.corpus.stopwords.words('english'))
STEMMER = nltk.stem.PorterStemmer()
LEMMATIZER = nltk.stem.WordNetLemmatizer()
PUNCT_TABLE = str.maketrans('', '', string.punctuation)

def _clean_document(
	doc: str,
	to_lower: bool,
	remove_punct: bool,
	remove_stopwords: bool,
	normalization: Literal['none', 'stem', 'lemmatize']
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
	if remove_stopwords or normalization != 'none':
		tokens = nltk.word_tokenize(doc)
	else:
		return doc.strip()

	# stopword removal
	if remove_stopwords:
		tokens = [t for t in tokens if t not in STOP_WORDS]


	# normalization
	if normalization == 'stem':
		tokens = [STEMMER.stem(t) for t in tokens]
	elif normalization == 'lemmatize':
		tokens = [LEMMATIZER.lemmatize(t) for t in tokens]


	# re-join everything into single string doc
	return " ".join(tokens)


def combine_text_fields(
	df: pd.DataFrame,
	strategy: Literal['desc_only', 'name_plus_desc']
	) -> pd.Series:
	
	desc = df['Project description'].fillna('')

	if strategy == 'desc_only':
		return desc
	elif strategy == 'name_plus_desc':
		name = df['Name'].fillna('')
		return name + " " + desc
	else:
		raise ValueError(f"non valid strategy {strategy}")


def create_preprocessed_corpus(
	df: pd.DataFrame,
	combine_strategy: Literal['desc_only', 'name_plus_desc'],
	to_lower: bool,
	remove_punct: bool,
	remove_stopwords: bool,
	normalization: Literal['none', 'stem', 'lemmatize']
	) -> pd.Series:
	"""
	takes raw dataframe and relevant hyperparams and returns
	the preprocessed corpus
	"""

	combined_series = combine_text_fields(df, strategy = combine_strategy)

	cleaned_series = combined_series.map(
		lambda doc: _clean_document(
			doc,
			to_lower = to_lower,
			remove_punct = remove_punct,
			remove_stopwords = remove_stopwords,
			normalization = normalization
			)
		)

	return cleaned_series



# %% [markdown]
# # Main code execution

# %%
if __name__ == "__main__":
	download_nltk_resources()

	file_path = "./data/dataset.csv"
	corpus_df = load_csv_to_dataframe(file_path)

	if corpus_df is not None:
		print("--- HEAD ---")
		print(corpus_df[['Name', 'Project description']].head(2))
		print("--- END HEAD ---")

		f1_params = {
			"combine_strategy": "desc_only",
			"to_lower": True,
			"remove_punct": True,
			"remove_stopwords": True,
			"normalization": 'none'
		}

		print("strat 1...")
		preprocessed_corpus_1 = create_preprocessed_corpus(corpus_df, **f1_params)
		print(preprocessed_corpus_1.head(2))
		print("--- end strat 1 ---")

		f2_params = {
			"combine_strategy": "name_plus_desc",
			"to_lower": True,
			"remove_punct": True,
			"remove_stopwords": True,
			"normalization": 'stem'
		}

		print("strat 2...")
		preprocessed_corpus_2 = create_preprocessed_corpus(corpus_df, **f2_params)
		print(preprocessed_corpus_2.head(2))
		print("--- end strat 2 ---")