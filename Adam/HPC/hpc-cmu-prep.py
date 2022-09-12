import pandas as pd
import requests
import gensim
import string
import nltk
from gensim.utils import simple_preprocess
from gensim.parsing.preprocessing import STOPWORDS
from nltk.corpus import wordnet as wn
from nltk import word_tokenize, pos_tag
from collections import defaultdict
from nltk.stem import WordNetLemmatizer, SnowballStemmer
from nltk.stem.porter import *
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

nltk.download('popular')
nltk.download('stopwords')
nltk.download('punkt')
nltk.download('wordnet')
nltk.download('averaged_perceptron_tagger')
nltk.download('omw-1.4')

def remove_stop_words(text):
    words = str(text).split(' ')
    text1 = " ".join([i for i in words if i not in stop_words])
    return text1
    
def clean_entry(text): 
    delete_dict = {sp_character: '' for sp_character in string.punctuation + string.digits}
    delete_dict[' '] = ' ' 
    table = str.maketrans(delete_dict)
    text = text.replace('\n','')
    text = text.replace('\t','')
    text1 = text.translate(table)
    return text1
    
def remove_all_stop_words(text):
    words = text.split(' ')
    text1 = " ".join([i for i in words if i not in all_stop_words])
    return text1

pd.read_csv("../data/email.csv")
desc = df['content'].str.lower()
stop_words = stopwords.words('english')
desc1 = desc.apply(remove_stop_words)
desc1 = desc1.apply(clean_entry)
all_stop_words = gensim.parsing.preprocessing.STOPWORDS
desc2 = desc1.apply(remove_all_stop_words)
tag_map = defaultdict(lambda : wn.NOUN)
tag_map['J'] = wn.ADJ
tag_map['V'] = wn.VERB
tag_map['R'] = wn.ADV
desc3 = []
for line in desc2:
    tokens = word_tokenize(line)
    lemma_function = WordNetLemmatizer()
    desc3.append([lemma_function.lemmatize(token, tag_map[tag[0]]) for token, tag in pos_tag(tokens)]) 
df['content'] = desc3
df.to_csv('../data/formatted_emails_cmu.csv')








