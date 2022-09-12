import gensim
import pandas as pd
from ast import literal_eval
from gensim.models.coherencemodel import CoherenceModel
from sklearn.model_selection import KFold
import statistics
from sklearn.metrics import jaccard_score
from scipy.optimize import linear_sum_assignment
import numpy as np

df = pd.read_csv("../data/formatted_emails_cmu.csv")
df['content'] = df['content'].apply(literal_eval)
vocab = gensim.corpora.Dictionary(df['content'])
train = [vocab.doc2bow(doc) for doc in df['content']]
prep =[]
coh=[]
for i in range(1,8):
    print(i)
    LDA = gensim.models.ldamodel.LdaModel(corpus=train, id2word=vocab, passes = i, num_topics=5)
    perp.append(LDA.log_perplexity())
    coh.append(CoherenceModel(model=LDA, corpus=train, dictionary=vocab,coherence='c_v').get_coherence())

with open('../data/perp.pickle','wb') as f:
    pickle.dump(perp,f)
with open('../data/coh.pickle','wb') as f:
    pickle.dump(coh,f)
