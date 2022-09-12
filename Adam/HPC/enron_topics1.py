from gensim import models,corpora
import pandas as pd
from ast import literal_eval
df = pd.read_csv('../data/formatted_content_enron.csv').dropna().reset_index(drop=True)
model = models.ldamodel.LdaModel.load('../model/cmu_lda.model')
dictionary = corpora.Dictionary.load('../model/cmu_lda.model.id2word')
topic = list()
df['fcontent'] = df['fcontent'].apply(literal_eval)
for email in df['fcontent']:
    if len(email) == 0:
        topic.append('none')
    else:
        topic.append(model.get_document_topics(dictionary.doc2bow(email)))
df['topic'] = topic
df.to_csv('../data/topic_enron.csv')
