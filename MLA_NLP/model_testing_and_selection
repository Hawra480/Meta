import sys
sys.path.append('/Users/hawraal-ghafli/anaconda3/lib/python3.7/site-packages')

from sklearn.naive_bayes import MultinomialNB
import numpy as np
import pylab
import pandas as pd
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import TfidfVectorizer, TfidfTransformer, HashingVectorizer, CountVectorizer
from sklearn import metrics
from pandas import read_csv
from pandas.plotting import scatter_matrix
from matplotlib import pyplot
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC
import sklearn
from sklearn import linear_model
from sklearn.feature_extraction import text

vectorizer = TfidfVectorizer()


mydata = pd.read_csv("/Users/hawraal-ghafli/Desktop/sys.review/working codes/ML_screening1.csv")
mydata['ta'] = ('' if mydata['Abstracts'].empty else mydata['Abstracts'].map(str)) +('' if mydata['Titles'].empty else mydata['Titles'].map(str))
mydata.pop('Titles')
mydata.pop('Abstracts')
#Split-out validation dataset
predict= mydata['Deci']
X= mydata['ta']
Y= mydata['Deci']

X_train, X_test, Y_train, Y_test = sklearn.model_selection.train_test_split(X, Y, test_size=0.8)
# Define training_vectors:
vectorizer = TfidfVectorizer()
X_training_vectors= vectorizer.fit_transform(X_train)
X_test_vectors=vectorizer.transform(X_test)

models = []
models.append(('LR', LogisticRegression(solver='liblinear', multi_class='ovr')))
models.append(('LDA', LinearDiscriminantAnalysis()))
models.append(('KNN', KNeighborsClassifier()))
models.append(('CART', DecisionTreeClassifier()))
models.append(('NB', GaussianNB()))
models.append(('SVM', SVC(gamma='auto')))
# evaluate each model in turn
results = []
names = []
for name, model in models:
	kfold = StratifiedKFold(n_splits=10, random_state=1)
	cv_results = cross_val_score(model, X_training_vectors, Y_train, cv=kfold, scoring='accuracy')
	results.append(cv_results)
	names.append(name)
	print('%s: %f (%f)' % (name, cv_results.mean(), cv_results.std()))
#Compare Algorithms
pyplot.boxplot(results, labels=names)
pyplot.title('Algorithm Comparison')
pyplot.show()
#KNN gave an accuracy of 0.82 out of 1
#LR accuracy =0.80
#SVM accuracy =0.82
#LDA:nan
#SVM:nan
#CART accuracy =0.78
# graph uploaded as Algorthim comparsion
#LR, KNN, SVM have the highst accuracy so far. will choose one of them to train the algorthim
