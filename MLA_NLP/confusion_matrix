# confusion mattrix for MLA classificattion compared to final studies included after full text-screeingin. 
import sys
sys.path.append('/Users/hawraal-ghafli/opt/anaconda3/pkgs/python-3.9.4-h88f2d9e_0/lib/python3.9/site-packages')

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


mydata = pd.read_csv('/Users/hawraal-ghafli/Desktop/All/sys.review/working codes/ML_screening1.csv')
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

clf = MultinomialNB().fit(X_training_vectors, Y_train)
linear= linear_model.LogisticRegression()
linear.fit(X_training_vectors, Y_train)
acc=linear.score(X_test_vectors, Y_test)
print (acc)
# accuracy of this training model is ~80% (0.8 out of 1)
#there is a bit of fluctuation every time I run the training code, which is normal

predictions=linear.predict(X_test_vectors)

mypred = pd.read_csv('/Users/hawraal-ghafli/Desktop/All/sys.review/working codes/ML_predicted_screeningM1.csv')
X_test= mypred['final']
Y_test= mypred['prediction']
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
 
cm = confusion_matrix(X_test,Y_test)
cm   
cmd = ConfusionMatrixDisplay(cm)
cmd.plot()
import matplotlib.pyplot as plt
plt.savefig('plot.png', dpi=300, bbox_inches='tight')
cmd.plot()
