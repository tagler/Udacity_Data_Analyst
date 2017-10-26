import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
import sys
import pickle

sys.path.append("./tools/")

# Task 1: Inport Data -------------------------------------------------------

# features_list is a list of strings, each of which is a feature name
features_list = ['poi', 
    'bonus', 'deferral_payments', 'deferred_income', 'director_fees',
    'exercised_stock_options', 'expenses', 'from_messages',
    'from_poi_to_this_person', 'from_this_person_to_poi', 'loan_advances',
    'long_term_incentive', 'other', 'restricted_stock',
    'restricted_stock_deferred', 'salary', 'shared_receipt_with_poi',
    'to_messages', 'total_payments', 'total_stock_value' ]
feature_names = features_list[1:]

# load the dictionary containing the dataset
with open("final_project_dataset.pkl", "r") as data_file:
    data_dict = pickle.load(data_file)

# allocation of labels     
labels_poi = []
for x in data_dict:
    labels_poi.append( data_dict[x]['poi'] )
print labels_poi.count(0)
print labels_poi.count(1)

# missing values counts
values = data_dict.values()[0].keys()
values_dict = {}
for x in values:
    values_dict[x] = 0
for x in range( 0,len( data_dict.values() ) ):
    for y in values:
        if data_dict.values()[x][y] == 'NaN':
            values_dict[y] = values_dict[y] + 1
print values_dict
for key, value in sorted(values_dict.iteritems(), key=lambda (k,v): (v,k)):
    print key, value, round( float(value)/len( data_dict.values() ), 2 )

# Task 2: Remove Outliers ---------------------------------------------------

# remove outlier entries 
data_dict_out = data_dict.copy()
del data_dict_out['THE TRAVEL AGENCY IN THE PARK']
del data_dict_out['TOTAL']
del data_dict_out['LOCKHART EUGENE E']

# Task 3-1: Create New Features ---------------------------------------------

# create features for percent of to/from messages 
data_dict_new = data_dict_out.copy()
for each in data_dict_new:
    from_poi = float( data_dict_new[each]['from_poi_to_this_person'] )
    to_poi = float( data_dict_new[each]['from_this_person_to_poi'] ) 
    from_messages = float( data_dict_new[each]['from_messages'] )
    to_messages = float( data_dict_new[each]['to_messages'] )
    # add new features 
    data_dict_new[each]['from_poi_percent'] = from_poi / to_messages
    data_dict_new[each]['to_poi_percent'] = to_poi / from_messages
    # fix nan values that will cause errors, convert to string 'NaN'
    if math.isnan( data_dict_new[each]['from_poi_percent'] ):
        data_dict_new[each]['from_poi_percent'] = 'NaN'
    if math.isnan( data_dict_new[each]['to_poi_percent'] ):
        data_dict_new[each]['to_poi_percent'] = 'NaN'
        
# add new features to features_list
features_list.append('from_poi_percent')
features_list.append('to_poi_percent')
feature_names = features_list[1:]

# store to my_dataset for export 
my_dataset = data_dict_new

# Task 3-2: Feature Selection -----------------------------------------------

# extract all features and labels from dataset
from feature_format import featureFormat, targetFeatureSplit
data = featureFormat(my_dataset, features_list, sort_keys = True)
labels, features = targetFeatureSplit(data)

# preprocess all features 
from sklearn import preprocessing
scale = preprocessing.MinMaxScaler()
features = scale.fit_transform(features)

# kbest feature selection
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import chi2
featureSelector = SelectKBest(chi2, k='all')
featureSelector.fit(features, labels)
featureSelector.pvalues_
kbest_features = []
for x in range(0,len(feature_names)):
    if featureSelector.pvalues_[x] < 0.15:
        kbest_features.append([feature_names[x],
                    round(featureSelector.pvalues_[x],2)])
print kbest_features        
        
# decision tree variable importance 
from sklearn import tree
clf = tree.DecisionTreeClassifier()
clf.fit(features, labels)
clf.feature_importances_
decision_best = []
for x in range(0,len(feature_names)):
    if clf.feature_importances_[x] > 0:
        decision_best.append([feature_names[x],
                    round(clf.feature_importances_[x],2)])
print decision_best       

# Lasso regularization feature selection 
from sklearn.linear_model import RandomizedLasso
rlasso = RandomizedLasso(alpha=0.0085)
rlasso.fit(features, labels)
lasso_best = []
for x in range(0,len(feature_names)):
    if rlasso.scores_[x] > 0:
        lasso_best.append([feature_names[x],
                            round(rlasso.scores_[x],2)])
print lasso_best

# top features selected
features_list = ['poi', 
        'bonus', 'deferred_income', 'exercised_stock_options',
        'expenses', 'loan_advances', 'long_term_incentive',
        'other', 'restricted_stock', 'salary',
        'shared_receipt_with_poi', 'total_payments', 
        'total_stock_value', 'to_poi_percent'] 
feature_names = features_list[1:]

# extract only selected features and labels from dataset
from feature_format import featureFormat, targetFeatureSplit
data = featureFormat(my_dataset, features_list, sort_keys = True)
labels, features = targetFeatureSplit(data)

# preprocess selected features 
from sklearn import preprocessing
scale = preprocessing.MinMaxScaler()
features = scale.fit_transform(features)

# split data into training and testing sets
from sklearn.cross_validation import train_test_split
features_train, features_test, labels_train, labels_test = \
    train_test_split(features, labels, test_size=0.5, random_state=42)

# Task 4: Test Classifiers --------------------------------------------------

# logistic regression
# Accuracy: 0.80073	Precision: 0.20791	Recall: 0.17600	
# F1: 0.19063	F2: 0.18157
from sklearn.linear_model import LogisticRegression
clf = LogisticRegression()

# naive-bayes 
# Accuracy: 0.81293	Precision: 0.30303	Recall: 0.31000	
# F1: 0.30648	F2: 0.30858
from sklearn.naive_bayes import GaussianNB
clf = GaussianNB()

# k-means clustering 
# Accuracy: 0.83433	Precision: 0.22783	Recall: 0.10150	
# F1: 0.14044	F2: 0.11416
from sklearn.cluster import KMeans
clf = KMeans(n_clusters=2)

# k-nearest neighbors 
# Accuracy: 0.86147	Precision: 0.38596	Recall: 0.06600	
# F1: 0.11272	F2: 0.07912
from sklearn.neighbors import KNeighborsClassifier
clf = KNeighborsClassifier(n_neighbors=2)

# random forest
# Accuracy: 0.86140	Precision: 0.43577	Recall: 0.13400	
# F1: 0.20497	F2: 0.15554
from sklearn.ensemble import RandomForestClassifier 
clf = RandomForestClassifier()

# extra tree
# Accuracy: 0.85867	Precision: 0.41254	Recall: 0.14150	
# F1: 0.21072	F2: 0.16291
from sklearn.ensemble import ExtraTreesClassifier
clf = ExtraTreesClassifier()

# gradient-boosting 
# Accuracy: 0.85947	Precision: 0.45408	Recall: 0.26700	
# F1: 0.33627	F2: 0.29098
from sklearn import ensemble
clf = ensemble.GradientBoostingClassifier()

# decision tree
# Accuracy: 0.82407	Precision: 0.33823	Recall: 0.33400	
# F1: 0.33610	F2: 0.33484
from sklearn import tree
clf = tree.DecisionTreeClassifier()

# ada-boosting 
# Accuracy: 0.84687	Precision: 0.40782	Recall: 0.32850	
# F1: 0.36389	F2: 0.34180
from sklearn.ensemble import AdaBoostClassifier
clf = AdaBoostClassifier()

# Task 5-1: Tune Classifier -------------------------------------------------

# cross-validation of training data
from sklearn.cross_validation import ShuffleSplit
cv = ShuffleSplit(features_train.shape[0], 
                    n_iter=5, test_size=0.2, random_state=42)

# tune decision tree classifier via grid search cv
from sklearn import tree
clf = tree.DecisionTreeClassifier()
from sklearn.grid_search import GridSearchCV
from sklearn.metrics import f1_score
param_grid = { 'criterion': ['gini','entropy'],
               'max_features': [None, 'auto', 'sqrt', 'log2'],
               'splitter' : ['best','random'],
               'max_depth' : [None,1,2,4,8,10,15],
               'min_samples_split' : [1,2,3,5,10],
               'min_samples_leaf' : [1,2,3],
               'class_weight' : [None, 'balanced']}
cv_clf = GridSearchCV(estimator=clf, 
                      cv=cv, 
                      param_grid=param_grid,
                      scoring='f1')
cv_clf.fit(features_train, labels_train)
print cv_clf.best_params_

# tune decision tree classifier via grid search cv
# Accuracy: 0.81427	Precision: 0.33827	Recall: 0.41100	
# F1: 0.37111	F2: 0.39406
clf = tree.DecisionTreeClassifier(splitter = 'best', 
            min_samples_leaf = 2,
            min_samples_split = 2, 
            criterion = 'entropy', 
            max_features = 'log2', 
            max_depth = 8, 
            class_weight = 'balanced')

# tune ada-boosting classifier via grid search cv
from sklearn.ensemble import AdaBoostClassifier
clf = AdaBoostClassifier()
from sklearn.grid_search import GridSearchCV
from sklearn.metrics import f1_score
param_grid = { 'n_estimators': [50,100,200],
               'learning_rate': [0.7,0.9,1,1.1],
               'algorithm' : ['SAMME', 'SAMME.R']}
cv_clf = GridSearchCV(estimator=clf, 
                      cv=cv, 
                      param_grid=param_grid,
                      scoring = 'f1')
cv_clf.fit(features_train, labels_train)
print cv_clf.best_params_

# tuned ada-boosting model via grid search cv
# 1st run: Accuracy: 0.85167  Precision: 0.42866  Recall: 0.33800	
# F1: 0.37797	F2: 0.35293
# 2nd run: Accuracy: 0.85153  Precision: 0.42803  Recall: 0.33750	
# F1: 0.37741	F2: 0.35241
clf = AdaBoostClassifier(n_estimators = 100, 
                         learning_rate = 0.9,
                         algorithm = 'SAMME.R')

# Task 5-2: Evaluation Metrics ----------------------------------------------

# predictions
clf.fit(features_train, labels_train)
test_predictions = clf.predict(features_test)

# feature importance 
clf.feature_importances_
ada_best = []
for x in range(0,len(feature_names)):
    if clf.feature_importances_[x] > 0:
        ada_best.append([feature_names[x],
                    round(clf.feature_importances_[x],2)])
print ada_best      

# confusion matrix and errors
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_auc_score
from sklearn.metrics import classification_report
print confusion_matrix(labels_test, test_predictions) 
print classification_report(labels_test, test_predictions)
print roc_auc_score(labels_test, test_predictions)

# Task 6: Dump Classifier, Dataset, and Features --------------------------

from tester import dump_classifier_and_data
dump_classifier_and_data(clf, my_dataset, features_list)
