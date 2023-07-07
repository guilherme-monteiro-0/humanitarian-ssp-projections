import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn import svm

# Binary classification
dataset = pd.read_csv('../intermediate_data/ssp1.csv')

y = dataset.pop('humanitarian').astype('str').astype('category').values
dataset.pop('humanitarian_needs')

x = dataset.values.astype(float)

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.33)

# docs: https://scikit-learn.org/stable/modules/generated/sklearn.svm.LinearSVC.html
clf = svm.LinearSVC(class_weight='balanced', verbose=False, max_iter=10000, tol=1e-6, C=0.1)
clf.fit(x_train, y_train) # train

print("Binary classification: ", clf.score(x_test, y_test))

# Regressor
dataset = pd.read_csv('../intermediate_data/ssp1.csv')

y = dataset.pop('humanitarian_needs').values.astype(float)
dataset.pop('humanitarian')

x = dataset.values.astype(float)

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.33)

clf = svm.LinearSVC()
clf.fit(x_train, y_train) # train

print("Regressor: ", clf.score(x_test, y_test))