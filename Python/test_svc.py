import pandas as pd
from sklearn.metrics import classification_report 
from sklearn.svm import SVC 
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split 

# Binary classification
dataset = pd.read_csv('../intermediate_data/ssp1.csv')

y = dataset.pop('humanitarian').astype('str').astype('category').values
dataset.pop('humanitarian_needs')

X = dataset.values.astype(float)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33)

# defining parameter range
param_grid = {
    'C': [0.1, 1, 10, 100],
    'gamma': ['scale', 'auto'],
    'kernel': ['linear']
}

grid = GridSearchCV(SVC(), param_grid, refit = True, verbose = 3, n_jobs = -1)

# fitting the model for grid search
grid.fit(X_train, y_train)

# print best parameter after tuning
print(grid.best_params_)
grid_predictions = grid.predict(X_test)

# print classification report
print(classification_report(y_test, grid_predictions))

# {'C': 100, 'gamma': 'scale', 'kernel': 'linear'}
#               precision    recall  f1-score   support

#            0       0.85      0.99      0.91       661
#            1       0.85      0.22      0.34       153

#     accuracy                           0.85       814
#    macro avg       0.85      0.60      0.63       814
# weighted avg       0.85      0.85      0.81       814

