import pandas as pd
from sklearn.metrics import r2_score 
from sklearn.svm import SVR 
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split 

# Regressor
dataset = pd.read_csv('../intermediate_data/ssp1.csv')

y = dataset.pop('humanitarian_needs').values.astype(float)
dataset.pop('humanitarian')

X = dataset.values.astype(float)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33)

param_grid = {
    'C': [0.1, 1, 10],
    'kernel': ['poly', 'rbf', 'sigmoid']
}

grid = GridSearchCV(SVR(), param_grid, refit = True, verbose = 3, n_jobs = -1)

# fitting the model for grid search
grid.fit(X_train, y_train)

# print best parameter after tuning
print(grid.best_params_)
grid_predictions = grid.predict(X_test)

# print score
print(r2_score(y_test, grid_predictions))

# {'C': 10, 'kernel': 'sigmoid'}
# -0.04306700031386668
