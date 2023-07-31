import pandas as pd
from sklearn.model_selection import train_test_split
from crepes import WrapClassifier, WrapRegressor
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor, HistGradientBoostingClassifier, HistGradientBoostingRegressor
from sklearn.preprocessing import LabelEncoder


# Regressor
dataset = pd.read_csv('../intermediate_data/iiasa_unhcr_displaced2.csv')

y = dataset.pop('displaced_persons').values.astype(float)
y_mean, y_std = y.mean(0), y.std(0)
y = (y - y_mean) / y_std

X = dataset

small_uniques = [uniques < 20 for uniques in dataset.apply(pd.Series.nunique).to_list()]
object_dtypes = [dtype == 'O' for dtype in X.dtypes.tolist()]
categorical_indicator = [a or b for a, b in zip(small_uniques, object_dtypes)]
categorical_columns = X.columns[categorical_indicator].tolist()
for i in range(0, len(categorical_indicator)):
    print(X.columns[i], ": Categorical" if categorical_indicator[i] else ": Numerical")

for categorical_column in categorical_columns:
    X[categorical_column] = X[categorical_column].astype('str')

for col in categorical_columns:
    X[col] = X[col].fillna("MissingValue")
    l_enc = LabelEncoder() 
    X[col] = l_enc.fit_transform(X[col].values)

X = X.values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

X_prop_train, X_cal, y_prop_train, y_cal = train_test_split(X_train, y_train, test_size=0.25)

rf = WrapRegressor(RandomForestRegressor())
rf.fit(X_prop_train, y_prop_train)

rf.calibrate(X_cal, y_cal)

confidence_intervals = rf.predict_int(X_test, confidence=0.95) # Optional y_min to truncate at 0
eval = rf.evaluate(X_test, y_test, confidence=0.05)

print("Regressor rf: ", eval)

# Gradient Boosting
rf = WrapRegressor(HistGradientBoostingRegressor(validation_fraction=None, max_iter=1500, max_depth=None, learning_rate=0.01))
rf.fit(X_prop_train, y_prop_train)

rf.calibrate(X_cal, y_cal)

confidence_intervals = rf.predict_int(X_test, confidence=0.95) # Optional y_min to truncate at 0
eval = rf.evaluate(X_test, y_test, confidence=0.05)

print("Regressor gb: ", eval)