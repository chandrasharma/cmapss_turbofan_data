# cmapss_turbofan_data
-Goal is to predict remaining useful life (RUL) of each engine after every flight. 
-After every flight the predicted RUL is compared with standard maintenance manual and a call to service the engine is made
-The data is interspersed with noise. This is to capture the effect of service on the next flight the engine makes

Approach
-A simple linear regression model is built taking frist 100 rows of train data as input. RUL is available only for the 
 first 100 rows in training set.
-Using this model the remaining rows of RUL in the train set is predicted
-The resultant file is used as the actual train data
-Variables are manually selected (see code), Rest are dropped
-PCA is used to reduce dimensions from 9 to 3 (test data has 27 dimensions)
