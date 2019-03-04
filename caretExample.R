# Example to use Caret tuned version of ridge RF
set.seed(275)

test_idx <- 1:75
xTrain <- iris[test_idx, c(1,2,3)]
yTrain <- iris[test_idx, 4]

xTest <- iris[-(test_idx), c(1,2,3)]
yTest <- iris[-(test_idx), 4]



# Generate estimators using pipeline code
source("replicationCode/2-generateEstimators.R")

caret_ridge <- estimator_grid[["caretRidgeRF"]]


# Parameters to adjust number of randomly sampled points and number of folds for CV
# CV will be repeated 10 times adaptively with a minimum of 5 times

tuned_ridge <- caret_ridge(Xobs = xTrain,
                           Yobs = yTrain,
                           tune_length = 10,
                           cv_fold = 8)

# See hyperparameters
tuned_ridge

y_pred <- predict(tuned_ridge, xTest)
