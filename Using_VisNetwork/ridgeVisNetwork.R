if (dir.exists("~/Dropbox/ridgeEvaluation/")) {
  setwd("~/Dropbox/ridgeEvaluation/")
} else if (dir.exists("~/ridgeEvaluationCode/")) {
  setwd("~/ridgeEvaluationCode/")
} else if (dir.exists("~/ridgeEvaluation/")) {
  setwd("~/ridgeEvaluation/")
} else if (dir.exists("/accounts/projects/sekhon/theo_s/gdrive/ridgeEvaluation")) {
  setwd("/accounts/projects/sekhon/theo_s/gdrive/ridgeEvaluation")
} else {
  stop("wd was not set correctly")
}

library(visNetwork)
library(forestry)
library(rpart)
library(shiny)

source("Using_VisNetwork/ridgeVisNetwork_soe.R")

# Simple Iris example with ridgeRF + rpart
y <- iris[,1]
x <- iris[,c(-1)]

rpart_tree <- rpart(y~., 
                    data = x)
set.seed(89)
forestry_tree <- forestry(x = x,
                          y = y,
                          nodesizeStrictSpl = 15,
                          ntree = 1, 
                          ridgeRF = TRUE)

visualizeRidge(forestry_tree = forestry_tree)

set.seed(55)
short_tree <- forestry(x = x,
                       y = y,
                       nodesizeStrictSpl = 35,
                       ntree = 1, 
                       ridgeRF = TRUE)


visualizeRidge(forestry_tree = short_tree)

deep_tree <- forestry(x = x,
                      y = y,
                      nodesizeStrictSpl = 5,
                      ntree = 1, 
                      ridgeRF = TRUE)

visualizeRidge(forestry_tree = deep_tree)



datasets_grid <- list()

set.seed(776291)
n_train <- 5000
n_test <- 5000
n <- n_train + n_test

x <- matrix(runif(5 * n), nrow = n, ncol = 5)
x <- as.data.frame(x)
y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5] + rnorm(n, sd = 10)
semilinear_1 <- cbind(x, y)

datasets_grid[["artificial-semilinear_1"]] <- list(
  "train" = semilinear_1[1:n_train, ], 
  "test" = semilinear_1[(n_train + 1):(n_train + n_test), ])

sampsize <- 4096
dataset_i <- 1

data_train <- datasets_grid[[dataset_i]][["train"]][1:sampsize,]
data_test <- datasets_grid[[dataset_i]][["test"]]

Xtrain <- data_train[, -ncol(data_train)]
Xtest <- data_test[, -ncol(data_test)]

Ytrain <- data_train[, ncol(data_train)]
Ytest <- data_test[, ncol(data_test)]

ridge_rf <- forestry(x = Xtrain,
                     y = Ytrain,
                     nodesizeStrictSpl = 377,
                     mtry = 4,
                     ntree = 1, 
                     ridgeRF = TRUE,
                     overfitPenalty = .577)

forestry <- forestry(x = Xtrain,
                     y = Ytrain,
                     nodesizeStrictSpl = 10,
                     ntree = 1, 
                     ridgeRF = FALSE)

ridge_pred <- predict(ridge_rf, Xtest)
rf_pred <- predict(forestry, Xtest)
  
ridge_mse <- mean((ridge_pred - Ytest) ^ 2)
rf_mse <- mean((rf_pred - Ytest) ^ 2)

visualizeRidge(ridge_rf)

visTree(rpart_tree, main = "Iris classification Tree", width = "100%")



