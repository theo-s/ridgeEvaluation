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

# install most up to date version of forestry
devtools::install_github("soerenkuenzel/forestry", ref = "coefficientAggregation")

library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)

set.seed(5387479) 
source("replicationCode/1.8-generateDataBrieman.R")

names <- names(datasets_grid)
for (niceness in c(.005,.01, .1, 1,10)) {
  print(paste0("Niceness: ", niceness))
  for (fold_i in 1:length(datasets_grid)) {
    data_name <- names[fold_i]
    dataset <- datasets_grid[[data_name]]
    
    train_x <- dataset$train[,-ncol(dataset$train)]
    train_y <- dataset$train[,ncol(dataset$train)]
    
    test_x <- dataset$test[,-ncol(dataset$test)]
    test_y <- dataset$test[,ncol(dataset$test)]
    
    rf <- forestry(x = train_x, y = train_y, mtry = 1, nodesizeSpl = 8)
    rf_pred <- predict(rf, test_x)
    
    rf_vi <- getVI(rf)

    
    rf_vi[rf_vi < 0] <- 0
    print(rf_vi)
    
    weights <- log(1 + 100*rf_vi + niceness)
    
    print(weights)
    
    rf_aggressive <- forestry(x = train_x, y = train_y, sampleWeights = weights, mtry = 1, nodesizeSpl = 8)
    rf_aggressive_pred <- predict(rf_aggressive, test_x)
    
    mse <- sqrt(sum((rf_pred - test_y)^2) / length(test_y))
    aggressive_mse <- sqrt(sum((rf_aggressive_pred - test_y)^2) / length(test_y))
  
    print(paste0("MSE: ", mse, " Aggressive MSE: ", aggressive_mse))
}}



