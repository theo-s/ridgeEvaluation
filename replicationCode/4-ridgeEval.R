if (dir.exists("~/Dropbox/ridgeEvaluation/")) {
  setwd("~/Dropbox/ridgeEvaluation/")
} else if (dir.exists("~/ridgeEvaluationCode/")) {
  setwd("~/ridgeEvaluationCode/")
} else if (dir.exists("~/ridgeEvaluation/")) {
  setwd("~/ridgeEvaluation/")
} else {
  stop("wd was not set correctly")
}

# install most up to date version of forestry
devtools::install_github("soerenkuenzel/forestry", ref = "master")

library(forestry)
library(ranger)
library(glmnet)
library(tidyverse)
library(reshape)

data_folder_name <- "replicationCode/estimates/"
dir.create(data_folder_name, showWarnings = FALSE)

source("replicationCode/1-generateData.R")
source("replicationCode/2-generateEstimators.R")
source("replicationCode/3-defineLambdaSelection.R")

set.seed(634801)

# Loop through data sets -------------------------------------------------------

# Validate Lambda selection
# Cycle through matrix of estimators/datasets_grid
# Output results as .csv

# Set fraction of data set aside for training + several sample sizes used
samplesize_grid <- 4 * 2^(1:10)


# Fold # and lambda list for CV
lambdas <- c(1:30) / 10
k <- 5

# Loop through all datset, estimator combination
for (sampsize in samplesize_grid) {
  for (dataset_i in 1:length(datasets_grid)) {
    # sampsize = 128; dataset_i = 2
    data_name <- names(datasets_grid)[dataset_i]
    
    if (sampsize > nrow(datasets_grid[[dataset_i]][["train"]])) {
      next
    }
    
    print(paste("Dataset =", data_name, 
                "and smpsize =", sampsize))
  
    data_train <- datasets_grid[[dataset_i]][["train"]][1:sampsize,]
    data_test <- datasets_grid[[dataset_i]][["test"]]
    
    Xtrain <- data_train[, -ncol(data_train)]
    Xtest <- data_test[, -ncol(data_test)]
    
    Ytrain <- data_train[, ncol(data_train)]
    Ytest <- data_test[, ncol(data_test)]
    
    for (estimator_i in 1:length(estimator_grid)) {
      # estimator_i = 1
      print(paste("Estimator = ", estimator_i))
      
      estimate_i <- NULL
      
      estimator <- estimator_grid[[estimator_i]]
      estimator_name <- names(estimator_grid)[estimator_i]
      predictor <- predictor_grid[[estimator_name]]
      
      filename <-
        paste0(data_folder_name, estimator_name,"-", data_name,"-",sampsize, 
               ".csv")
      if (file.exists(filename)) {
        print("File already exists. Running next file!")
        next()
      }

      training_time <- prediction_time <- NA
      estimate_i <-
        tryCatch({
          #If ridge RF, CV select lambda
          if (substr(estimator_name, 1, 5) == "ridge") {
            l <- lambdaCrossValidation(as.data.frame(Xtrain),
                                       Ytrain,
                                       lambdas,
                                       k,
                                       estimator)
          }
          
          training_time_start <- Sys.time()
          if (substr(estimator_name, 1, 5) == "ridge") {
            E <- estimator(Xobs = as.data.frame(Xtrain),
                           Yobs = Ytrain,
                           lambda = l)
          } else {
            E <- estimator(Xobs = as.data.frame(Xtrain),
                           Yobs = Ytrain)
          }
          
          training_time <- as.numeric(difftime(Sys.time(),
                                               training_time_start,
                                               tz,
                                               units = "mins"))
          
          prediction_time_start <- Sys.time()
          pdts <- predictor(E, Xtest)
          prediction_time <- as.numeric(difftime(Sys.time(),
                                                 prediction_time_start,
                                                 tz,
                                                 units = "mins"))
          pdts
        },
        error = function(err) {
          print(err)
          warning(paste("Error when running", estimator_name))
          return(NA)
        })
      
      estimate_i <- data.frame(estimator_name, 
                               data_name,
                               sampsize,
                               y_estimate = as.numeric(estimate_i),
                               y_true = Ytest,
                               training_time,
                               prediction_time)
      filename <- paste0(data_folder_name, estimator_name,"-", data_name,"-",sampsize, ".csv")
      
      col.names <- !file.exists(filename)
      
      write.table(
        estimate_i,
        file = filename,
        col.names = col.names,
        row.names = FALSE,
        sep = ","
        )
    }
  }
}

