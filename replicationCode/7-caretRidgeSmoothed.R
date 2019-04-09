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
devtools::install_github("soerenkuenzel/forestry", ref = "master")

library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)

data_folder_name <- "replicationCode/estimates/"
dir.create(data_folder_name, showWarnings = FALSE)

source("replicationCode/1.5-generateDataBigtest.R")
source("replicationCode/2-generateEstimators.R")

set.seed(634801)

# Loop through data sets -------------------------------------------------------

# Validate Lambda selection
# Cycle through matrix of estimators/datasets_grid
# Output results as .csv

# Set fraction of data set aside for training + several sample sizes used
samplesize_grid <- 4 * 2^(4:10)
num_avg <- 25

# Loop through all datset, estimator combination
for (sampsize in samplesize_grid) {
  for (dataset_i in 1:length(datasets_grid)) {
    # sampsize = 64; dataset_i = 1
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
      
      params <-
        paste0(data_folder_name, estimator_name,"-", data_name,"-",sampsize,"-",
               "random",".csv")
      if (substr(estimator_name, 1, 5) == "caret") {
        cur_params <- read.csv(params)
      } else if (substr(estimator_name, 1, 6) == "ranger") {
        # We only want to tune ranger once and retrieve parameters
        Er <- estimator(Xobs = as.data.frame(Xtrain),
                        Yobs = Ytrain)
        
        cur_params <- data.frame(mtry = Er$mtry,
                                 min.node.size = Er$min.node.size)
      }
      
      mse_avg <- 0
      train_time_avg <- 0
      prediction_time_avg <- 0
      
      for (iter in 1:num_avg) {
        estimate_i <- {
          #If ridge RF, use version tuned with caret and save final parameters
          
          training_time_start <- Sys.time()
          if (substr(estimator_name, 1, 5) == "caret") {
            
            
            E <- forestry(x = as.data.frame(Xtrain),
                          y = Ytrain,
                          ridgeRF = TRUE,
                          nodesizeSpl = 1,
                          nodesizeAvg = 1,
                          nodesizeStrictAvg = 1,
                          nodesizeStrictSpl = cur_params$nodesizeStrictSpl[1],
                          mtry = cur_params$mtry[1],
                          overfitPenalty = cur_params$overfitPenalty[1])
            
          } else if (substr(estimator_name, 1, 6) == "ranger") {
            E <- ranger(Ytrain ~., 
                        data = cbind(as.data.frame(Xtrain), Ytrain),
                        mtry = cur_params$mtry[1],
                        min.node.size = cur_params$min.node.size[1])
            
          } else {
            E <- estimator(Xobs = as.data.frame(Xtrain),
                           Yobs = Ytrain)
          }
          training_time <- as.numeric(difftime(Sys.time(),
                                               training_time_start,
                                               tz,
                                               units = "mins"))
          
          
          prediction_time_start <- Sys.time()
          pred <- predictor(E, Xtest)
          prediction_time <- as.numeric(difftime(Sys.time(),
                                                 prediction_time_start,
                                                 tz,
                                                 units = "mins"))
          
          pred
        }
        
        cur_mse <- mean((estimate_i - Ytest)^2)
        mse_avg <- mse_avg + (cur_mse / num_avg)
        train_time_avg <- train_time_avg + (training_time / num_avg)
        prediction_time_avg <- prediction_time_avg + (prediction_time / num_avg)
      }
      
      print(paste0("MSE AVg is ", mse_avg, " Train avg is ", train_time_avg, " pred avg ", prediction_time_avg))
      
      estimate_i <- data.frame(estimator_name, 
                               data_name,
                               sampsize,
                               mse = mse_avg,
                               train_time_avg,
                               prediction_time_avg)
      filename <- paste0(data_folder_name, "smooth-",estimator_name,"-", data_name,"-",sampsize, ".csv")
      
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
