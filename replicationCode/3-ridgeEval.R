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
devtools::install_github("soerenkuenzel/forestry", ref = "RidgeRF")

library(forestry)
library(ranger)
library(glmnet)
library(tidyverse)
library(reshape)

data_folder_name <- "replicationCode/estimates/"
dir.create(data_folder_name, showWarnings = FALSE)

source("replicationCode/1-generateData.R")
source("replicationCode/2-generateEstimators.R")

set.seed(634801)

# Loop through data sets -------------------------------------------------------

# Validate Lambda selection
# Cycle through matrix of estimators/datasets_grid
# Output results as .csv

# Set fraction of data set aside for training + several sample sizes used
samplesize_grid <- 4 * 2^(1:10)


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
      if(file.exists(filename)) {
        print("File already exists. Running next file!")
        next()
      }
      
      estimate_i <-
        tryCatch({
          E <- estimator(Xobs = as.data.frame(Xtrain), Yobs = Ytrain)
          predictor(E, Xtest)
        },
        error = function(err) {
          print(err)
          warning(paste("Error when running", estimator_name))
          return(NA)
        })
      
      estimate_i <- as.data.frame(estimate_i)
      estimate_i <- cbind(estimate_i, Ytest)
      
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

