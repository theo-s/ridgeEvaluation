library(forestry)
library(ranger)
library(glmnet)
library(tidyverse)
library(reshape)

setwd("~/ridgeEvaluationCode/")

data_folder_name <- "replicationCode/estimates/"
dir.create(data_folder_name, showWarnings = FALSE)

# Validate Lambda selection
# Cycle through matrix of estimators/datasets
# Output results as .csv

# Set fraction of data set aside for training + several sample sizes used
sample.fraction <- .3
sample.sizes <- c(50)

# Loop through all datset, estimator combination
for (sampsize in sample.sizes) {
  for (dataset_i in 1:length(datasets)) {
    print(paste("Dataset = ", names(datasets)[dataset_i]))
  
    data <- datasets[[dataset_i]]
    data <- data[1:sampsize,]
    test_index <- sample(nrow(data), round(sample.fraction * nrow(data)))
    
    Xtrain <- data[-test_index, -ncol(data)]
    Xtest <- data[test_index, -ncol(data)]
    
    Ytrain <- data[-test_index, ncol(data)]
    Ytest <- data[test_index, ncol(data)]
    data_name <- names(datasets)[dataset_i]
    
    for (estimator_i in 1:length(estimator_grid)) {
      print(paste("Estimator = ", estimator_i))
      
      estimate_i <- NULL
      
      estimator <- estimator_grid[[estimator_i]]
      estimator_name <- names(estimator_grid)[estimator_i]
      predictor <- predictor_grid[[estimator_name]]
      
      estimate_i <-
        tryCatch({
          E <- estimator(Xobs = Xtrain, Yobs = Ytrain)
          predictor(E, Xtest)
        },
        error = function(err) {
          print(err)
          warning(paste("Error when running", estimator_name))
          return(NA)
        })
          
      estimate_i <- as.data.frame(estimate_i)
      estimate_i <- cbind(estimate_i, Ytest)
      
      filename <-
        paste0(data_folder_name, estimator_name,"-", data_name,"-",sampsize, ".csv")
      
      col.names <- !file.exists(filename)
      
      write.table(
        estimate_i,
        file = filename,
        append = TRUE,
        col.names = col.names,
        row.names = FALSE,
        sep = ","
        )
    }
  }
}
