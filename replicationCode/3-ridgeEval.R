if (dir.exists("~/Dropbox/ridgeEvaluation/")) {
  setwd("~/Dropbox/ridgeEvaluation/")
} else {
  setwd("~/ridgeEvaluationCode/")
}
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
# Cycle through matrix of estimators/datasets
# Output results as .csv

# Loop through all datset, estimator combination
for (dataset_i in 1:length(datasets)) {
  print(paste("Dataset = ", names(datasets)[dataset_i]))

  data <- datasets[[dataset_i]]
  Xobs <- data[,-ncol(data)]
  Yobs <- data[,ncol(data)]
  data_name <- names(datasets)[dataset_i]
  
  for (estimator_i in 1:length(estimator_grid)) {
    print(paste("Estimator = ", estimator_i))
    
    estimate_i <- NULL
    
    estimator <- estimator_grid[[estimator_i]]
    estimator_name <- names(estimator_grid)[estimator_i]
    predictor <- predictor_grid[[estimator_name]]
    
    estimate_i <-
      tryCatch({
        E <- estimator(Xobs = Xobs, Yobs = Yobs)
        predictor(E, Xobs)
      },
      error = function(err) {
        print(err)
        warning(paste("Error when running", estimator_name))
        return(NA)
      })
        
    estimate_i <- as.data.frame(estimate_i)
    
    filename <-
      paste0(data_folder_name, estimator_name,"-", data_name, ".csv")
    
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

