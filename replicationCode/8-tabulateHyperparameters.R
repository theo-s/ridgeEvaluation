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

library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)
library(xtable)

data_folder_name <- "replicationCode/parameters/"

source("replicationCode/1-generateData.R")

set.seed(634801)

# Loop through data sets -------------------------------------------------------

# Validate Lambda selection
# Cycle through matrix of estimators/datasets_grid
# Output results as .csv

# Set fraction of data set aside for training + several sample sizes used
samplesize_grid <- 4 * 2^(4:10)

total_params <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                         c("Dataset",
                           "Sample_Size",
                           "mtry",
                           "nodesizeStrictSpl",
                           "overfitPenalty"))

# Loop through all datset, estimator combination
for (dataset_i in 1:length(datasets_grid)) {
  for (sampsize in samplesize_grid) {
    # sampsize = 64; dataset_i = 1
    data_name <- names(datasets_grid)[dataset_i]
    
    print(paste("Dataset =", data_name, 
                "and sampsize =", sampsize))
    
    params <-
      paste0(data_folder_name, "caretRidgeRF-", data_name,"-",sampsize,"-",
             "random",".csv")

    cur_params <- read.csv(params)
    
    total_params <- total_params %>% add_row(Dataset = data_name, 
                                             Sample_Size = sampsize,
                                             mtry = cur_params$mtry[1],
                                             nodesizeStrictSpl = cur_params$nodesizeStrictSpl[1],
                                             overfitPenalty = cur_params$overfitPenalty[1])
    
  }
}

xtable(total_params)
