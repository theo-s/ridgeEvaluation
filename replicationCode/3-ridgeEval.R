library(forestry)
library(ranger)
library(glmnet)
library(tidyverse)
library(reshape)

setwd("~/ridgeEvaluationCode/")
dir.create("simulatedData/estimates", showWarnings = FALSE)

# Validate Lambda selection
# Cycle through matrix of estimators/datasets
# Output results as .csv

# Loop through all datsets
