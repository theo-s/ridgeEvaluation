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

source("Using_VisNetwork/ridgeVisNetwork_soe.R")

visualizeRidge(forestry_tree = forestry_tree)

visTree(rpart_tree, main = "Iris classification Tree", width = "100%")



