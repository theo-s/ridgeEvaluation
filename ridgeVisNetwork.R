library(visNetwork)
library(forestry)
library(rpart)
library(shiny)


# Simple Iris example with ridgeRF + rpart
y <- iris[1:75,1]
x <- iris[1:75,c(-1, -5)]

rpart_tree <- rpart(y~., 
                    data = x)

forestry_tree <- forestry(x = x,
                          y = y,
                          nodesizeStrictSpl = 10,
                          ntree = 1,
                          ridgeRF = TRUE)

visTree(rpart_tree, main = "Iris classification Tree", width = "100%")

# Get split points data for Ridge tree
forestry_tree <- make_savable(forestry_tree)

feat_names <- colnames(x)
split_feat <- forestry_tree@R_forest[[1]]$var_id
split_val <- forestry_tree@R_forest[[1]]$split_val
