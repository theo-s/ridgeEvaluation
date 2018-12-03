library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(ggplot2)

# Define all estimators:

estimator_grid <- list(
  "ridge_1" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, ridgeRF = TRUE, mtry = 3, overfitPenalty = lambda),
  "ridge_2" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, nodesizeSpl = 25, mtry = 3, ridgeRF = TRUE, overfitPenalty = lambda),
  "ridge_3" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, ntree = 80, nodesizeSpl = 40, mtry = 5, ridgeRF = TRUE, overfitPenalty = lambda),
  "ridge_4" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, nodesizeSpl = 80, ridgeRF = TRUE, overfitPenalty = lambda),
  "ridge_5" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, mtry = 3, nodesizeSpl = 15, ridgeRF = TRUE, overfitPenalty = lambda),
  "ridge_6" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, ridgeRF = TRUE, overfitPenalty = 10000),
  
  "forestry_1" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, mtry = 3),
  "forestry_2" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, nodesizeSpl = 25, mtry = 3),
  "forestry_3" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, ntree = 80, nodesizeSpl = 40, mtry = 5),
  "forestry_4" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, nodesizeSpl = 80),
  "forestry_5" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, mtry = 3, nodesizeSpl = 15),
  "forestry_6" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs),
  
  "ranger_1" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs), mtry = 3),
  "ranger_2" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs), min.node.size = 25, mtry = 3),
  "ranger_3" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs), num.trees = 80, min.node.size = 40, mtry = 5),
  "ranger_4" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs), min.node.size = 80),
  "ranger_5" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs), min.node.size = 15, mtry = 3),
  
  "glmnet_1" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 1),
  "glmnet_2" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 0),
  "glmnet_3" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = .5),
  
  "local_RF_1" = function(Xobs, Yobs)
    local_linear_forest(X = Xobs, Y = Yobs, num.trees = 500, mtry = 3),
  "local_RF_2" = function(Xobs, Yobs)
    local_linear_forest(X = Xobs, Y = Yobs, num.trees = 500, min.node.size = 25, mtry = 3),
  "local_RF_3" = function(Xobs, Yobs)
    local_linear_forest(X = Xobs, Y = Yobs, num.trees = 80, min.node.size = 40, mtry = 5),
  "local_RF_4" = function(Xobs, Yobs)
    local_linear_forest(X = Xobs, Y = Yobs, num.trees = 500, min.node.size = 80),
  "local_RF_5" = function(Xobs, Yobs)
    local_linear_forest(X = Xobs, Y = Yobs, num.trees = 500, min.node.size = 15, mtry = 3)
)


predictor_grid <- list(
  "ridge_1" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_2" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_3" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_4" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_5" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_6" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  
  "forestry_1" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "forestry_2" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "forestry_3" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "forestry_4" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "forestry_5" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "forestry_6" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  
  "ranger_1" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_2" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_3" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_4" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_5" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  
  "glmnet_1" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  "glmnet_2" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  "glmnet_3" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  "local_RF_1" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_2" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_3" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_4" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_5" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  }
)
