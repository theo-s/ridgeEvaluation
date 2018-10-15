library(forestry)
library(ranger)
library(glmnet)
library(ggplot2)

# Define all estimators:

estimator_grid <- list(
  "ridge_1" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, ridgeRF = TRUE, overfitPenalty = lambda),
  "ridge_2" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, nodesizeSpl = 25, ridgeRF = TRUE, 
             overfitPenalty = lambda),
  "ridge_3" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, ntree = 25, nodesizeSpl = 50, ridgeRF = TRUE, 
             overfitPenalty = lambda),
  "ridge_4" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, nodesizeSpl = 25, ridgeRF = TRUE, 
             overfitPenalty = lambda),
  "ridge_5" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs, mtry = 3, ridgeRF = TRUE, overfitPenalty = lambda),

  "ranger_1" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),
  "ranger_2" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),
  "ranger_3" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),

  "glmnet_1" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 1),
  "glmnet_2" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 0),
  "glmnet_3" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = .5)
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
  
  "ranger_1" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_2" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_3" = function(estimator, feat) {
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
  }
)
