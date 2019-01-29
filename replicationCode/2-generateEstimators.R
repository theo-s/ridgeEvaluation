library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(Cubist)
library(ggplot2)
library(caret)

# Define all estimators:

estimator_grid <- list(
  "tunedRidgeRF" = function(Xobs, Yobs) {
    ridgeRF <- list(type = "Regression",
                    library = "forestry",
                    loop = NULL) 
    
    params <- data.frame(parameter = c("mtry", "nodesizeSpl", "overfitPenalty"),
                         class = rep("numeric", 3),
                         label = c("mtry", "nodesizeSpl", "overfitPenalty"))
    
    ridgeGrid <- function(x, y, len = 1, search = "random") {
      ## Define ranges for the parameters and
      ## generate random values for them
      
      grid <- data.frame(mtry = sample(1:ncol(x), 
                                       len, 
                                       replace = TRUE),
                         nodesizeSpl = ceiling(exp(runif(len, 
                                                         min = log(5),
                                                         max = log(.25*nrow(x))))),
                         # Might want to pass specific range/distribution for lambdas ?? 
                         overfitPenalty = exp(runif(len, 
                                                    min = log(.05), 
                                                    max = log(15)))
      )
      grid
    }
    
    ridgeFit <- function(x, y, wts, param, lev = NULL, last, weights, classProbs) { 
      forestry(x = x, 
               y = y, 
               ridgeRF = TRUE, 
               nodesizeSpl = param$nodesizeSpl, 
               nodesizeStrictSpl = 1,
               mtry = param$mtry, 
               overfitPenalty = param$overfitPenalty)
    }
    
    ridgePred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
      predict(modelFit, newdata)
    }
    
    ridgeProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
      foresty::predict(modelFit, newdata, type = "probabilities")
    }
    
    
    ridgeRF$parameters <- params
    ridgeRF$grid <- ridgeGrid
    ridgeRF$fit <- ridgeFit
    ridgeRF$predict <- ridgePred
    ridgeRF$prob <- ridgeProb
    
    fitControl <- trainControl(method = "repeatedcv",
                               ## 5-fold CV
                               number = 10,
                               ## repeated 5 times
                               repeats = 10,
                               adaptive = list(min = 5, alpha = 0.05, 
                                               method = "gls", complete = TRUE)
    )
    
    rf <- train(Yobs ~.,
                data = cbind(Xobs, Yobs), 
                method = ridgeRF,
                metric = "RMSE",
                tuneLength = 10,
                trControl = fitControl)
    rf
  },
  

  "forestry" = function(Xobs, Yobs, lambda)
    forestry(Xobs, Yobs),
  
  "ranger" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),

  
  "glmnet" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs),

  
  "local_RF" = function(Xobs, Yobs)
    local_linear_forest(X = Xobs, Y = Yobs, num.trees = 500),

  
  "cubist" = function(Xobs, Yobs)
    cubist(x = Xobs, y = Yobs)
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
  "ridge_7" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_8" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_9" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "ridge_10" = function(estimator, feat) {
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
  },
  "local_RF_6" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_7" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_8" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_9" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  "local_RF_10" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  
  "cubist_1" = function(estimator, feat) {
    return(predict(estimator, feat))
  }
  
)
