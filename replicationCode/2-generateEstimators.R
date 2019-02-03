library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(Cubist)
library(ggplot2)
library(caret)

# Define all estimators:

estimator_grid <- list(
  "caretRidgeRF" = function(Xobs, Yobs) {
    ridgeRF <- list(type = "Regression",
                    library = "forestry",
                    loop = NULL,
                    params = data.frame(parameter = c("mtry", "nodesizeStrictSpl", "overfitPenalty"),
                                        class = rep("numeric", 3),
                                        label = c("mtry", "nodesizeStrictSpl", "overfitPenalty")),
                    grid = function(x, y, len = 1, search = "random") {
                      ## Define ranges for the parameters and
                      ## generate random values for them
                      
                      paramGrid <- data.frame(mtry = sample(1:ncol(x), 
                                                            len, 
                                                            replace = TRUE),
                                              nodesizeStrictSpl = ceiling(exp(runif(len, 
                                                                                    min = log(5),
                                                                                    max = log(.25*nrow(x))))),
                                              # Might want to pass specific range/distribution for lambdas
                                              overfitPenalty = exp(runif(len, 
                                                                         min = log(.05), 
                                                                         max = log(15)))
                      )    
                      return(paramGrid) 
                    },
                    fit = function(x, y, wts, param, lev = NULL, last, weights, classProbs) { 
                      forestry(x = x, 
                               y = y, 
                               ridgeRF = TRUE, 
                               nodesizeSpl = 1, 
                               nodesizeAvg = 1,
                               nodesizeStrictAvg = 1,
                               nodesizeStrictSpl = param$nodesizeStrictSpl,
                               mtry = param$mtry, 
                               overfitPenalty = param$overfitPenalty,
                               saveable = FALSE)
                    },
                    predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
                      predict(modelFit, newdata)
                    },
                    prob= NULL)
    
    fitControl <- trainControl(method = "repeatedcv",
                               ## 5-fold CV
                               number = 10,
                               ## repeated 5 times
                               repeats = 10,
                               adaptive = list(min = 5, alpha = 0.05, 
                                               method = "gls", complete = TRUE))
    
    rf <- train(Yobs ~.,
                data = cbind(Xobs, Yobs), 
                method = ridgeRF,
                metric = "RMSE",
                tuneLength = 10,
                trControl = fitControl)
    return(rf)
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
  "caretRidgeRF" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  
  
  "forestry" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  
  
  "ranger" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  
  
  "glmnet" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  
  
  "local_RF" = function(estimator, feat) {
    return(predict(estimator, newdata = feat)$predictions)
  },
  
  
  "cubist" = function(estimator, feat) {
    return(predict(estimator, feat))
  }
  
)
