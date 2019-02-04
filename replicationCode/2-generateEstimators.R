library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(Cubist)
library(ggplot2)
library(caret)
library(rBayesianOptimization)

# Define all estimators:

estimator_grid <- list(
  "caretRidgeRF" = function(Xobs, Yobs) {
      
    ridgeRF <- list(type = "Regression",
                    library = "forestry",
                    loop = NULL,
                    parameters = data.frame(
                      parameter = c("mtry", "nodesizeStrictSpl", "overfitPenalty"),
                      class = rep("numeric", 3),
                      label = c("mtry", "nodesizeStrictSpl", "overfitPenalty")),
                    grid = function(x, y, len = NULL, search = "random") {
                      ## Define ranges for the parameters and
                      ## generate random values for them
                      
                      paramGrid <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
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
                               ## 10-fold CV
                               number = 5,
                               ## repeated 5 times
                               repeats = 2,
                               adaptive = list(min = 1, alpha = 0.05, 
                                               method = "gls", complete = TRUE))
    
    random_rf <- train(Yobs ~.,
                       data = cbind(Xobs, Yobs), 
                       method = ridgeRF,
                       metric = "RMSE",
                       tuneLength = 3,
                       trControl = fitControl)
    
    ctrl <- trainControl(method = "repeatedcv", repeats = 5)
    
    ridge_bayes <- function(mtry, nodesizeStrictSpl, overfitPenalty) {
      ## Use the same model code but for a single hyperparameter set
      txt <- capture.output(
        mod <- train(Yobs ~ .,
                     data = cbind(Xobs, Yobs),
                     method = ridgeRF,
                     metric = "RMSE",
                     trControl = ctrl,
                     tuneGrid = data.frame(mtry = ceiling(mtry), 
                                           nodesizeStrictSpl = ceiling(nodesizeStrictSpl),
                                           overfitPenalty = overfitPenalty))
      )
      list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
    }
    
    lower_bound <- c(mtry = 1, 
                     nodesizeStrictSpl = 5,
                     overfitPenalty = .05)
    upper_bound <- c(mtry = ncol(Xobs), 
                     nodesizeStrictSpl = .25*nrow(Xobs),
                     overfitPenalty = 25)
    bounds <- list(mtry = c(lower_bound[1], upper_bound[1]),
                   nodesizeStrictSpl = c(lower_bound[2], upper_bound[2]),
                   overfitPenalty = c(lower_bound[3], upper_bound[3]))
    
    initial_grid <- random_rf$results[, c("mtry", "nodesizeStrictSpl", "overfitPenalty", "RMSE")]
    initial_grid$RMSE <- -initial_grid$RMSE
    names(initial_grid) <- c("mtry", "nodesizeStrictSpl", "overfitPenalty", "Value")
    
    ba_search <- BayesianOptimization(ridge_bayes,
                                      bounds = bounds,
                                      init_grid_dt = initial_grid, 
                                      init_points = 0, 
                                      n_iter = 3,
                                      acq = "ucb", 
                                      kappa = 1, 
                                      eps = 0.0,
                                      verbose = TRUE)
    
    bayes_rf <- train(Yobs ~ ., 
                      data = cbind(Xobs, Yobs),
                      method = ridgeRF,
                      tuneGrid = data.frame(mtry = ceiling(ba_search$Best_Par["mtry"]), 
                                            nodesizeStrictSpl = ceiling(ba_search$Best_Par["nodesizeStrictSpl"]),
                                            overfitPenalty = ba_search$Best_Par["overfitPenalty"]),
                      metric = "RMSE",
                      trControl = ctrl)
    
    return(list("random_rf" = random_rf$finalModel, 
                "bayes_rf" = bayes_rf$finalModel))
  },
  
  "forestry" = function(Xobs, Yobs)
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
