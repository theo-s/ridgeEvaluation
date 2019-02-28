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

    create_random_node_sizes <- function(nobs, len) {
      # Function creates random node sizes
      potential_sample_space <- c(1:20, round(exp((1:20) / 4) / exp(5) * nobs / 4))
      potential_sample_space <- unique(potential_sample_space)

      potential_sample_space <- potential_sample_space[
        potential_sample_space != 0 & potential_sample_space < nobs]

      if (length(potential_sample_space) < 1) {
        stop(paste0("It seems like there are not enough samples to generate ",
                    "nodesizes. It looks like the samples size is ", nobs, "."))
      }
      return(sample(potential_sample_space, size = len, replace = TRUE))
    }

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
                                              nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                                                           len = len),
                                              # Might want to pass specific range/distribution for lambdas
                                              overfitPenalty = exp(runif(len,
                                                                         min = log(.05),
                                                                         max = log(10)))
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
                               repeats = 5,
                               adaptive = list(min = 3, alpha = 0.05,
                                               method = "gls", complete = TRUE))

    random_rf <- train(Yobs ~.,
                       data = cbind(Xobs, Yobs),
                       method = ridgeRF,
                       metric = "RMSE",
                       tuneLength = 50,
                       trControl = fitControl)

    return(list("random_rf" = random_rf$finalModel))
  },
  
  "forestry" = function(Xobs, Yobs)
    forestry(Xobs, Yobs),
  
  "ranger" = function(Xobs, Yobs) {
    length <- 50
    fitControl <- trainControl(method = "repeatedcv",
                               ## 5-fold CV
                               number = 5,
                               ## repeated 5 times
                               repeats = 10,
                               adaptive = list(min = 5, alpha = 0.05,
                                               method = "gls", complete = TRUE))
    
    create_random_node_sizes <- function(nobs, len) {
      # Function creates random node sizes
      potential_sample_space <- c(1:20, round(exp((1:20) / 4) / exp(5) * nobs / 4))
      potential_sample_space <- unique(potential_sample_space)
      
      potential_sample_space <- potential_sample_space[
        potential_sample_space != 0 & potential_sample_space < nobs]
      
      if (length(potential_sample_space) < 1) {
        stop(paste0("It seems like there are not enough samples to generate ",
                    "nodesizes. It looks like the samples size is ", nobs, "."))
      }
      return(sample(potential_sample_space, size = len, replace = TRUE))
    }
    
    grid <- function(x, y, len) {
      paramGrid <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
                              splitrule = "variance",
                              min.node.size = create_random_node_sizes(nobs = nrow(x),
                                                                       len = len))
      return(paramGrid)
    }
    
    ranger_grid <- grid(Xobs, Yobs, length)
    
    tuned_ranger <- train(Yobs ~.,
                          data = cbind(Xobs, Yobs),
                          method = 'ranger',
                          metric = "RMSE",
                          tuneLength = length,
                          trControl = fitControl,
                          tuneGrid = ranger_grid)
    
    return(tuned_ranger$finalModel)
  },
  
  
  
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
