
# define helper functions:------------------------------------------------------
create_random_node_sizes <- function(nobs, len) {
  # Function creates random node sizes
  potential_sample_space <- c(1:20, round(exp((1:20) / 4) / exp(5) * nobs / 4))
  potential_sample_space <- unique(potential_sample_space)
  
  potential_sample_space[potential_sample_space < 2] <- 2
  
  potential_sample_space <- potential_sample_space[
    potential_sample_space != 0 & potential_sample_space < nobs]
  
  if (length(potential_sample_space) < 1) {
    stop(paste0("It seems like there are not enough samples to generate ",
                "nodesizes. It looks like the samples size is ", nobs, "."))
  }
  return(sample(potential_sample_space, size = len, replace = TRUE))
}


# Define all estimators:--------------------------------------------------------

estimator_grid <- list(
  "caretRidgeRF" = function(Xobs, Yobs, tune_length = 50, cv_fold = 8, note = NA) {
    library(forestry)
    library(caret)
    
    ridgeRF <- list(type = "Regression",
                    library = "forestry",
                    loop = NULL,
                    parameters = data.frame(
                      parameter = c("mtry", "nodesizeStrictSpl", "overfitPenalty", "minSplitGain", "ntree"),
                      class = rep("numeric", 5),
                      label = c("mtry", "nodesizeStrictSpl", "overfitPenalty", "minSplitGain", "ntree")),
                    grid = function(x, y, len = NULL, search = "random") {
                      ## Define ranges for the parameters and
                      ## generate random values for them
                      
                      paramGrid <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
                                              nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                                                           len = len),
                                              # Might want to pass specific range/distribution for lambdas
                                              overfitPenalty = exp(runif(len,
                                                                         min = log(.1),
                                                                         max = log(10))),
                                              minSplitGain = runif(len, 0, .5)^4, 
                                              ntree = 500
                      )
                      return(paramGrid)
                    },
                    fit = function(x, y, wts, param, lev = NULL, last, weights, classProbs) {
                      print(param)
                      
                      forestry(x = x,
                               y = y,
                               ridgeRF = TRUE,
                               ntree = param$ntree,
                               nodesizeSpl = 1,
                               nodesizeAvg = 1,
                               nodesizeStrictAvg = 1,
                               nthread = 1,
                               nodesizeStrictSpl = param$nodesizeStrictSpl,
                               mtry = param$mtry,
                               overfitPenalty = param$overfitPenalty,
                               saveable = FALSE, 
                               minSplitGain = param$minSplitGain)
                    },
                    predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
                      predict(modelFit, newdata)
                    },
                    prob = NULL)
    
    fitControl <- trainControl(method = "adaptive_cv",
                               ## 8-fold CV
                               number = cv_fold,
                               ## repeated 5 times
                               repeats = 4,
                               adaptive = list(min = 2, alpha = 0.05,
                                               method = "gls", complete = TRUE))
    
    random_rf <- train(Yobs ~.,
                       data = cbind(Xobs, Yobs),
                       method = ridgeRF,
                       metric = "RMSE",
                       tuneLength = tune_length,
                       trControl = fitControl)
    
    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(object = list(random_rf), 
            file = paste0("replicationCode/tuningParam/RidgeForest", note, ".RDS"))
    
    
    return(list("random_rf" = random_rf$finalModel))
  },
  "caretRidgeTree" = function(Xobs, Yobs, tune_length = 100, cv_fold = 8, note = NA) {
    library(forestry)
    library(caret)
    
    ridgeRF <- list(type = "Regression",
                    library = "forestry",
                    loop = NULL,
                    parameters = data.frame(
                      parameter = c("mtry", "nodesizeStrictSpl", "overfitPenalty", "minSplitGain", "ntree"),
                      class = rep("numeric", 5),
                      label = c("mtry", "nodesizeStrictSpl", "overfitPenalty", "minSplitGain", "ntree")),
                    grid = function(x, y, len = NULL, search = "random") {
                      ## Define ranges for the parameters and
                      ## generate random values for them
                      
                      paramGrid <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
                                              nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                                                           len = len),
                                              # Might want to pass specific range/distribution for lambdas
                                              overfitPenalty = exp(runif(len,
                                                                         min = log(.1),
                                                                         max = log(10))),
                                              minSplitGain = runif(len, 0, .5)^4, 
                                              ntree = 1
                      )
                      return(paramGrid)
                    },
                    fit = function(x, y, wts, param, lev = NULL, last, weights, classProbs) {
                      print(param)
                      
                      forestry(x = x,
                               y = y,
                               replace = TRUE,
                               sample.fraction = 1,
                               ridgeRF = TRUE,
                               ntree = param$ntree,
                               nodesizeSpl = 1,
                               nodesizeAvg = 1,
                               nodesizeStrictAvg = 1,
                               nthread = 1,
                               nodesizeStrictSpl = param$nodesizeStrictSpl,
                               mtry = param$mtry,
                               overfitPenalty = param$overfitPenalty,
                               saveable = FALSE, 
                               minSplitGain = param$minSplitGain)
                    },
                    predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
                      predict(modelFit, newdata)
                    },
                    prob = NULL)
    
    fitControl <- trainControl(method = "adaptive_cv",
                               ## 8-fold CV
                               number = cv_fold,
                               ## repeated 5 times
                               repeats = 4,
                               adaptive = list(min = 2, alpha = 0.05,
                                               method = "gls", complete = TRUE))
    
    random_rf <- train(Yobs ~.,
                       data = cbind(Xobs, Yobs),
                       method = ridgeRF,
                       metric = "RMSE",
                       tuneLength = tune_length,
                       trControl = fitControl)
    
    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(object = list(random_rf), 
            file = paste0("replicationCode/tuningParam/RidgeForest", note, ".RDS"))
    
    
    return(list("random_rf" = random_rf$finalModel))
  },
  
  # "ranger" = function(Xobs, Yobs, tune_length = 25, cv_fold = 4) {
  #   library(ranger)
  #   library(caret)
  #   
  #   
  #   fitControl <- trainControl(method = "adaptive_cv",
  #                              ## 5-fold CV
  #                              number = cv_fold,
  #                              ## repeated 5 times
  #                              repeats = 2,
  #                              adaptive = list(min = 3, alpha = 0.05,
  #                                              method = "BT", complete = TRUE))
  #   
  #   create_random_node_sizes <- function(nobs, len) {
  #     # Function creates random node sizes
  #     potential_sample_space <- c(1:20, round(exp((1:20) / 4) / exp(5) * nobs / 4))
  #     potential_sample_space <- unique(potential_sample_space)
  #     
  #     potential_sample_space <- potential_sample_space[
  #       potential_sample_space != 0 & potential_sample_space < nobs]
  #     
  #     if (length(potential_sample_space) < 1) {
  #       stop(paste0("It seems like there are not enough samples to generate ",
  #                   "nodesizes. It looks like the samples size is ", nobs, "."))
  #     }
  #     return(sample(potential_sample_space, size = len, replace = TRUE))
  #   }
  #   
  #   grid <- function(x, y, len) {
  #     paramGrid <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
  #                             splitrule = "variance",
  #                             min.node.size = create_random_node_sizes(nobs = nrow(x),
  #                                                                      len = len))
  #     return(paramGrid)
  #   }
  #   
  #   ranger_grid <- grid(Xobs, Yobs, tune_length)
  #   browser()
  #   tuned_ranger <- train(Yobs ~.,
  #                         data = cbind(Xobs, Yobs),
  #                         method = 'ranger',
  #                         metric = "RMSE",
  #                         tuneLength = length,
  #                         trControl = fitControl,
  #                         tuneGrid = ranger_grid)
  #   
  #   return(tuned_ranger$finalModel)
  # },
  
  
  
  "glmnet" = function(Xobs, Yobs, note = NA) {
    library(glmnet)
    library(caret)
    
    glmnet(x = data.matrix(Xobs), y = Yobs)
  },
  
  "local_RF" = function(Xobs, Yobs, note = NA) {
    library(grf)
    library(caret)
    library(onehot)
    encoder <- onehot(Xobs)
    
    llf <- local_linear_forest(
      X = predict(encoder, Xobs),
      Y = Yobs,
      num.trees = 500,
      num.threads = 1
    )
    return(list(encoder, llf))
  },
  
  
  
  "cubist" = function(Xobs, Yobs, note = NA) {
    library(Cubist)
    library(caret)
    
    cubist(x = Xobs, y = Yobs)
  }
)


predictor_grid <- list(
  "caretRidgeRF" = function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  },
  
  "caretRidgeTree" = function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
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
    return(predict(estimator[[2]], 
                   newdata = predict(estimator[[1]], feat))$predictions)
  },
  
  
  "cubist" = function(estimator, feat) {
    return(predict(estimator, feat))
  }
  
)
