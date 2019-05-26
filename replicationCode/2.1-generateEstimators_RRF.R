

# RidgeRF BT tuned -------------------------------------------------------------
estimator_grid[["caretRidgeRF_BT"]] <- function(Xobs,
                                                Yobs,
                                                tune_length = 200,
                                                cv_fold = 8,
                                                note = NA) {
  library(forestry)
  library(caret)
  
  ridgeRF <- list(
    type = "Regression",
    library = "forestry",
    loop = NULL,
    parameters = data.frame(
      parameter = c(
        "mtry",
        "nodesizeStrictSpl",
        "overfitPenalty",
        "minSplitGain",
        "ntree", 
        "sample.fraction"
      ),
      class = rep("numeric", 6),
      label = c(
        "mtry",
        "nodesizeStrictSpl",
        "overfitPenalty",
        "minSplitGain",
        "ntree",
        "sample.fraction"
      )
    ),
    grid = function(x, y, len = NULL, search = "random") {
      ## Define ranges for the parameters and
      ## generate random values for them
      
      paramGrid <-
        data.frame(
          mtry = sample(1:ncol(x), size = len, replace = TRUE),
          nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                       len = len),
          # Might want to pass specific range/distribution for lambdas
          overfitPenalty = exp(runif(
            len,
            min = log(.1),
            max = log(10)
          )),
          minSplitGain = runif(len, 0, .5) ^
            4,
          ntree = 500,
          sample.fraction = runif(len, 0.5, 1))
      return(paramGrid)
    },
    fit = function(x,
                   y,
                   wts,
                   param,
                   lev = NULL,
                   last,
                   weights,
                   classProbs) {
      print(param)
      
      forestry(
        x = x,
        y = y,
        ridgeRF = TRUE,
        ntree = param$ntree,
        sample.fraction = param$sample.fraction,
        nodesizeSpl = 1,
        nodesizeAvg = 1,
        nodesizeStrictAvg = 1,
        nthread = 1,
        nodesizeStrictSpl = param$nodesizeStrictSpl,
        mtry = param$mtry,
        overfitPenalty = param$overfitPenalty,
        saveable = FALSE,
        minSplitGain = param$minSplitGain
      )
    },
    predict = function(modelFit,
                       newdata,
                       preProc = NULL,
                       submodels = NULL) {
      predict(modelFit, newdata)
    },
    prob = NULL
  )
  
  fitControl <- trainControl(
    method = "adaptive_cv",
    ## 8-fold CV
    number = cv_fold,
    ## repeated 5 times
    repeats = 4,
    adaptive = list(
      min = 3,
      alpha = 0.01,
      method = "BT",
      complete = FALSE
    )
  )
  
  random_rf <- train(
    y = Yobs, 
    x = Xobs, 
    method = ridgeRF,
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl
  )
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(random_rf),
    file = paste0("replicationCode/tuningParam/RidgeForest", note, ".RDS")
  )
  
  return(list("random_rf" = random_rf$finalModel))
}



predictor_grid[["caretRidgeRF_BT"]] <-
  function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  }

# RidgeRF no minSplitGain ------------------------------------------------------
estimator_grid[["caretRidgeRF_noMinSplitGain"]] <- function(Xobs,
                                                            Yobs,
                                                            tune_length = 200,
                                                            cv_fold = 8,
                                                            note = NA) {
  library(forestry)
  library(caret)
  
  ridgeRF <- list(
    type = "Regression",
    library = "forestry",
    loop = NULL,
    parameters = data.frame(
      parameter = c(
        "mtry",
        "nodesizeStrictSpl",
        "overfitPenalty",
        "minSplitGain",
        "ntree", 
        "sample.fraction"
      ),
      class = rep("numeric", 6),
      label = c(
        "mtry",
        "nodesizeStrictSpl",
        "overfitPenalty",
        "minSplitGain",
        "ntree",
        "sample.fraction"
      )
    ),
    grid = function(x, y, len = NULL, search = "random") {
      ## Define ranges for the parameters and
      ## generate random values for them
      
      paramGrid <-
        data.frame(
          mtry = sample(1:ncol(x), size = len, replace = TRUE),
          nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                       len = len),
          # Might want to pass specific range/distribution for lambdas
          overfitPenalty = exp(runif(
            len,
            min = log(.1),
            max = log(10)
          )),
          minSplitGain = 0,
          ntree = 500,
          sample.fraction = runif(len, 0.5, 1))
      return(paramGrid)
    },
    fit = function(x,
                   y,
                   wts,
                   param,
                   lev = NULL,
                   last,
                   weights,
                   classProbs) {
      print(param)
      
      forestry(
        x = x,
        y = y,
        ridgeRF = TRUE,
        ntree = param$ntree,
        sample.fraction = param$sample.fraction,
        nodesizeSpl = 1,
        nodesizeAvg = 1,
        nodesizeStrictAvg = 1,
        nthread = 1,
        nodesizeStrictSpl = param$nodesizeStrictSpl,
        mtry = param$mtry,
        overfitPenalty = param$overfitPenalty,
        saveable = FALSE,
        minSplitGain = param$minSplitGain
      )
    },
    predict = function(modelFit,
                       newdata,
                       preProc = NULL,
                       submodels = NULL) {
      predict(modelFit, newdata)
    },
    prob = NULL
  )
  
  fitControl <- trainControl(
    method = "adaptive_cv",
    ## 8-fold CV
    number = cv_fold,
    ## repeated 5 times
    repeats = 4,
    adaptive = list(
      min = 3,
      alpha = 0.01,
      method = "gls",
      complete = FALSE
    )
  )
  
  random_rf <- train(
    y = Yobs, 
    x = Xobs, 
    method = ridgeRF,
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl
  )
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(random_rf),
    file = paste0("replicationCode/tuningParam/RidgeForest", note, ".RDS")
  )
  
  return(list("random_rf" = random_rf$finalModel))
}



predictor_grid[["caretRidgeRF_noMinSplitGain"]] <-
  function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  }

# Tune Ridge Tree more min splitGain -------------------------------------------
estimator_grid[["caretRidgeTree_moreSplit"]] <- function(Xobs,
                                                         Yobs,
                                                         tune_length = 400,
                                                         cv_fold = 8,
                                                         note = NA) {
  library(forestry)
  library(caret)
  
  ridgeRF <- list(
    type = "Regression",
    library = "forestry",
    loop = NULL,
    parameters = data.frame(
      parameter = c(
        "mtry",
        "nodesizeStrictSpl",
        "overfitPenalty",
        "minSplitGain",
        "ntree"
      ),
      class = rep("numeric", 5),
      label = c(
        "mtry",
        "nodesizeStrictSpl",
        "overfitPenalty",
        "minSplitGain",
        "ntree"
      )
    ),
    grid = function(x, y, len = NULL, search = "random") {
      ## Define ranges for the parameters and
      ## generate random values for them
      
      paramGrid <-
        data.frame(
          mtry = sample(1:ncol(x), size = len, replace = TRUE),
          nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                       len = len),
          # Might want to pass specific range/distribution for lambdas
          overfitPenalty = exp(runif(
            len,
            min = log(.1),
            max = log(10)
          )),
          minSplitGain = runif(len, 0, .5) ^
            1.5,
          ntree = 1
        )
      return(paramGrid)
    },
    fit = function(x,
                   y,
                   wts,
                   param,
                   lev = NULL,
                   last,
                   weights,
                   classProbs) {
      print(param)
      
      forestry(
        x = x,
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
        minSplitGain = param$minSplitGain
      )
    },
    predict = function(modelFit,
                       newdata,
                       preProc = NULL,
                       submodels = NULL) {
      predict(modelFit, newdata)
    },
    prob = NULL
  )
  
  fitControl <- trainControl(
    method = "adaptive_cv",
    ## 8-fold CV
    number = cv_fold,
    ## repeated 5 times
    repeats = 4,
    adaptive = list(
      min = 3,
      alpha = 0.01,
      method = "gls",
      complete = FALSE
    )
  )
  
  random_rf <- train(
    y = Yobs, 
    x = Xobs, 
    method = ridgeRF,
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl
  )
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(random_rf),
    file = paste0("replicationCode/tuningParam/RidgeTree", note, ".RDS")
  )
  
  
  return(list("random_rf" = random_rf$finalModel))
}

predictor_grid[["caretRidgeTree_moreSplit"]] <-
  function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  }
