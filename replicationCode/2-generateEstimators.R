
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


# Define all estimators:========================================================

estimator_grid <- list()


#Tuning forestry RF ------------------------------------------------------------
estimator_grid[["forestryRF"]] <- function(Xobs,
                                             Yobs,
                                             tune_length = 50,
                                             cv_fold = 8,
                                             note = NA) {
  library(forestry)
  library(caret)
  
  forestryRF <- list(
    type = "Regression",
    library = "forestry",
    loop = NULL,
    parameters = data.frame(
      parameter = c(
        "mtry",
        "nodesizeStrictSpl",
        "ntree", 
        "sample.fraction"
      ),
      class = rep("numeric", 4),
      label = c(
        "mtry",
        "nodesizeStrictSpl",
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
        ntree = param$ntree,
        sample.fraction = param$sample.fraction,
        nodesizeSpl = 1,
        nodesizeAvg = 1,
        nodesizeStrictAvg = 1,
        nthread = 1,
        nodesizeStrictSpl = param$nodesizeStrictSpl,
        mtry = param$mtry,
        saveable = FALSE
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
      min = 2,
      alpha = 0.05,
      method = "gls",
      complete = TRUE
    )
  )
  
  random_rf <- train(
    Yobs ~ .,
    data = cbind(Xobs, Yobs),
    method = forestryRF,
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl
  )
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(random_rf),
    file = paste0("replicationCode/tuningParam/ForestryForest", note, ".RDS")
  )
  
  return(list("random_rf" = random_rf$finalModel))
}

#Tuning Ridge RF --------------------------------------------------------------
estimator_grid[["caretRidgeRF"]] <- function(Xobs,
                                            Yobs,
                                            tune_length = 50,
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
      min = 2,
      alpha = 0.05,
      method = "gls",
      complete = TRUE
    )
  )
  
  random_rf <- train(
    Yobs ~ .,
    data = cbind(Xobs, Yobs),
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

# Tune Ridge Tree --------------------------------------------------------------
estimator_grid[["caretRidgeTree"]] <- function(Xobs,
                                               Yobs,
                                               tune_length = 100,
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
            4,
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
      min = 2,
      alpha = 0.05,
      method = "gls",
      complete = TRUE
    )
  )
  
  random_rf <- train(
    Yobs ~ .,
    data = cbind(Xobs, Yobs),
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
  
# Tune ranger --------------------------------------------------------------
estimator_grid[["ranger"]] <- function(Xobs,
                                       Yobs,
                                       tune_length = 100,
                                       cv_fold = 8,
                                       note = NA) {
  # browser()
  # Xobs = iris[,-1]
  # Yobs = iris[,1]
  library(ranger)
  library(caret)
  library(BradleyTerry2)
  
  fitControl <- trainControl(
    method = "adaptive_cv",
    ## 5-fold CV
    number = cv_fold,
    ## repeated 5 times
    repeats = 4,
    adaptive = list(
      min = 2,
      alpha = 0.05,
      method = "BT",
      complete = TRUE
    )
  )
  
  grid <- function(x, y, len) {
    paramGrid <-
      data.frame(
        mtry = sample(1:ncol(x), size = len, replace = TRUE),
        splitrule = "variance",
        min.node.size = create_random_node_sizes(nobs = nrow(x),
                                                 len = len)
      )
    return(paramGrid)
  }
  
  ranger_grid <- grid(Xobs, Yobs, tune_length)
  tuned_ranger <- train(
    Yobs ~ .,
    data = cbind(Xobs, Yobs),
    method = 'ranger',
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl,
    tuneGrid = ranger_grid)
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(tuned_ranger),
    file = paste0("replicationCode/tuningParam/ranger", note, ".RDS")
  )
  
  return(tuned_ranger$finalModel)
}

# Tuning glmnet ----------------------------------------------------------------
estimator_grid[["glmnet"]] <- function(Xobs,
                                       Yobs,
                                       tune_length = 100,
                                       cv_fold = 8,
                                       note = NA) {
  library(glmnet)
  library(caret)
  browser()
  
  encoder <- onehot::onehot(Xobs)
  
  Xobs <- predict(encoder, Xobs)
  
  
  ridgeRF <- list(
    type = "Regression",
    library = "glmnet",
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
            4,
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
      
      glmnet(
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
    ## 5-fold CV
    number = cv_fold,
    ## repeated 5 times
    repeats = 10,
    adaptive = list(
      min = 5,
      alpha = 0.02,
      method = "BT",
      complete = TRUE
    )
  )
  
  tuned_glmnet <- train(
    Yobs ~ .,
    data = cbind(Xobs, Yobs),
    method = 'glmnet',
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl, 
    search = "random"
  )
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(tuned_glmnet),
    file = paste0("replicationCode/tuningParam/glmnet", note, ".RDS")
  )
  
  return(list(model = tuned_glmnet$finalModel, encoder = encoder))
}

# Tuning cubist ----------------------------------------------------------------
estimator_grid[["cubist"]] <- function(Xobs,
                                       Yobs,
                                       tune_length = 100,
                                       cv_fold = 8,
                                       note = NA) {
  library(cubist)
  library(caret)
  
  encoder <- onehot::onehot(Xobs)
  
  Xobs <- predict(encoder, Xobs)
  
  fitControl <- trainControl(
    method = "adaptive_cv",
    ## 5-fold CV
    number = cv_fold,
    ## repeated 5 times
    repeats = 10,
    adaptive = list(
      min = 5,
      alpha = 0.02,
      method = "BT",
      complete = TRUE
    )
  )
  
  tuned_cubist <- train(
    Yobs ~ .,
    data = cbind(Xobs, Yobs),
    method = 'cubist',
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl, 
    search = "random"
  )
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(tuned_cubist),
    file = paste0("replicationCode/tuningParam/cubist", note, ".RDS")
  )
  
  return(list(model = tuned_cubist$finalModel, encoder = encoder))
}

# Tuning localRF ---------------------------------------------------------------
estimator_grid[["local_RF"]] <- function(Xobs,
                                         Yobs,
                                         tune_length = 100,
                                         cv_fold = 8,
                                         note = NA) {
  library(grf)
  library(caret)
  library(onehot)
  encoder <- onehot(Xobs)
  Xobs <- predict(encoder, Xobs)
  
  # llf <- local_linear_forest(
  #   X = predict(encoder, Xobs),
  #   Y = Yobs,
  #   num.trees = 500,
  #   num.threads = 1
  # )
  # return(list(encoder, llf))
  local_RF <- list(
    type = "Regression",
    library = "grf",
    loop = NULL,
    parameters = data.frame(
      parameter = c(
        "mtry",
        "min.node.size",
        "num.trees", 
        "sample.fraction"
      ),
      class = rep("numeric", 4),
      label = c(
        "mtry",
        "min.node.size	",
        "num.trees",
        "sample.fraction"
      )
    ),
    grid = function(x, y, len = NULL, search = "random") {
      ## Define ranges for the parameters and
      ## generate random values for them
      
      paramGrid <-
        data.frame(
          mtry = sample(1:ncol(x), size = len, replace = TRUE),
          min.node.size	 = create_random_node_sizes(nobs = nrow(x),
                                                       len = len),
          num.trees = 500,
          sample.fraction = runif(len, 0.5, 1))
      print(paramGrid)
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
      
      local_linear_forest(
        X = x,
        Y = y,
        num.threads = param$num.trees,
        sample.fraction = param$sample.fraction,
        nthread = 1,
        min.node.size	 = param$min.node.size	,
        mtry = param$mtry
      )
    },
    predict = function(modelFit,
                       newdata,
                       preProc = NULL,
                       submodels = NULL) {
      predict(modelFit, newdata)$predictions
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
      min = 2,
      alpha = 0.05,
      method = "gls",
      complete = TRUE
    )
  )
  
  local_rf <- train(
    Yobs ~ .,
    data = cbind(Xobs, Yobs),
    method = local_RF,
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl
  )
  
  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  saveRDS(
    object = list(local_rf),
    file = paste0("replicationCode/tuningParam/local_rf", note, ".RDS")
  )
  
  return(list("random_rf" = local_rf$finalModel))
}
  
  
# Tuning BART ----------------------------------------------------------------
estimator_grid[["BART"]] <- function(Xobs,
                                     Yobs,
                                     tune_length = NA,
                                     cv_fold = NA,
                                     note = NA) {
  
  return(list(Xobs = Xobs, Yobs = Yobs))
}






predictor_grid <- list(
  "forestryRF" = function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  },
  
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
    return(predict(estimator[[2]], 
                   newdata = predict(estimator[[1]], feat)))
  },
  
  
  "local_RF" = function(estimator, feat) {
    return(predict(estimator[[2]], 
                   newdata = predict(estimator[[1]], feat))$predictions)
  },
  
  
  "cubist" = function(estimator, feat) {
    return(predict(estimator, feat))
  }, 
  
  "BART" = function(estimator, feat) {
    library(dbarts)
    bartFit = wbart(x.train = estimator$Xobs,
                    y.train = estimator$Yobs,
                    x.test = feat)
    
    return(bartFit$yhat.train.mean)
  }
)

