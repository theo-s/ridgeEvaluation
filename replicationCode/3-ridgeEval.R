library(forestry)
library(ranger)
library(glmnet)
library(ggplot2)

# Validate Lambda selection
# Cycle through matrix of estimators/datasets
# Output results as .csv

# Cross Validation function for Lambda Selection in ridgeRF
lambdaCrossValidation <- function(x,
                                  y,
                                  lambdas,
                                  k,
                                  ntree,
                                  nodesizeStrictSpl) {
  x <- as.data.frame(x)
  nfeatures <- ncol(x)
  
  if (nrow(x) %% k != 0) {
    stop("Cannot split observations into selected K folds")
  }
  
  mix <- sample(1:nrow(x))
  fs <- nrow(x) / k
  
  folds <- data.frame(matrix(ncol = fs, nrow = 0))
  
  for (i in 1:k) {
    folds <- rbind(folds, c(mix[(1+(i-1)*fs):(i*fs)]))
  }
  
  results <- data.frame(matrix(ncol = 2, nrow = 0))
  
  # Cycle through Lambdas
  for (l in lambdas) {
    # Train on foldC and test on fold
    mseL <- 0
    for (i in 1:k) {
      
      foldIndex <- as.vector(folds[1,], mode = "numeric")
      xTrain <- x[-foldIndex,]
      yTrain <- y[-foldIndex]
      
      xTest <- x[foldIndex,]
      yTest <- y[foldIndex]
      
      rf <- forestry(x = xTrain, y = yTrain, ntree = ntree, nodesizeStrictSpl = nodesizeStrictSpl, ridgeRF = TRUE, overfitPenalty = l)
      yPred <- predict(rf, xTest)
      
      mse <- sum((yPred - yTest)^2)
      mseL <- mseL + mse
    }
    
    results <- rbind(results, c(l, mseL / k))
  }
  
  colnames(results) <- c("Lambda", "MSE")
  return(results)
}


# Test Lambda selection on iris

x <- iris[,-1]
y <- iris[,1]
lambdas <- c(1:50) / 10

k = 5

lambdaCrossValidation(x,
                      y,
                      lambdas,
                      10)
