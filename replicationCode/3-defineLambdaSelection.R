library(forestry)
library(ranger)
library(glmnet)
library(ggplot2)

#   Cross Validation for Lambda Selection in ridgeRF

# Add ntrees??
lambdaCrossValidation <- function(x,
                                  y,
                                  lambdas,
                                  k,
                                  estimator) {
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

      rf <- estimator(x, y, l)
      yPred <- predict(rf, xTest)

      mse <- sum((yPred - yTest)^2)
      mseL <- mseL + mse
    }

    results <- rbind(results, c(l, mseL / k))
  }

  colnames(results) <- c("Lambda", "MSE")
  return(results$Lambda[results$Lambda == min(results$Lambda)])
}

