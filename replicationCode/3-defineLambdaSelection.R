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

  x <- x[sample(nrow(x)),]
  folds <- folds <- cut(seq(1, nrow(x)), breaks = k, labels = FALSE)

  results <- data.frame(matrix(ncol = 2, nrow = 0))

  # Cycle through Lambdas
  for (l in lambdas) {
    # Train on foldC and test on fold
    mseL <- 0
    for (i in 1:k) {

      xTrain <- x[folds != i,]
      yTrain <- y[folds != i]

      xTest <- x[folds == i,]
      yTest <- y[folds == i]

      rf <- estimator(Xobs = xTrain, Yobs = yTrain, lambda = l)
      yPred <- predict(rf, xTest)

      mse <- sum((yPred - yTest)^2)
      mseL <- mseL + mse
    }

    results <- rbind(results, c(l, mseL / k))
  }

  colnames(results) <- c("Lambda", "MSE")
  return(results$Lambda[results$Lambda == min(results$Lambda)])
}

