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


# Iris to test CV Lambda selection

x <- iris[,-1]
y <- iris[,1]
lambdas <- c(1:50) / 10

k = 5

lambdaCrossValidation(x,
                      y,
                      lambdas,
                      10)


#   Model 1: LM-RF Hybrid
set.seed(154)
b <- matrix(runif(8,-1,1), nrow = 8, ncol = 1)
x <- matrix(rnorm(8*500), nrow = 500, ncol = 8)
x <- as.data.frame(x)
y <- ifelse((x[,1] > .3),
            .9 * x[,1] + ifelse((x[,3] < 1.4),
                                -3.2*x[,2] + ifelse((x[,1] > 1),
                                                  2*x[,1], -.05*x[,2]), 3.297*x[,5]), .456*x[,4])
y <- matrix(y, ncol = 1, nrow = 500)
lambdas <- c(1:50) / 10
l <- lambdaCrossValidation(x = x, y = y, lambdas = lambdas, 5, 2, 15)
l <- l$Lambda[l$MSE == min(l$MSE)]
rf <- forestry(x,y,nodesizeStrictSpl = 15,ridgeRF = TRUE, overfitPenalty = l)

data <- cbind(x,y)
ranger <- ranger(y ~., data = data)

sum((y - predict(ranger, data = data)$predictions)^2)
sum((y - predict(rf, x))^2)

#   Model 2: Purely Linear Model (p = 5, n = 100)
set.seed(154)
b <- matrix(runif(5,-1,1), nrow = 5, ncol = 1)
x <- matrix(rnorm(5*250), nrow = 250, ncol = 5)

y <- x %*% b + rnorm(250)
x <- as.data.frame(x)
lambdas <- c(1:50) / 10

l <- lambdaCrossValidation(x = x, y = y, lambdas = lambdas, 5, 2, 25)
l <- l$Lambda[l$MSE == min(l$MSE)]
rf <- forestry(x,y, nodesizeStrictSpl = 25, ridgeRF = TRUE, overfitPenalty = l)

data <- cbind(x,y)
ranger <- ranger(y ~., data = data)

sum((y - predict(ranger, data = data)$predictions)^2)

sum((y - predict(rf, x))^2)


#   Model 3: RF TREE
set.seed(154)
b <- matrix(runif(8,-1,1), nrow = 8, ncol = 1)
x <- matrix(rnorm(5*250), nrow = 250, ncol = 5)
x <- as.data.frame(x)
y <- ifelse((x[,1] > .3),
            3.21 + ifelse((x[,3] < 1.4),
                                -6.7 + ifelse((x[,1] > 1),
                                                    .568, rnorm(1)), 2.68 + rnorm(1,3)), -3.7)
y <- matrix(y, ncol = 1, nrow = 250)
lambdas <- c(1:50) / 10
l <- lambdaCrossValidation(x = x, y = y, lambdas = lambdas, 5, 2, 25)
l <- l$Lambda[l$MSE == min(l$MSE)]
rf <- forestry(x,y,ridgeRF = TRUE, overfitPenalty = l)
data <- cbind(x,y)
ranger <- ranger(y ~., data = data)

sum((y - predict(ranger, data = data)$predictions)^2)

sum((y - predict(rf, x))^2)
