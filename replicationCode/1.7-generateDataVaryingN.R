try(setwd("~/Dropbox/ridgeEvaluation/"), silent = TRUE)

library(forestry)
library(MASS)

datasets_grid <- list()

# Artificially created data sets -----------------------------------------------
# Dataset #1 Linear ------------------------------------------------------------
set.seed(776291)
n_train <- 2100
n_test <- 10000
n <- n_train + n_test

p <- 10
nonlinear.feats <- 5

b <- matrix(runif(p,-1, 1), nrow = p, ncol = 1)
b[sample(1:p, nonlinear.feats),] = 0

x <- matrix(rnorm(p * n), nrow = n, ncol = p)

y <- x %*% b + rnorm(n, sd = 2)
x <- as.data.frame(x)
lm_artificial_ds <- cbind(x, y)

for (nobs in 128* 2^(1:4)) {
  data_name <- paste0("artificial-LM-", nobs)
  
  datasets_grid[[data_name]] <- list(
    "test" = lm_artificial_ds[1:n_test, ], 
    "train" = lm_artificial_ds[(n_test + 1):(n_test + nobs), ])
  
}

# Dataset #2 Step Function -----------------------------------------------------
n_train <- 2100
n_test <- 10000
n <- n_train + n_test
p <- 10 
num_levels <- 50

x <- matrix(runif(p * n, 0, 1), nrow = n, ncol = p)
y_levels <- runif(num_levels, -10, 10) 
sample_idx <- sample(1:nrow(x), num_levels)

reg <- forestry(x = x[sample_idx,],
                y = y_levels,
                nodesizeSpl = 1,
                nodesizeStrictSpl = 1,
                nodesizeAvg = 1,
                nodesizeStrictAvg = 1)

simulated_y <- predict(reg, x)

simulated_step <- cbind(x, simulated_y)

for (nobs in 128* 2^(1:4)) {
  data_name <- paste0("simulated-Step-Function-", nobs)
  
  datasets_grid[[data_name]] <- list(
    "test" = lm_artificial_ds[1:n_test, ], 
    "train" = lm_artificial_ds[(n_test + 1):(n_test + nobs), ])
  
}

str(datasets_grid)
