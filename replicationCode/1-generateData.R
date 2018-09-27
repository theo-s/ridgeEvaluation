library(forestry)
library(MASS)


datasets_grid <- list()

# Boston data sets -------------------------------------------------------------
# n = 506, p = 13
# Generate several simulated datasets to run estimators on
set.seed(56280222)
x <- Boston[,-14]
y <- Boston[,14]
n_boston <- nrow(Boston)

test_id <- sort(sample(n_boston, size = n_boston/2))
train_id <- (1:n_boston)[!(1:n_boston) %in% test_id]


#   Model 1: LM-RF Hybrid with big leaves and high penalty
ridge <- forestry(x = x, y = y, nodesizeStrictSpl = 25, ridgeRF = TRUE, 
                  overfitPenalty = 1.5)
imputed_y <- predict(ridge, x) + rnorm(nrow(x), sd = 2)

ridge_simulated_data <- data.frame(x, y = imputed_y)

datasets_grid[["Boston-LM-RF-smooth"]] <- list(
  "train" = ridge_simulated_data[train_id, ], 
  "test" = ridge_simulated_data[test_id, ])

#   Model 2: LM-RF Hybrid small leaves and low penalty
ridge <- forestry(x = x, y = y, nodesizeStrictSpl = 5, ridgeRF = TRUE, 
                  overfitPenalty = 0.3)
imputed_y <- predict(ridge, x) + rnorm(nrow(x), sd = 2)

ridge_simulated_data <- data.frame(x, y = imputed_y)

datasets_grid[["Boston-LM-RF-spiky"]] <- list(
  "train" = ridge_simulated_data[train_id, ], 
  "test" = ridge_simulated_data[test_id, ])


#   Model 3: Linear Model
limod <- lm(y ~ ., data = x)
imputed_y <- predict(limod, x) + rnorm(nrow(x), sd = 1)

limod_simulated_data <- data.frame(x, y = imputed_y)

datasets_grid[["Boston-LM"]] <- list(
  "train" = limod_simulated_data[train_id, ], 
  "test" = limod_simulated_data[test_id, ])

#   Model 4: RF
rf <- forestry(x = x, y = y)

imputed_y <- predict(rf, x) + rnorm(nrow(x), sd = 1)

rf_simulated_data <- data.frame(x, y = imputed_y)

datasets_grid[["Boston-RF"]] <- list(
  "train" = rf_simulated_data[train_id, ], 
  "test" = rf_simulated_data[test_id, ])


# Artificially created data sets -----------------------------------------------
set.seed(776291)
n_train <- 5000
n_test <- 5000
n <- n_train + n_test
p <- 8
nonlinear.feats <- 6

b <- matrix(runif(p,-1, 1), nrow = p, ncol = 1)
b[sample(1:p, nonlinear.feats),] = 0

x <- matrix(rnorm(p * n), nrow = n, ncol = p)

y <- x %*% b + rnorm(n)
x <- as.data.frame(x)
lm_artificial_ds <- cbind(x, y)

datasets_grid[["artificial-LM"]] <- list(
  "train" = lm_artificial_ds[1:n_train, ], 
  "test" = lm_artificial_ds[(n_train + 1):(n_train + n_test), ])


str(datasets_grid)


