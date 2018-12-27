library(forestry)
library(MASS)


datasets_grid <- list()

# Boston data sets -------------------------------------------------------------
# n = 506, p = 13
# Generate several simulated datasets to run estimators on
# set.seed(56280222)
# x <- Boston[,-14]
# y <- Boston[,14]
# n_boston <- nrow(Boston)
# 
# test_id <- sort(sample(n_boston, size = n_boston/2))
# train_id <- (1:n_boston)[!(1:n_boston) %in% test_id]
#
# #   Model 1: LM-RF Hybrid with big leaves and high penalty
# ridge <- forestry(x = x, y = y, nodesizeStrictSpl = 25, ridgeRF = TRUE,
#                   overfitPenalty = 1.5)
# imputed_y <- predict(ridge, x) + rnorm(nrow(x), sd = 2)
# 
# ridge_simulated_data <- data.frame(x, y = imputed_y)
# 
# datasets_grid[["Boston-LM-RF-smooth"]] <- list(
#   "train" = ridge_simulated_data[train_id, ],
#   "test" = ridge_simulated_data[test_id, ])
# 
# #   Model 2: LM-RF Hybrid small leaves and low penalty
# ridge <- forestry(x = x, y = y, nodesizeStrictSpl = 5, ridgeRF = TRUE,
#                   overfitPenalty = 0.3)
# imputed_y <- predict(ridge, x) + rnorm(nrow(x), sd = 2)
# 
# ridge_simulated_data <- data.frame(x, y = imputed_y)
# 
# datasets_grid[["Boston-LM-RF-spiky"]] <- list(
#   "train" = ridge_simulated_data[train_id, ],
#   "test" = ridge_simulated_data[test_id, ])
# 
# 
# #   Model 3: Linear Model
# limod <- lm(y ~ ., data = x)
# imputed_y <- predict(limod, x) + rnorm(nrow(x), sd = 1)
# 
# limod_simulated_data <- data.frame(x, y = imputed_y)
# 
# datasets_grid[["Boston-LM"]] <- list(
#   "train" = limod_simulated_data[train_id, ],
#   "test" = limod_simulated_data[test_id, ])
# 
# #   Model 4: RF
# rf <- forestry(x = x, y = y)
# 
# imputed_y <- predict(rf, x) + rnorm(nrow(x), sd = 1)
# 
# rf_simulated_data <- data.frame(x, y = imputed_y)
# 
# datasets_grid[["Boston-RF"]] <- list(
#   "train" = rf_simulated_data[train_id, ],
#   "test" = rf_simulated_data[test_id, ]) 

#   Model 5: Large LM-RF Hybrid with big leaves and high penalty 
bike <- read.csv("replicationCode/bike.csv", header = TRUE)
bike <- bike[1:8200,-17]

bike_x <- bike[,c(-1, -2, -16)]
bike_y <- bike[,16]
n_bike <- nrow(bike_x)

test_id <- sort(sample(n_bike, size = n_bike/2))
train_id <- (1:n_bike)[!(1:n_bike) %in% test_id]

ridge <- forestry(x = bike_x, y = bike_y, nodesizeStrictSpl = 25, ridgeRF = TRUE, 
                  overfitPenalty = 1.5)


imputed_y <- predict(ridge, bike_x) + rnorm(nrow(bike_x), sd = 10)

ridge_simulated_data <- data.frame(bike_x, y = imputed_y)

datasets_grid[["Bike-LM-RF-smooth"]] <- list(
  "train" = ridge_simulated_data[train_id, ], 
  "test" = ridge_simulated_data[test_id, ])

#   Model 6: Large LM-RF Hybrid small leaves and low penalty
ridge <- forestry(x = bike_x, y = bike_y, nodesizeStrictSpl = 5, ridgeRF = TRUE, 
                  overfitPenalty = .5)


imputed_y <- predict(ridge, bike_x) + rnorm(nrow(bike_x), sd = 15)

ridge_simulated_data <- data.frame(bike_x, y = imputed_y)

datasets_grid[["Bike-LM-RF-spiky"]] <- list(
  "train" = ridge_simulated_data[train_id, ], 
  "test" = ridge_simulated_data[test_id, ])


# Artificially created data sets -----------------------------------------------
set.seed(776291)
n_train <- 5000
n_test <- 5000
n <- n_train + n_test

p <- 5
nonlinear.feats <- 3

b <- matrix(runif(p,-1, 1), nrow = p, ncol = 1)
b[sample(1:p, nonlinear.feats),] = 0

x <- matrix(rnorm(p * n), nrow = n, ncol = p)

y <- x %*% b + rnorm(n, sd = 2)
x <- as.data.frame(x)
lm_artificial_ds <- cbind(x, y)

datasets_grid[["artificial-LM-Small"]] <- list(
  "train" = lm_artificial_ds[1:n_train, ], 
  "test" = lm_artificial_ds[(n_train + 1):(n_train + n_test), ])

p <- 10
nonlinear.feats <- 5

b <- matrix(runif(p,-1, 1), nrow = p, ncol = 1)
b[sample(1:p, nonlinear.feats),] = 0

x <- matrix(rnorm(p * n), nrow = n, ncol = p)

y <- x %*% b + rnorm(n, sd = 2)
x <- as.data.frame(x)
lm_artificial_ds <- cbind(x, y)

datasets_grid[["artificial-LM-Medium"]] <- list(
  "train" = lm_artificial_ds[1:n_train, ], 
  "test" = lm_artificial_ds[(n_train + 1):(n_train + n_test), ])

p <- 15
nonlinear.feats <- 8

b <- matrix(runif(p,-1, 1), nrow = p, ncol = 1)
b[sample(1:p, nonlinear.feats),] = 0

x <- matrix(rnorm(p * n), nrow = n, ncol = p)

y <- x %*% b + rnorm(n, sd = 2)
x <- as.data.frame(x)
lm_artificial_ds <- cbind(x, y)

datasets_grid[["artificial-LM-Large"]] <- list(
  "train" = lm_artificial_ds[1:n_train, ], 
  "test" = lm_artificial_ds[(n_train + 1):(n_train + n_test), ])

x <- matrix(runif(5 * n), nrow = n, ncol = 5)
x <- as.data.frame(x)
y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5] + rnorm(n, sd = 10)
semilinear_1 <- cbind(x, y)

datasets_grid[["artificial-semilinear_1"]] <- list(
  "train" = semilinear_1[1:n_train, ], 
  "test" = semilinear_1[(n_train + 1):(n_train + n_test), ])

x <- matrix(runif(3 * n), nrow = n, ncol = 3)
x <- as.data.frame(x)
y <- 10*x[,1] + 5*(x[,2])^2 + (x[,3])^3 + rnorm(n, sd = 5)
semilinear_2 <- cbind(x, y)

datasets_grid[["artificial-semilinear_2"]] <- list(
  "train" = semilinear_2[1:n_train, ], 
  "test" = semilinear_2[(n_train + 1):(n_train + n_test), ])


str(datasets_grid)

