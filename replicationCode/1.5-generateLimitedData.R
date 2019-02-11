library(forestry)
library(MASS)


datasets_grid <- list()



#   Model 5: Large LM-RF Hybrid with big leaves and high penalty 
bike <- read.csv("replicationCode/bike.csv", header = TRUE)
bike <- bike[1:8200,-17]

bike_x <- bike[,c(-1, -2, -16)]
bike_y <- bike[,16]
n_bike <- nrow(bike_x)

test_id <- sort(sample(n_bike, size = n_bike/2))
train_id <- (1:n_bike)[!(1:n_bike) %in% test_id]

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

x <- matrix(runif(5 * n), nrow = n, ncol = 5)
x <- as.data.frame(x)
y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5] + rnorm(n, sd = 10)
semilinear_1 <- cbind(x, y)

datasets_grid[["artificial-semilinear_1"]] <- list(
  "train" = semilinear_1[1:n_train, ], 
  "test" = semilinear_1[(n_train + 1):(n_train + n_test), ])


str(datasets_grid)
