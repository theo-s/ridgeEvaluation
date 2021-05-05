library(Rforestry)
library(MASS)


datasets_grid <- list()

# Load in bike dataset
bike <- read.csv("replicationCode/bike.csv", header = TRUE)
bike <- bike[1:14100,-17]

bike_x <- bike[,c(-1, -2, -16)]
bike_y <- bike[,16]
n_bike <- nrow(bike_x)

test_id <- sort(sample(n_bike, size = 10000))
train_id <- (1:n_bike)[!(1:n_bike) %in% test_id]

set.seed(3984938)


# Real Datasets ----------------------------------------------------------------

#------ 4-Linear Function ------------------------------------------------------

sd <- 2
f_l <- as.matrix(c(0, 0, 0, 0, .4, -.9, 0, 0, 2, 0, -1.7, .6, 0), nrow = 13, ncol = 1)
# y <- as.matrix(bike_x) %*% f_l + rnorm(n_bike, mean = 0, sd = sd)
#
# bike_linear_ds <- cbind(bike_x, y)
#
# datasets_grid[["Bike_Linear"]] <- list(
#   "train" = bike_linear_ds[train_id, ],
#   "test" = bike_linear_ds[test_id, ])

#------ 5-Step Function --------------------------------------------------------

set.seed(63)

for (iter in 1:3) {
  num_levels = 20
  range = 10
  sd = 1

  pt_idx <- sample(n_bike, size = num_levels)
  pts <- bike_x[pt_idx,]
  levels <- runif(num_levels, min = -range, max = range)

  f_s <- forestry(x = pts,
                  y = levels,
                  mtry = 1,
                  nodesizeSpl = 1,
                  nodesizeStrictSpl = 1,
                  ntree = 1)

  x <- bike_x
  y <- predict(f_s, bike_x) + rnorm(n_bike, mean = 0, sd = sd)

  bike_step_ds <- cbind(x, y)

  data_name <- paste0("Bike_Step",iter)

  datasets_grid[[data_name]] <- list(
    "train" = bike_step_ds[train_id, ],
    "test" = bike_step_ds[test_id, ])


  #------ 6-Semi-Linear Function -------------------------------------------------

  sd = 2

  summer <- bike_x[bike_x$season == 2 | bike_x$season == 3,]
  winter <- bike_x[bike_x$season == 1 | bike_x$season == 4,]

  summer_y <- as.matrix(summer) %*% f_l + rnorm(nrow(summer), mean = 0, sd = sd)
  winter_y <- predict(f_s, winter) + rnorm(nrow(winter), mean = 0, sd = sd)

  y <- c(summer_y, winter_y)
  x <- rbind(summer, winter)

  bike_hybrid_ds <- cbind(x,y)

  data_name <- paste0("Bike_Hybrid",iter)

  datasets_grid[[data_name]] <- list(
    "train" = bike_hybrid_ds[train_id, ],
    "test" = bike_hybrid_ds[test_id, ])
}

# Simulated Datasets -----------------------------------------------------------
#------ 1-Linear Function ------------------------------------------------------

# DONE IN ARTIFICIAL LM MEDIUM
# p <- 10
# nonlinear.feats <- 5
#
# b <- matrix(runif(p,-1, 1), nrow = p, ncol = 1)
# b[sample(1:p, nonlinear.feats),] = 0
#
# x <- matrix(rnorm(p * n), nrow = n, ncol = p)
#
# y <- x %*% b + rnorm(n, sd = 2)
# x <- as.data.frame(x)
# lm_artificial_ds <- cbind(x, y)
#
# datasets_grid[["artificial-LM-Medium"]] <- list(
#   "train" = lm_artificial_ds[1:n_train, ],
#   "test" = lm_artificial_ds[(n_train + 1):(n_train + n_test), ])

#------ 2-Step Function --------------------------------------------------------
# p <- 10
# n <- 15000
# num_levels <- 50
# sd <- 1
#
# pts <- as.data.frame(matrix(rnorm(p * num_levels), nrow = num_levels, ncol = p))
# levels <- runif(num_levels, min = -10, max = 10)
#
# tree <- forestry(x = pts,
#                  y = levels,
#                  mtry = 1,
#                  nodesizeSpl = 1,
#                  nodesizeStrictSpl = 1,
#                  ntree = 1)
#
# x <- as.data.frame(matrix(rnorm(p * n), nrow = n, ncol = p))
# y <- predict(tree, x) + rnorm(n, mean = 0, sd = sd)
#
# simulated_step_ds <- cbind(x, y)
#
# datasets_grid[["Simulated_Step"]] <- list(
#   "train" = simulated_step_ds[train_id, ],
#   "test" = simulated_step_ds[test_id, ])


#------ 3-Semi-Linear Function -------------------------------------------------

# Done in Friedman's function
# x <- matrix(runif(5 * n), nrow = n, ncol = 5)
# x <- as.data.frame(x)
# y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5] + rnorm(n, sd = 10)
# semilinear_1 <- cbind(x, y)
#
# datasets_grid[["artificial-semilinear_1"]] <- list(
#   "train" = semilinear_1[1:n_train, ],
#   "test" = semilinear_1[(n_train + 1):(n_train + n_test), ])



str(datasets_grid)
