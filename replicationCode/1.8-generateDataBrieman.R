library(MASS)
library(gclus)
library(forestry)

datasets_grid <- list()

data(ozone)
Boston
servo <- read.csv("replicationCode/servo.csv", header = TRUE)
abalone <- read.csv("replicationCode/abalone.csv")

set.seed(3984938)

# Simulated Datasets -----------------------------------------------------------

# Friedman 1
n <- 2200
x <- matrix(runif(10 * n), nrow = n, ncol = 10)
x <- as.data.frame(x)

y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5] + rnorm(n, sd = 1)
friedman_1 <- cbind(x, y)

test_id <- 201:2200
train_id <- 1:200

datasets_grid[["Friedman_1"]] <- list(
  "train" = friedman_1[train_id, ],
  "test" = friedman_1[test_id, ])

# Friedman 2
# Error SD selected to roughly give signal to noise ratio of 3:1
x1 <- runif(n, min = 0, max = 100)
x2 <- runif(n, min = 40*pi, max = 560*pi)
x3 <- runif(n, min = 0, max = 1)
x4 <- runif(n, min = 1, max = 11)

x <- data.frame(x1, x2, x3, x4)

y_2 <- x[,1]^2 + (x[,2]*x[,3] - (1/(x[,2]*x[,3]))^2) ^(.5) + rnorm(n, mean = 0, sd = 600)
friedman_2 <- cbind(x, y_2)


datasets_grid[["Friedman_2"]] <- list(
  "train" = friedman_2[train_id, ],
  "test" = friedman_2[test_id, ])

# Friedman 1
# Error SD selected to roughly give signal to noise ratio of 3:1

y_3 <- atan( (x[,2]*x[,3] - ( 1/(x[,2]*x[,4]) )) / x[,1]) + rnorm(n, mean = 0, sd = .3)
friedman_3 <- cbind(x, y_3)


datasets_grid[["Friedman_3"]] <- list(
  "train" = friedman_3[train_id, ],
  "test" = friedman_3[test_id, ])


# Real Datasets ----------------------------------------------------------------

# Boston Housing
n <- nrow(Boston)

test_id <- sort(sample(n, size = round(.1*n)))
train_id <- (1:n)[!(1:n) %in% test_id]


datasets_grid[["Boston_Housing"]] <- list(
  "train" = Boston[train_id, ],
  "test" = Boston[test_id, ])


# Ozone
ozone <- cbind(ozone, Ozone = ozone$Ozone)
ozone <- ozone[,-1]
n <- nrow(ozone)

test_id <- sort(sample(n, size = round(.1*n)))
train_id <- (1:n)[!(1:n) %in% test_id]


datasets_grid[["Ozone"]] <- list(
  "train" = ozone[train_id, ],
  "test" = ozone[test_id, ])

# Servo
n <- nrow(servo)
test_id <- sort(sample(n, size = round(.1*n)))
train_id <- (1:n)[!(1:n) %in% test_id]


datasets_grid[["Servo"]] <- list(
  "train" = servo[train_id, ],
  "test" = servo[test_id, ])

# Abalone
n <- nrow(abalone)

test_id <- sort(sample(n, size = round(.25*n)))
train_id <- (1:n)[!(1:n) %in% test_id]


datasets_grid[["Abalone"]] <- list(
  "train" = abalone[train_id, ],
  "test" = abalone[test_id, ])


str(datasets_grid)
