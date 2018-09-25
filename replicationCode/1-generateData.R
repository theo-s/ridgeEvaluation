library(forestry)
library(MASS)

# Generate several simulated datasets to run estimators on
# Models 1 and 3 n = 506, p = 13
set.seed(58)
x <- Boston[,-14]
y <- Boston[,14]


#   Model 1: LM-RF Hybrid
ridge <- forestry(x = x, y = y, nodesizeStrictSpl = 25  ,ridgeRF = TRUE, overfitPenalty = 1.5)
medval <- predict(ridge, x) + rnorm(nrow(x), sd = 2)

lmrf <- cbind(x, medval)

#   Model 2: Purely Linear Model (Specify number of features linear on)

n <- 500
p <- 8
nonlinear.feats <- 6

b <- matrix(runif(p,-1,1), nrow = p, ncol = 1)
b[sample(1:p, nonlinear.feats),] = 0

x <- matrix(rnorm(p*n), nrow = n, ncol = 8)

y <- x %*% b + rnorm(n)
x <- as.data.frame(x)
lm <- cbind(x, y)


#   Model 3: RF TREE
rf <- forestry(x = x, y = y)

medval <- predict(rf, x) + rnorm(nrow(x), sd = 2)
tree <- cbind(x, medval)

datasets <- list("LM-RF" = lmrf,
                 "LM" = lm,
                 "RF" = tree)
