library(forestry)
library(MASS)

# Generate several simulated datasets to run estimators on
set.seed(58)
x <- Boston[,-14]
y <- Boston[,14]

#   Model 1: LM-RF Hybrid
ridge <- forestry(x = x, y = y, nodesizeStrictSpl = 25  ,ridgeRF = TRUE, overfitPenalty = 1.5)
medval <- predict(ridge, x) + rnorm(nrow(x), sd = 2)

lmrf <- cbind(x, medval)

#   Model 2: Purely Linear Model (p = 8, n = 500)
b <- matrix(runif(8,-1,1), nrow = 8, ncol = 1)
b[c(2,5),] = 0
b
x <- matrix(rnorm(8*500), nrow = 500, ncol = 8)

y <- x %*% b + rnorm(500)
x <- as.data.frame(x)
lm <- cbind(x, y)


#   Model 3: RF TREE
rf <- forestry(x = x, y = y)

medval <- predict(rf, x) + rnorm(nrow(x), sd = 2)
tree <- cbind(x, medval)

datasets <- list("LM-RF" = lmrf,
                 "LM" = lm,
                 "RF" = tree)
