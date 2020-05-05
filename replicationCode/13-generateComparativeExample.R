# A Simple example illustrating the flexibility of Linear Random Forests
library(forestry)
library(grf)
library(ggplot2)

n <- 100 
x <- runif(n,0,1)
y <- ifelse(x > .5, -1*x+ 1, 1*x) + rnorm(n, sd = .04)
df <- data.frame(x,y)

# Locally linear data set
ggplot(data = df, aes(x = x, y = y)) + geom_point(shape = 1) + theme_classic()


# Standard CART tree
rf <- forestry(x = x, 
               y = y,
               nodesizeStrictSpl = 5,
               ntree = 1)

rf_out <- predict(rf, x)
rf_df <- data.frame(x, rf_out)
ggplot(data = rf_df, aes(x = x, y = rf_out)) + geom_point(shape = 1) + theme_classic()


# Linear CART tree
lrf <- forestry(x = x, 
                y = y,
                nodesizeStrictSpl = 5,
                ntree = 1,
                linear = TRUE)

lrf_out <- predict(lrf, x)
lrf_df <- data.frame(x, lrf_out)
ggplot(data = lrf_df, aes(x = x, y = lrf_out)) + geom_point(shape = 1) + theme_classic()


# Local Linear Forest tree
llf <- ll_regression_forest(X = as.data.frame(x), 
                            Y = y, 
                            num.trees = 1,
                            enable.ll.split = TRUE)

llf_out <- predict(llf, as.data.frame(x))$predictions
llf_df <- data.frame(x, llf_out)
ggplot(data = llf_df, aes(x = x, y = llf_out)) + geom_point(shape = 1) + theme_classic()





