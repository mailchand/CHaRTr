tic.clearlog()
tic("total")
tic("data generation")
X <- matrix(rnorm(50000*1000), 50000, 1000)
b <- sample(1:1000, 1000)
y <- runif(1) + X %*% b + rnorm(50000)
toc(log = TRUE, quiet = TRUE)
tic("model fitting")
model <- lm(y ~ X)
toc(log = TRUE, quiet = TRUE)
toc(log = TRUE, quiet = TRUE)