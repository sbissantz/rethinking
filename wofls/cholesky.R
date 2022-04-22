#
# Cholesky factorization
#

# Simulated correlated RVs
#

# Correlation matrix
(R <- matrix(c(1,0.7,0.7,1),2,2))
# Cholesky decomposition (R=L^TL)
L <- chol(R)
# t(L) %*% L # since: R=L^T*L
# Matrix of standard deviations
sigma <- diag(c(1,2))
# Multiply sigma with the lower triangular square root of the correlation
# matrix: t(L)
Lambda <- sigma %*% t(L)
# RNG
set.seed(123)
N <- 1e3
# z ~ normal(0,1)
Z <- rbind(rnorm(N),rnorm(N))
X <- Lambda %*% Z

# rho
cor(X[1,], X[2,])

d <- data.frame(height = X[1,], weight=X[2,]) 
