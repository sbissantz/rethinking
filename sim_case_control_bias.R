#
# Simulate Case Control Bias
#

# Functional relationships
#
# X: indep
# Y: f(X) 
# Z: f(Y) 

# Model sketch
#
# y_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_X*X_i + beta_Y*Y_i 
# alpha ~ normal(0,0.2)
# beta_X ~ normal(0,0.5)
# beta_Y ~ normal(0,0.5)
# sigma ~ exponential(1)

f <- function(N=100, b_XY=1, b_YZ=1) {
  N <- 1e3
  X <- rnorm(N)
  Y <- rnorm(b_XY*X)
  Z <- rnorm(b_YZ*Y)
  b_X <- lm((Y ~ X))['X']
  b_XZ <- lm(Y ~ X + Z)['X']
  c(b_X, b_XZ)
}
sim <- mcreplicate::mc_replicate(1e4, f(), mc.cores=4)







