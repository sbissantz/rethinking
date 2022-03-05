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
  X <- rnorm(N)
  Y <- rnorm(N, b_XY*X)
  Z <- rnorm(N, b_YZ*Y)
  b_X <- coef(lm(Y ~ X))['X']
  b_XZ <- coef(lm(Y ~ X + Z))['X']
  c(b_X, b_XZ)
}
sim <- mcreplicate::mc_replicate(1e4, f(), mc.cores=4)

plot(density(sim[1,]), ylim=c(0,5), xlim=c(0,1.5), lwd=3, 
             xlab="Posterior mean")
lines(density(sim[2,], lwd=3), lwd=3, col="red")
text(x=0.5, 5, "wrong") ; text(x=1, 5, "correct")









