#
# Simulate Bias Amlification
#
# Functional relationships
# Z: indep.
# U: indep.
# X: f(U,Z)
# Y: f(U,X)
N <- 1e3
Z <- rnorm(N, 0,1)
U <- rnorm(N, 0,1)
b_ZX <- 2 ; b_U <- 2
X <- rnorm(N, b_U*U+b_ZX*Z)
b_U <- 10 ; b_XY <- 4
Y <- rnorm(N, b_U*U+b_XY*X)
d <- data.frame(Z, U, X, Y)
round(cor(d), digits=2)

# Model sketch
#
# y_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_X*X_i beta_Z*Z_i
# alpha ~ normal(0,0.2)
# beta_X ~ normal(0,0.5)
# beta_Z ~ normal(0,0.5)
# sigma ~ exponential(1)

# Reduction
#
dat_ls <- list(N=nrow(d), X=X, Y=Y, Z=Z)

# Less confounded model
#
lm(Y ~ X, data=dat_ls)

# Fitting
#
file <- file.path(getwd(), "stan", "simulations", "bias_amplification.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
# Bias amplification!
fit$print()