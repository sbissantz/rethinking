dag <- dagitty::dagitty('dag {
                        X [pos="0,0"]
                        Y [pos="1,0"]
                        Z [pos="0,1"]
                        Z <- X -> Y 
}')
plot(dag)

# Functional relationships
#
N <- 1e3
X <- rnorm(N)
# Z: common cause of X & Y
Y <- rnorm(N, X)
Z <- rnorm(N, -X)
cor(data.frame(X,Y,Z))

# Sketch
#
# Y ~ normal(mu_i, sigma)
# mu_i = alpha + beta_X * X_i + beta_Z * Z_i
# alpha ~ normal(0, 0.2)
# beta_X ~ normal(0, 0.5)
# beta_Y ~ normal(0, 0.5)
# sigma ~exponential(1)

# PPS
#
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
# Relationship X,Y (same for Z,Y)
alpha <- rnorm(N, 0, 0.2)
beta_X <- rnorm(N, 0, 0.5)
for(i in 1:100) abline(a = alpha[i], b = beta_X[i],  
                       col=scales::alpha("steelblue", .3))

# Reduction
#
dat_ls <- list(N=N,X=X,Y=Y,Z=Z)

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_sim_spurious.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format="data.frame")
bayesplot::mcmc_trace(samples)

# Inferene
#
par(mfrow=c(1,2))
plot(X,Z, pch=20, col=scales::alpha("steelblue", .3))
for(i in 1:100) abline(a=samples$alpha[i], b=samples$beta_Z[i], 
                       col=scales::alpha("black", 0.3))
title("Z & Y | Z")
plot(X,Y, pch=20, col=scales::alpha("steelblue", .3))
for(i in 1:100) abline(a=samples$alpha[i], b=samples$beta_X[i], 
                       col=scales::alpha("black", 0.3))
title("X & Y | Z")
par(mfrow=c(1,1))
