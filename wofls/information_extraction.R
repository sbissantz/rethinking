#
# Information extraction
#

# How much information did the robot extract from the data?

source("prep.R")

#  In a nutshell
#
N <- 1e3
a <- rnorm(N, 0, 0.2)
b <- rnorm(N, 0, 0.5) 
sigma ~ dexp(N, 1)
samples <- fit$draws(format="data.frame")
prior_dens <- density(b)
post_dens <- density(samples$beta_M)
plot(prior_dens, ylim=c(0, max(post_dens$y)), lwd=2, 
     main="Information extraction")
lines(post_dens, lwd=2, col="steelblue")
legend("topright", legend=c("Prior", "Posterior"), pch=c(20,20), 
       col=c("black", "steelblue"))

#
# Documented version 
#

# Number of units
N <- 1e3

# alpha prior
a <- rnorm(N, 0, 0.2)
# Note: This should match the prior of the stan model

# beta prior
b <- rnorm(N, 0, 0.5) 
# Note: This should match the prior of the stan model

# sigma prior
sigma ~ dexp(N, 1)
# Note: This should match the prior of the stan model

# Samples from the posterior 
samples <- fit$draws(format="data.frame")

# Densities from samples
prior_dens <- density(b)
post_dens <- density(samples$beta_M)

# Visualization
plot(prior_dens, ylim=c(0, max(post_dens$y)), lwd=2, 
     main="Information extraction")
lines(post_dens, lwd=2, col="steelblue")
legend("topright", legend=c("Prior", "Posterior"), pch=c(20,20), 
       col=c("black", "steelblue"))

