# 
# Prior implications
#

# Before setting up a joint prior model for the analysis, I inspect the prior
# for each parameter independently. 

# In a nutshell
#

curve(dnorm(x, 0, 0.01), from=-3, to=3)
qv <- qnorm(c(.05, .50, .95), mean = 0, sd=0.01)
abline(v=qv, lty=2)
set.seed(123)

N <- 1e3 ; mu <- 0 ; sigma <- 0.01
alpha <- rnorm(N, mu, sigma)
sum(alpha > 2*sigma | alpha < -2*sigma)/N

# Documented version
#

# alpha priors
#

# Normal

# Unstandardized
curve(dnorm(x, 178, 20), from=100, to=250)

# Standardized 
# Note: Should be 0!
curve(dnorm(x, 0, 0.01), from=-3, to=3)
# Probabilites
p <- c(.05, .50, .95)
# Quantile values
(qv <- qnorm(p, mean = 0, sd=0.01))
abline(v=qv, lty=2)

# Quantiles
q <- c(-0.01645, 0, 0.01645)
(pv <- pnorm(q, mean = 0, sd=0.01))
# [1] 0.04998491 0.50000000 0.95001509

# Random deviates 
set.seed(123)
N <- 1e3 ; mu <- 0 ; sigma <- 0.01
alpha <- rnorm(N, mu, sigma)
# Are 5% of all cases above 2*sigma=0.2?
sum(alpha > 2*sigma | alpha < -2*sigma)/N
# [1] 0.047


# beta priors
#

# Normal (unstandardized) 

curve(dnorm(x, 0, 10), from=-50, to=50)

# Normal (standardized)

# Setup
N <- 1e3 ; mu <- 0 ; sigma <- 0.5
# Density function 
curve(dnorm(x, mu, sigma), from=-3, to=3, xlab="X", ylab="Y")
# Probabilites 
p <- c(0.05, 0.5, 0.95)
# Quantile values
qv <- qnorm(p, mu, sigma)
# Add the quantile values to the plot
abline(v=qv, lty=2)

# Determine RNG state
set.seed(123)
# Random deviates 
alpha <- rnorm(N, mu, sigma)
# Are 5% of all cases above 2*sigma=0.2?
sum(alpha > 2*sigma | alpha < -2*sigma)/N
# [1] 0.047

# Lognormal prior (unstandardized)

curve(dlnorm(x, 0, 1), from=0, to=10, xlab="weight", ylab="height")

# Lognormal prior (standardized)

# Setup
N <- 1e3 ; mu_log <- -1 ; sigma_log <- .6

# Note: ln(X) ~ normal(mu, sigma). 
# Here reversed: X ~ lognormal(mu_log, sigma_log)
curve(dlnorm(x, mu_log, sigma_log), from=0, to=10, xlab="X", ylab="Y")
# Probabilites
p <- c(0.05, 0.5, 0.95)
# Quantile values
(qv <- qlnorm(p, mu_log, sigma_log))
# Add the quantile values to the plot
abline(v=qv, lty=2)

# Determine RNG state
set.seed(123)
# Random deviates 
alpha <- rlnorm(N, mu_log, sigma_log)
# Are 5% of all cases above 1? 
sum(alpha > 1)/N




# Student t?



# Plot the prior implications
#




# Additional: 95% interval 
#

curve(dlnorm(x, 0, 1), from=0, to=10, xlab="weight", ylab="height")
# Quantile values for the 95% percentile interval
qv <- qlnorm(c(.05, .50, .95), meanlog=0, sdlog=1)
abline(v=qv, lty=2)

# Sigma priors
#

# sigma prior
curve(dexp(x, 1), from=-1, to=5)
curve(dunif(x, 0, 50), from=-10, to=60)


curve(dlnorm(x, 0, 1), from=0, to=10, xlab="weight", ylab="height")




















