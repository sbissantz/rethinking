options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# Fluctuation phenomenon --------------------------------------------------

# Normal by addition
#
N <- 1e3 ; steps <- 16
pos <- replicate(N, sum(runif(steps, -1, 1)))

hist(pos)
plot(density(pos))

# Normal by small multiplications
#
N <- 1e3 ; steps <- 12
pos <- replicate(N, prod(1 + runif(steps, 0, 0.1)))
hist(pos)
plot(density(pos))

N <- 1e3 ; steps <- 12
# Normal
small <- replicate(N, prod(1 + runif(steps, 0, 0.01)))
# Not Normal
big <- replicate(N, prod(1 + runif(steps, 0, 0.5)))
# Visualize
plot(density(small)) 
plot(density(big))

# Normal by log multiplication
#
log.big <- replicate(N, log(prod(1 + runif(steps, 0, 0.5))))
plot(density(log.big))

# Data
#
library(rethinking)
data(Howell1) ; d <- Howell1
rethinking::precis(d)
d2 <- d[d$age >18,]

# Model
# h ~ Normal(mu, sigma)
# mu ~ Normal(178, 20) 
# sigma ~ Uniform(0, 50)

# Priors
#
# Prior for mu
curve( dnorm(x, 178, 20), from=100, to=250 )
# Prior for sigma
curve( dunif(x, 0, 50), from=-10, to=60 )

# Prior predictive simulation
#
N <- 1e3
# Prior for mu
sample_mu <- rnorm(N, 178, 20) 
# Prior for sigma
sample_sigma <- runif(N, 0, 50) 
# Joint prior
prior_h <- rnorm(N, sample_mu, sample_sigma)
# Visualize the joint prior
plot(density(prior_h))

# Model
#
dat_ls <- list(
                 N = length(d2$height),
                 h = d2$height
)

model.stan <- "
data{
    int<lower=0> N;
    real<lower=0> h[N];
}
parameters{
   real mu; 
   real sigma; 
}
model{
    h ~ normal(mu, sigma);
    mu ~ normal(170, 20);
    sigma ~ uniform(0, 50);
}
"
fit <- rstan::stan(model_code = model.stan, data = dat_ls)
samples <- rstan::extract(fit)

# Plot: Marginal posterior densities
#
plot( density(samples$mu) )
# Add a normal distribution 
mu <- mean(samples$mu)
sigma <- sd(samples$mu)
curve(dnorm(x, mu, sigma), add=TRUE, lty=2)

# Plot: Marginal posterior densities
#
plot( density(samples$sigma) )
# Add a normal distribution 
mu <- mean(samples$sigma)
sigma <- sd(samples$sigma)
curve(dnorm(x, mu, sigma), add=TRUE, lty=2)

# Plot: Joint posterior distribution
#
plot( samples$mu, samples$sigma, pch=20, cex=0.3)

# Posterior-Relations between parameters
# 
(Sigma <- cov(as.matrix(fit)[,-3]))
cor(as.matrix(fit)[,-3])
cov2cor(Sigma)
# Learning about mu tells us almost nothing about sigma!

# Multicariate sampling
# (get > 4e3 samples)
#
(mu <- sapply(samples, mean)[-3])
(Sigma <- cov(as.matrix(fit)[,-3]))
N <- 1e5
MASS::mvrnorm(N, mu, Sigma = cov(as.matrix(fit)[,-3]))









