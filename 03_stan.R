
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

# model -------------------------------------------------------------------

# Data
#
library(rethinking)
data(Howell1) ; d <- Howell1
rethinking::precis(d)
d2 <- d[d$age >18,]

# Model
# h ~ normal(mu, sigma)
# mu ~ normal(178, 20) 
# sigma ~ uniform(0, 50)

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

dat_ls <- list(
                 N = length(d2$height),
                 h = d2$height
)
# Model 
#  h ~ normal(mu, sigma);
#  mu ~ normal(170, 20);
#  sigma ~ uniform(0, 50);

# Note: test.stan requires to end with a blank line
fml <- file.path(getwd(), "stan", "mdl31.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)

fit <- mdl$sample(
  data = dat_ls, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500
)

fit$summary()
fit$cmdstan_diagnose()
samples <- fit$draws(variables = c("mu", "sigma"), format = "df")
bayesplot::mcmc_trace(samples)
# Alternatively with rstan
stanfit <- rstan::read_stan_csv(fit$output_files())
rstan::traceplot(stanfit)

# Plot: Marginal posterior densities
#
plot( density(samples$mu) )
# Compare to a normal distribution 
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
(Sigma <- cov(samples[,1:2]))
cov2cor(Sigma)
# Learning about mu tells us almost nothing about sigma!
# cor(samples[,1:2])

# Multivariate sampling
# (get > 4e3 samples)
#
mean(samples$Sigma)
(mu <- sapply(samples, mean)[-3])
(Sigma <- cov(samples[,1:2]))
N <- 1e5
MASS::mvrnorm(N, mu, Sigma)

# model -------------------------------------------------------------------

# Data example
#
library(rethinking)
data(Howell1) ; d <- Howell1
rethinking::precis(d)
d2 <- d[d$age >18,]
plot(d2$weight, d2$height)

# Linear model 
#
# h ~ normal(mu, sigma)
#   mu_i = a + b(x_i - xbar) 
#     a ~ normal(178, 20)
#     b ~ normal(0, 10)
#   sigma ~ uniform(0, 50)

# Prior implications
#
# alpha prior
curve(dnorm(x, 178, 20), from=100, to=250)
# beta prior
curve(dnorm(x, 0, 10), from=-50, to=50)
# sigma prior
curve(dunif(x, 0, 50), from=-10, to=60)

# log normal pripr
curve(dlnorm(x, 0, 1), from=0, to=10, xlab="weight", ylab="height")
# check the 95% percentile interval
qv <- qlnorm(c(.5,.95), meanlog=0, sdlog=1)
abline(v=qv, lty=2)

# Prior predictive simulation
#
# mu 
N <- 1e2
a <- rnorm(N, 178, 20)
# b <- rnorm(N, 0, 10)
b <- rlnorm(N, 0, 1)
x <- d2$weight ; y <- d2$height
xbar <- mean(d2$weight) 
plot(NULL, xlim=range(x), ylim=c(-100, 400),
     xlab="Weight", ylab="Height")
abline(h=c(0,272), lty = 2)
for(i in 1:N) {
  curve(a[i] + b[i]*(x-xbar), from=min(d2$weight)[1], to=max(d2$weight), 
        add=TRUE, col=col.alpha("black", .2)) 
}

# Data 
#
w_seq <- seq(20,70)
dat_list <- list(
                 h = d2$height,
                 w = d2$weight,
                 N = length(d2$height),
                 w_seq = w_seq,
                 W = length(w_seq)
)

# Definition 
#
fml <- file.path(getwd(), "stan", "mdl32.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(
  data = dat_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500
)
# Samples 
#
samples <- fit$draws(format="df")

# Diagnostics
#
fit$cmdstan_diagnose()
fit$cmdstan_summary()
fit$summary()
bayesplot::mcmc_trace(samples)
# Alternatively with rstan
stanfit <- rstan::read_stan_csv(fit$output_files())
rstan::traceplot(stanfit)

# Posterior line predictions
w_seq <- 20:70 
wbar <- mean(d2$weight)
N_samples <- nrow(samples)
calc_mu <- function(weight) samples$alpha + samples$beta*(weight-wbar)
# Posterior line prediction
mu <- vapply(w_seq, calc_mu, double(N_samples))
# Posterior predictions
sigma <- samples$sigma
sim_height <- function(weight) {
  rnorm(N_samples,
        mean=samples$alpha + samples$beta * (weight - wbar),
        sd=samples$sigma)
}
h_tilde <- vapply(w_seq, sim_height, double(N_samples))

# MAP
mu_mean <- colMeans(mu)
# mu's HPDI
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
# h_tilde's HPDI
h_HPDI <- apply(h_tilde, 2, rethinking::HPDI)

# Visualize (Spaghetti plot)
#
N_samples <- nrow(samples)
plot(d2$weight, d2$height, col="lightgrey")
for(i in seq(N_samples)) {
  curve(samples$alpha[i] + samples$beta[i]*(x - wbar), add=TRUE)
}
plot(d2$weight, d2$height, col=col.alpha(rangi2,"0.5"))
# MAP line 
lines(w_seq, mu_mean)
# High Posterior Density Intervals
# Distribution of mu 
rethinking::shade(mu_HPDI, w_seq)
# High Posterior Density Intervals
# Model expects to find 89% of actual hights within...
rethinking::shade(h_HPDI, w_seq)

# Posterior Covmat
round(cor(samples[,2:4]),digits=2)

# Posterior distribution for the mean height 
# ..of a 50kg Kung
#
# plot(density(samples$mu[,30]))
plot(density(mu[,31]))
plot(density(samples$"mu[31]"))

# Posterior line predictions (mu)
# for Kungs between 20-50kg
# 
plot(d2$weight, d2$height, col="lightblue")
for(i in seq(w_seq)){
  points(w_seq, samples$mu[i,], pch=16)
}
mean.mu <- apply(samples$mu, 2, mean)
points(w_seq, mean.mu, pch=20, col="white", cex=.5)

plot(d2$weight, d2$height, col="lightblue")
# ASM: Same prob. mass at each tail 
mean.mu <- apply(samples$mu, 2, mean)
PI.mu <- apply(samples$mu, 2, rethinking::PI)
lines(w_seq, mean.mu)
rethinking::shade(PI.mu, w_seq)

# Posterior prediction
#

# Model fitted ... but visualization is missing
dat_list <- list(
  y = rbinom(1e3, 1, .5),
  N = 1e3 
)
# Note: test.stan requires to end with a blank line
file <- file.path(getwd(), "test.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(
  data = dat_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500
)

# Note: test.stan requires to end with a blank line
file <- file.path(getwd(), "f32.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)

fit <- mdl$sample(
  data = dat_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500
)

fit$print()
fit$sampler_diagnostics()
fit$cmdstan_diagnose()

mu_samples <- fit$draws(variables = "mu", format = "df")
bayesplot::mcmc_trace(mu_samples)

