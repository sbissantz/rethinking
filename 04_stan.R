
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
mean(samples$sigma)
(mu <- sapply(samples, mean)[(1:2)])
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
# bayesplot::mcmc_trace(samples)
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
mean.mu <- apply(mu, 2, mean)
points(w_seq, mean.mu, pch=20, col="white", cex=.5)

plot(d2$weight, d2$height, col="lightblue")
# ASM: Same prob. mass at each tail 
mean.mu <- apply(mu, 2, mean)
PI.mu <- apply(mu, 2, rethinking::PI)
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

mu_samples <- fit$draws(variables="mu", format="df")
bayesplot::mcmc_trace(mu_samples)

# mdl441 ------------------------------------------------------------------

library(rethinking)
data("Howell1")
d <- Howell1
plot(d$weight, d$height)

# Data wrangling
#
w_s <- (d$weight-mean(d$weight))/sd(d$weight)
w_s2 <- w_s^2
dat_ls <- list(
  N = nrow(d),
  w_s = w_s,
  w_s2 = w_s2,
  h = d$height
)
file <- file.path(getwd(), "stan", "mdl441.stan")
# Second order polynomial
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(
  data = dat_ls,
  seed = 123,
  refresh = 500
)
fit$cmdstan_diagnose()
samples <- fit$draws(format="df")
# bayesplot::mcmc_trace(samples)
fit$print()

# Posterior line predictions
#
calc_mu <- function(w_s, w_s2) {
  with(samples, alpha + beta_1 * w_s + beta_2 * w_s2)
}
w_seq <- seq(-2,2,length.out=30)
mu <- mapply(calc_mu, w_seq, w_seq^2) 
# Posterior predictions
#
calc_h <- function(N, w_s, w_s2, sigma) {
 rnorm(N,
       mean=with(samples, alpha + beta_1 * w_s + beta_2 * w_s2),
       sd=samples$sigma
)}
N_samples <- nrow(samples)
h_tilde <- mapply(calc_h, N=N_samples, w_seq, w_seq^2)

mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
h_HPDI <- apply(h_tilde, 2, rethinking::HPDI)

plot(w_s, d$height, col="lightgrey")
lines(w_seq, mu_mean)
rethinking::shade(mu_HPDI, w_seq)
rethinking::shade(h_HPDI, w_seq)

# model 45 ----------------------------------------------------------------

library(rethinking)
data("Howell1")
d <- Howell1

# Data wrangling
#
d$w_s <- (d$weight - mean(d$weight))/ sd(d$weight)
dat_ls <- list(
  N = nrow(d),
  w_s = d$w_s,
  w_s2 = d$w_s^2,
  w_s3 = d$w_s^3,
  h = d$height
)
# Define, evaluate 6 fit
#
fml  <- file.path(getwd(), "stan", "mdl45.stan")
mdl  <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)
# Diagnostics
fit$cmdstan_diagnose()
fit$cmdstan_summary()
# Posterior samples for the MCMC approx parameters
#
samples <- fit$draws(format="df")
# Posterior line predictions
#
w_sseq <- seq(-2.2,2.2, length.out=50) 
calc_mu <- function(w_s) {
 samples$a + samples$b1*w_s + samples$b2*(w_s)^2 + samples$b3*(w_s)^3
}
mu <- sapply(w_sseq, calc_mu)
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
sim_h <- function(w_s) {
  rnorm(n=nrow(samples),
        mean=with(samples, a + b1*w_s + b2*(w_s)^2 + b3*(w_s)^3),
        sd=samples$sigma
  )
}
# Posterior predictions
#
h_tilde <- sapply(w_sseq, sim_h)
h_HPDI <- apply(h_tilde, 2, rethinking::HPDI)
# Visualize
#
plot(standardize(d$weight), d$height, col="lightgrey", xaxt="n",
xlab="weight", ylab="height")
# "Unstandardize" the predictor scale
at <- seq(-2.2,2.2, by=.4)
labels <- at * sd(d$weight) + mean(d$weight)
axis( side=1, at=at, labels=round(labels,1) )
lines(w_sseq, mu_mean)
shade(mu_HPDI, w_sseq)
shade(h_HPDI, w_sseq)

# Spline ---------------------------------------------------------------------- 

library(rethinking)
data("cherry_blossoms")
d <- cherry_blossoms
rethinking::precis(d)
# Complete case analysis
dcc <- d[complete.cases(d$doy),]
N_knots <- 15
probs <- seq(0,1, length.out=N_knots)
knots <- quantile(dcc$year, probs)

# B-Spline basis matrix (B)
#
B <- splines::bs(dcc$year, knots = knots[-c(1,N_knots)], degree = 3, 
                 intercept = TRUE)
round(B, digits=2)
# Visualize (B)
#
plot(NULL, xlim = range(dcc$year), ylim = c(0,1), 
     xlab="year", ylab="basis value" )
for(i in 1:ncol(B)) lines(dcc$year, B[,i], lwd=2)
# for(i in 1:ncol(B)) points(dcc$year, B[,i], type="l", lwd=2)
# Show the knots
text("+", x=knots, y=1)

dat_ls <- list(
  N = nrow(dcc),
  K = ncol(B), 
  B = B,
  D = dcc$doy
)

# model -------------------------------------------------------------------

file <- file.path(getwd(), "stan", "mdl46.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls, seed = 123)

fit$cmdstan_diagnose()
samples <- fit$draws(format = "df")
# bayesplot::mcmc_trace(samples)
fit$print(max_rows = 100)
bayesplot::mcmc_trace(samples)
# Seems to be approx. equal to
# m4.7 <- quap(
#   alist(
#     D ~ dnorm(mu, sigma),
#     mu <- a + B %*% w,
#     a ~ dnorm(100,10),
#     w ~ dnorm(0,10),
#     sigma ~ dexp(1)
#   ), data=list( D=dcc$doy, B = B),
#   start=list(w=rep(0, ncol(B) ) ) )
# precis(m4.7, depth = 2)

w <- fit$draws("w", format = "matrix")
w_means <- colMeans(w)
mu <- fit$draws("mu", format = "matrix")

# Visualize (B%*%w)
#
plot(NULL, xlim=range(dcc$year), ylim=c(-6,6), xlab="year", ylab="basis*weight")
for ( i in 1:ncol(B) ) lines( dcc$year, w_means[i] * B[,i], lwd=3, col="black")

# Posterior line predictions 
#
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)

# Visualize
#
plot( dcc$year, dcc$doy, xlab="year", ylab="Day in year", 
      col=col.alpha(rangi2, 0.3), pch=16 )
lines(dcc$year, mu_mean, lwd=1.5)
rethinking::shade(mu_HPDI, dcc$year, col=col.alpha("black", 0.5))


