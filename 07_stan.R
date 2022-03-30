#
# Overfitting 
#

# Type in data
#
sppnames <- c("afarensis", "africanus", "habilits", "boisei", "rudolfensis",
              "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species=sppnames, brain=brainvolcc, mass=masskg)

# Visualize
#
plot(d$mass, d$brain, ylab="brain volume (cc)", xlab="body mass (kg)", pch=20)
text(x=masskg, y=brainvolcc+50, sppnames)

# Rescale variables
#
# Standardize
d$M <- with(d, (mass - mean(mass))/sd(mass))
# Normalize
d$B <- with(d, brain/max(brain))

# Model sketch
# (1)
# B_i ~ Normal(mu_i, sigma)
# mu_i = alpha + beta_M * M_i
# alpha ~ normal(0,0.1)
# beta_M ~ normal(0,5)
# sigma ~ exponential(1)

# Reduction
#
dat_ls <- list(N=nrow(d), B=d$B, M=d$M)
 
# Fit the model
#
file <- file.path(getwd(), "stan", "7", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# STAN
# for (i in 1:N) {
#  logprob[i] = normal_lpdf(B[i] | alpha + beta_M * M[i], sigma); 
# }
samples <- fit$draws(format="df")  

# normal_lpdf <- function(x, mean, sd) {
#  sum(dnorm(x,mean,sd,log=TRUE))
# } 

N <- nrow(d) 
log_lik <- vector(length = N)
for (i in 1:nrow(d)) {
  log_lik[i] <- with(samples, {
    mean(dnorm(alpha + beta_M * d$M[i], sigma,log=TRUE))
    })
}
log_lik


rethinking::lppd(fit)
  

# Samples
#
samples <- fit$draws(format="data.frame")

# Calculate & Simulate 
#
N <- nrow(samples)
x_seq <- seq(-3,3, length.out=N)

calc_mu <- function(M) {
    with(samples, alpha + beta_M*M)
}
sim_B <- function(M, N){
    with(samples, abs(rnorm(N, mean=alpha + beta_M*M, sd=sigma)))
}
x_seq <- seq(-3,3, length.out=N)
B_tilde <- vapply(x_seq, sim_B, FUN.VALUE=numeric(N), N=N)
mu <- vapply(x_seq, calc_mu, FUN.VALUE=numeric(N))
mu_mean <- colMeans(mu)

# Visualize
#
plot(d$B ~ d$M, pch=20, col=scales::alpha("black", .3))
for(i in 1:1e2) lines(x_seq, B_tilde[i,], col=scales::alpha("steelblue", .1))
for(i in 1:1e2) lines(x_seq, mu[i,], col=scales::alpha("white", .3), lwd=2)
lines(x_seq, mu_mean, lwd=3)


# Entropy & accuracy 
#
H <- function(p) {
    -sum(p * log(p))
}
p <- c(0.5, 0.5)
# Shannon entropy
H(p)

# KL-divergence
#
D_KL <- function(p, q) {
    sum(p * log(p/q))
}
p <- c(0.5, 0.5) ; q <- c(0.4, 0.6)
D_KL(p,q)

lps <- function(q) sum(log(q))

# lppd
rethinking::lppd

# lppd

# 1. Scenario: calculate the lp within STAN and use loo 
# See: https://mc-stan.org/loo/articles/loo2-with-rstan.html
loo::waic(log_lik)

# 2a. Scenario: calculate the lp with STAN and use rethinking
n <- ncol(logprob)
ns <- nrow(logprob) 
log_sum_exp <- function (x)  { 
  xmax <- max(x)
  xsum <- sum(exp(x - xmax))
  xmax + log(xsum)
}
f <- function(i) log_sum_exp(logprob[,i]) - log(ns)
lppd <- sapply(1:n, f)
sum(lppd)

# 2b.









