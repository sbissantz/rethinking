#
# WAIC 
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

path <- "/home/steven/projects/stanmisc"
file <- file.path(path, "stan", "7", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()


# Samples
#
samples <- fit$draws(format="data.frame")

stanfit <- rstan::read_stan_csv(fit$output_files())
log_lik <- fit$draws("logprob")


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

# Cross entropy
#
H2 <- function(p, q) {
  -sum(p*log(q))
}
# Cross entropy
H2(p,q) 

# Test
#
# cross entropy - shanonen entropy = KL divergence
all.equal(H2(p,q) - H(p), D_KL(p,q))
# [1] TRUE

# Log-probability score
# 
S <- function(q) sum(log(q))
# Log-probability score
S(q)

# Bayesian log-score (lppd)
# ..to work with Stan samples
#
n <- ncol(logprob)
ns <- nrow(logprob) 
log_sum_exp <- function (x)  { 
  xmax <- max(x)
  xsum <- sum(exp(x - xmax))
  xmax + log(xsum)
}
f <- function(i) log_sum_exp(logprob[,i]) - log(ns)
(lppd <- sapply(1:n, f))
sum(lppd)

# Bayesian log-score (lppd)
# ...pure R version (no Stan)
#
n_samples <- 1e3
n_cases <- nrow(d)

logprob <- vapply(seq(n_samples), 
                  function(s) { 
                    mu <- samples$alpha[s] + samples$beta_M[s] * d$M
                    dnorm(d$M, mu, samples$sigma[s], log=TRUE)
                  }, FUN.VALUE=numeric(n_cases))

# ..for efficiency 
#
log_sum_exp <- function (x)  { 
  xmax <- max(x)
  xsum <- sum(exp(x - xmax))
  xmax + log(xsum)
}

lppd <- vapply(seq(n_cases), 
               function(i) log_sum_exp(logprob[i,] - log(n_samples)),
               FUN.VALUE=numeric(1))

# Effective number of parameters
#
p_WAIC <- vapply(1:n_cases, 
                 function(i) var(logprob[i,]),
                 FUN.VALUE=numeric(1))

# Deviance
#
-2*(sum(lppd) - sum(p_WAIC))

# WAIC standard error
#
waic_vec <- -2* (lppd - p_WAIC)
sqrt(n_cases * var(waic_vec))

# CV (LOOCV)
# ..to hard to implement it! 
# ..use Aki Vethari's package loo & Stan
#

# Note: PSIS works only over loo
#
ll_matrix <- loo::extract_log_lik(stanfit , merge_chains=FALSE )


rel_n_eff <- loo::relative_eff(exp(log_lik))

rel_n_eff <- loo::relative_eff(exp(ll_matrix))
loo_list <- suppressWarnings( loo::loo( ll_matrix , r_eff = rel_n_eff ) )
looIC <- as.vector( loo_list$pointwise[,4] )
lppd <- as.vector( loo_list$pointwise[,1] )
pD <- as.vector( loo_list$pointwise[,3] )







