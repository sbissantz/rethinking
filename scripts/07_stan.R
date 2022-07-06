
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

path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "7", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()
stanfit <- rstan::read_stan_csv(fit$output_files())
rethinking::trankplot(stanfit)

# Samples
#
samples <- fit$draws(format="data.frame")
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

#
# 7.3 Model comparison
#

# Simulating Post-treatment Bias
#
# Number of plants
N <- 100
# Initial heights
h0 <- rnorm(N,10,2)
# Treatments
tx <- rep( 0:1, each=N/2 )
# Fungus & growth
p_fungus <- .5-tx*0.4
fungus <- rbinom(N,1, prob=p_fungus)
# Final heights
h1 <- h0 + rnorm(N,5-3*fungus)
d <- data.frame(h0=h0, tx=tx, fungus=fungus, h1=h1)
# Reduced data list!
dat_ls <- list(N=nrow(d), h0=d$h0, h1=d$h1)

#
# Refit M6.6
#

path <- "/home/steven/projects/stanmisc"
path <- file.path(getwd(), "stanmisc")
# Fitting!
file <- file.path(path, "stan", "6", "6.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Log-Likelihood
#
log_L <- fit$draws("log_lik")
# Effective sample size!
rel_n_eff <- loo::relative_eff(exp(log_L))
head(rel_n_eff)
# PSIS
(loo_ls_1 <- loo::loo(log_L, r_eff = rel_n_eff, is_method="psis"))
(waic_ls_1 <- loo::waic(log_L))

#
# Refit M6.7
#

# Reduced data list
dat_ls <- list(N=nrow(d), h0=d$h0, h1=d$h1, F=d$fungus, T=d$tx)
path <- "/home/steven/projects/stanmisc"
path <- file.path(getwd(), "stanmisc")
# Fitting
file <- file.path(path, "stan", "6", "7.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Log-Likelihood
#
log_L <- fit$draws("log_lik")
# Effective sample size!
rel_n_eff <- loo::relative_eff(exp(log_L))
# PSIS
(loo_ls_2 <- loo::loo(log_L, r_eff = rel_n_eff, is_method="psis"))
(waic_ls_2 <- loo::waic(log_L))

#
# Refit M6.8
#

# Fitting!
path <- "/home/steven/projects/stanmisc"
path <- file.path(getwd(), "stanmisc")
file <- file.path(path, "stan", "6", "8.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Log-Likelihood
#
log_L <- fit$draws("log_lik")
# Effective sample size!
rel_n_eff <- loo::relative_eff(exp(log_L))
# PSIS
(loo_ls_3 <- loo::loo(log_L, r_eff = rel_n_eff, is_method="psis"))
(waic_ls_3 <- loo::waic(log_L))

#
# Model comparison!
#
# PSIS
loo::loo_compare(loo_ls_1, loo_ls_2, loo_ls_3)
# OOS elpd contras between mdl 3 and 1
# (Note: Sd not shown in the output above)
loo::loo_compare(loo_ls_3, loo_ls_1)
# WAIC
loo::loo_compare(waic_ls_1, waic_ls_2, waic_ls_3)
# OOS elpd contras between mdl 3 and 1
# (Note: Sd not shown in the output above)
loo::loo_compare(waic_ls_1, waic_ls_2)

# Note: loo compare compares the elpd differences
#

# Differences in waic (waic=-2xelpd_waic)
#
waic_diff_12 <- waic_ls_1$pointwise - waic_ls_2$pointwise
n <- nrow(waic_diff_12)
mean(waic_diff_12[,"waic"])
(se_waic_diff_12 <- sqrt(n*var(waic_diff_12[,"waic"])))

# Differences in looic (looic=-2xelpd_loo)
#
(loo_diff_12 <- loo_ls_1$pointwise - loo_ls_2$pointwise)
n <- nrow(loo_diff_12)
mean(loo_diff_12[,"looic"])
(se_loo_diff_12 <- sqrt(n*var(loo_diff_12[,"looic"])))

# Mc Elreath Version
#
# -2(lppd - p_waic)
lppd <- waic_ls_1$pointwise[,"elpd_waic"]
p_waic <- waic_ls_1$pointwise[,"p_waic"]
D <- -2*(lppd - p_waic)
sum(D) ; sqrt(n*var(D))
# The differences in OOS Deviances
# -2(lppd - p_waic)
lppd_2 <- waic_ls_2$pointwise[,"elpd_waic"]
lppd_3 <- waic_ls_3$pointwise[,"elpd_waic"]
p_waic_2 <- waic_ls_2$pointwise[,"p_waic"]
p_waic_3 <- waic_ls_3$pointwise[,"p_waic"]
D_2 <- -2*(lppd_2 - p_waic_2)
D_3 <- -2*(lppd_3 - p_waic_3)
D_diff <- D_2 - D_3
sum(D_diff) ; sqrt(n*var(D_diff))

#
# Finding outliers
#

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$A <- scale( d$MedianAgeMarriage )
d$D <- scale( d$Divorce )
d$M <- scale( d$Marriage )

dat_ls <- list(N=nrow(d), A=as.numeric(d$A), D=as.numeric(d$D),
               M=as.numeric(d$M))

#
# Refit M5.1
#
path <- file.path(getwd(), "stanmisc")
path <- "/home/steven/projects/stanmisc"
file <- file.path(path, "stan", "5", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Sampling
#
log_L <- fit$draws("log_lik")

# PSIS & WAIC
#
rel_n_eff <- loo::relative_eff(exp(log_L))
loo_ls_1 <- loo::loo(log_L, r_eff=rel_n_eff, is_method="psis")
waic_ls_1 <- loo::waic(log_L)

# Visualize
#
plot(loo_ls_1$pointwise[,"influence_pareto_k"],
     waic_ls_1$pointwise[,"p_waic"], pch=20, col="steelblue")
abline(h=0.5, lty=2)

# D_oos
#
lppd <- loo_ls_1$pointwise[,"elpd_loo"]
p_loo <- loo_ls_1$pointwise[,"p_loo"]
OOSD <- -2*(lppd - p_loo)
sum(OOSD)
n_cases <- nrow(d)
# Standard error
sqrt(n_cases * var(OOSD))

#
# Refit M5.2
#
path <- file.path(getwd(), "stanmisc")
path <- "/home/steven/projects/stanmisc"
file <- file.path(path, "stan", "5", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Sampling
#
log_L <- fit$draws("log_lik")

# PSIS & WAIC
#
rel_n_eff <- loo::relative_eff(exp(log_L))
loo_ls_2 <- loo::loo(log_L, r_eff=rel_n_eff, is_method="psis")
waic_ls_2 <- loo::waic(log_L)

# Visualize
#
plot(loo_ls_2$pointwise[,"influence_pareto_k"],
     waic_ls_2$pointwise[,"p_waic"], pch=20, col="steelblue")
abline(v=0.5, lty=2)

# D_oos
#
lppd <- loo_ls_2$pointwise[,"elpd_loo"]
p_loo <- loo_ls_2$pointwise[,"p_loo"]
OOSD <- -2*(lppd - p_loo)
sum(OOSD)
n_cases <- nrow(d)
# Standard error
sqrt(n_cases * var(OOSD))

#
# Refit M5.3
#
dat_ls <- list(N=nrow(d), K=2, D=as.numeric(d$D), X=cbind(as.numeric(d$A), M=as.numeric(d$M)))
path <- "/home/steven/projects/stanmisc"
file <- file.path(path, "stan", "5", "3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Sampling
#
log_L <- fit$draws("log_lik")

# PSIS & WAIC
#
rel_n_eff <- loo::relative_eff(exp(log_L))
loo_ls_3 <- loo::loo(log_L, r_eff=rel_n_eff, is_method="psis")
waic_ls_3 <- loo::waic(log_L)

# Visualize
#
plot(loo_ls_3$pointwise[,"influence_pareto_k"],
     waic_ls_3$pointwise[,"p_waic"], pch=20, col="steelblue")
abline(h=0.5, lty=2)

# D_oos
#
lppd <- loo_ls_3$pointwise[,"elpd_loo"]
p_loo <- loo_ls_3$pointwise[,"p_loo"]
OOSD <- -2*(lppd - p_loo)
sum(OOSD)
n_cases <- nrow(d)
# Standard error
sqrt(n_cases * var(OOSD))

#
# Compare!
#
loo::loo_compare(loo_ls_1, loo_ls_2, loo_ls_3)

waic_diff_12 <- waic_ls_1$pointwise - waic_ls_2$pointwise
n <- nrow(waic_diff_12)
sum(waic_diff_12[,"waic"])
(se_waic_diff_12 <- sqrt(n*var(waic_diff_12[,"waic"])))

#
# Refit 5.3 using a Student's t as data distribution
#
path <- "/home/steven/projects/stanmisc"
# Fitting!
file <- file.path(path, "stan", "5", "3t.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Log-Likelihood
#
log_L <- fit$draws("log_lik")
# Effective sample size!
rel_n_eff <- loo::relative_eff(exp(log_L))
# PSIS
(loo_ls_t <- loo::loo(log_L, r_eff = rel_n_eff, is_method="psis"))
(waic_ls_t <- loo::waic(log_L))
