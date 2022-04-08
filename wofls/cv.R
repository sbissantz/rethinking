#
# Overfitting 
#

# Data
#
source("prep.R")

# Modifications 
#
# Standardize
d$M <- with(d, (mass - mean(mass))/sd(mass))
# Normalize
d$B <- with(d, brain/max(brain))

# Reduction
#
dat_ls <- list(N=nrow(d), B=d$B, M=d$M)

# Model sketch
#
# B_i ~ Normal(mu_i, sigma)
# mu_i = alpha + beta_M * M_i
# alpha ~ normal(0,0.1)
# beta_M ~ normal(0,5)
# sigma ~ exponential(1)
path <- "~/projects/stanmisc/wofls/"
file <- file.path(path, "model.stan")

# Fitting
#
# Compile the Stan program
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
# Fit the model
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
# Extract samples
samples <- fit$draws(format="data.frame")
#
# 1. Create a stanfit object
# Note: to extract_log_lik using loo elements must be names "log_lik"
# stanfit <- rstan::read_stan_csv(fit$output_files())
# LL <- loo::extract_log_lik(stanfit , merge_chains=FALSE )
#
# 2. DIY: extract the samples and put them in an array (default) 
# log-likelihood matrix
log_L <- fit$draws("logprob")

# CV (LOOCV)
# ..to hard to implement! 
# ..use loo (Vethari) 
#
# Get the effective sample size
rel_n_eff <- loo::relative_eff(exp(log_L))

# LOOCV -- using PSIS
(loo_ls <- loo::loo( log_L, r_eff = rel_n_eff, is_method="psis"))

# See: str(loo_ls)
#

# LOO (summary)
loo_ls$estimates
# LOO (pointwise)
loo_ls$pointwise

# LOOIC (pointwise)
#
(looIC <- loo_ls$pointwise[,4])
# sum(looIC) yields: loo_ls$estimates "looic"

# lppd (pointwise)
(lppd <-  loo_ls$pointwise[,1]) # sum(lppd)
# sum(lppd) yields: loo_ls$estimates "elpd_loo"

# Penalty term (McElreath: pD)
(p_loo <-  loo_ls$pointwise[,3])
# sum(p_loo) yields: loo_ls$estimates "p_loo"

# Pareto smothed importance weights
pareto_k <- loo_ls$pointwise[,5]
# Visualize pareto k diagnostics
plot(pareto_k, pch=20) ; abline(h=0.5, lty=2)

# TODO: Check correctness
# OOS Deviance
# ..accounted for overfitting risk
#
-2*(sum(lppd) - sum(p_loo))

# TODO: Check correctness
# PSIS standard error
#
n_cases <- nrow(d)
cv_vec <- -2*(lppd - p_loo)
sqrt(n_cases * var(cv_vec))

