#
# Overfitting 
#

# Preparation
#
source("prep.R")

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
(looIC <- loo_ls$pointwise[,"looic"])
# sum(looIC) yields: loo_ls$estimates "looic"

# lppd (pointwise)
(lppd <-  loo_ls$pointwise[,"elpd_loo"]) # sum(lppd)
# sum(lppd) yields: loo_ls$estimates "elpd_loo"

# Penalty term (McElreath: pD)
(p_loo <-  loo_ls$pointwise[,"p_loo"])
# sum(p_loo) yields: loo_ls$estimates "p_loo"

# Pareto smothed importance weights
pareto_k <- loo_ls$pointwise[,"influence_pareto_k"]
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

