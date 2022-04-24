#
# PSIS
#

# Goal: estimate the out-of-sample deviance of a model.
#

source("prep.R")


#
# "Daily workflow"
#

# In a nutshell
#

LL <- fit$draws("logprob")
rel_eff <- loo::relative_eff(exp(LL))
PSIS <- loo::loo(LL, r_eff = rel_eff, is_method="psis")
lppd <- PSIS$pointwise[,"elpd_loo"]
p_psis <- PSIS$pointwise[,"p_loo"]
PSIS_i <- -2*(lppd-p_psis)
N <- dim(LL)[3]

sum(PSIS_i) ; sqrt(N * var(PSIS_i))
pareto_k <- PSIS$pointwise[,"influence_pareto_k"]
plot(pareto_k, pch=20) ; abline(h=0.5, lty=2)

#
# Documented Version
#

LL <- fit$draws("logprob")
# Note: This does actually create the scaffold of a stanfit object. So we can
# operate with the functions which use them. E.g.: loo::extract_log_lik().
# Hint: If you use loo::extract_log_lik make sure your stan model includes
# a 'vector[N] log_lik;'.

# PSIS bundle
#
# MCMC effective sample size
# exp() is mandatory: log(!)-likelihood
rel_eff <- loo::relative_eff(exp(LL))
(PSIS <- loo::loo(LL, r_eff = rel_eff, is_method="psis"))
# Note use PSIS$estimates to get the sum()-mary of the $pointwise values. 
# see: PSIS$estimates & WAIC$pointwise & apply(WAIC$pointwise, 2, sum)

# Log pointwise predictive density 

#
(lppd <-  PSIS$pointwise[,"elpd_loo"])
# Note: sum(lppd) yield the WAIC$estimate for 'elpd_loo'
# Note: -2*sum(lppd) yield the PSIS$estimate for 'looic'

# Effective number of parameters (penalty term)
#
(p_psis <-  PSIS$pointwise[,"p_loo"])
# Note: sum(p_waic) yields the WAIC$estimates "p_loo"

# OOS predictive accuracy (pointwise)
# ..corrected for the overfitting risk
#
-2*(lppd-p_psis)
# Note: ..to compare a single value
-2*(sum(lppd)-sum(p_psis))

# Approximate PSIS standard error 
#
N <- dim(LL)[3]
# The number of cases (the sample size) is the third dimension of the array.
# Note: you can also use: n_cases <- nrow(d)
PSIS_i <- -2*(lppd-p_psis)
# Approximate standard error
sqrt(N * var(PSIS_i))

# Visualize!
#

# Pareto smothed importance weights
pareto_k <- PSIS$pointwise[,"influence_pareto_k"]
# Visualize pareto k diagnostics
plot(pareto_k, pch=20) ; abline(h=0.5, lty=2)

