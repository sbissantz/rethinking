#
# WAIC 
#

# Preperation
#
source("prep.R")

# Samples
#
# Extract samples
samples <- fit$draws(format="data.frame")

# 1. Create lppd in R (no generated quantities in STAN)
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

# 2. Create the lppd (with generated quantities in STAN)
#

# log-likelihood matrix
log_L <- fit$draws("logprob", format="matrix")

# ..to work with Stan samples
#
n <- ncol(log_L)
ns <- nrow(log_L) 
log_sum_exp <- function (x)  { 
  xmax <- max(x)
  xsum <- sum(exp(x - xmax))
  xmax + log(xsum)
}
f <- function(i) log_sum_exp(log_L[,i]) - log(ns)
(lppd <- vapply(1:n, f, FUN.VALUE=numeric(1)))
sum(lppd)

# ------------

# Effective number of parameters
#
p_WAIC <- vapply(1:n_cases, 
                 function(i) var(log_L[,i]),
                 FUN.VALUE=numeric(1))

# TODO: Check correctness
# OOS Deviance
# ..accounted for overfitting risk
#
-2*(sum(lppd) - sum(p_WAIC))

# TODO: Check correctness
# WAIC standard error
#
n_cases <- nrow(d)
waic_vec <- -2*(lppd - p_WAIC)
sqrt(n_cases * var(waic_vec))

# -----------------------------------------------------------------------------

#
# 3a. Create a "stanfit" 
# Note: to extract_log_lik using loo elements must be names "log_lik"
# stanfit <- rstan::read_stan_csv(fit$output_files())
# LL <- loo::extract_log_lik(stanfit , merge_chains=FALSE )
#

# 3a. Create a "stanfit" 
# Note: same effect as loo::extract_log_lik()
# ..requires an array
log_L <- fit$draws("logprob")
waic_ls <- loo::waic(log_L)

# WAIC estimates (summary)
waic_ls$estimates
# WAIC (pointwise)
waic_ls$pointwise

# LOOIC (pointwise)
#
(waic <- waic_ls$pointwise[,"waic"])
# sum(looIC) yields: loo_ls$estimates "looic"

# lppd (pointwise)
(lppd <-  waic_ls$pointwise[,"elpd_waic"])
# sum(waic) yields: waic_ls$estimates "waic"

# Penalty term (effective number of parameters)
(p_waic <-  waic_ls$pointwise[,"p_waic"])
# sum(p_waic) yields: loo_ls$estimates "p_loo"

# Visualize p_waic diagnostics
plot(p_waic, pch=20) ; abline(h=0.5, lty=2)

# TODO: Check correctness
# OOS Deviance
# ..accounted for overfitting risk
#
-2*(sum(lppd) - sum(p_waic))

# TODO: Check correctness
# WAIC standard error
#
n_cases <- nrow(d)
waic_vec <- -2*(lppd - p_waic)
sqrt(n_cases * var(waic_vec))

# -----------------------------------------------------------------------------

# TODO: Big deviation between calculations! 

