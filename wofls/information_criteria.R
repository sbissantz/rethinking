#
# WAIC 
#

# Goal: estimate the out-of-sample deviance of a model.
#

source("prep.R")


#
# "Daily workflow"
#

#
# In a nutshell
LL <- fit$draws("logprob")
WAIC <- loo::waic(LL)
lppd <-  WAIC$pointwise[,"elpd_waic"]
p_waic <-  WAIC$pointwise[,"p_waic"]
WAIC_i <- -2*(lppd-p_waic)
N <- dim(LL)[3]

sum(WAIC_i) ; sqrt(N * var(WAIC_i))
plot(p_waic, pch=20) ; abline(h=0.5, lty=2)


#
# Documented Version
#


# MCMC log-likelihood array
#
LL <- fit$draws("logprob")
# Note: This does actually create the scaffold of a stanfit object. So we can
# operate with the functions which use them. E.g.: loo::extract_log_lik().
# Hint: If you use loo::extract_log_lik make sure your stan model includes
# a 'vector[N] log_lik;'.

# WAIC bundle
#
(WAIC <- loo::waic(LL))
# Note use WAIC$estimates to get the sum()-mary of the $pointwise values. 
# see: WAIC$estimates & WAIC$pointwise & apply(WAIC$pointwise, 2, sum)

# Log pointwise predictive density 
#
(lppd <-  WAIC$pointwise[,"elpd_waic"])
# Note: sum(lppd) yield the WAIC$estimate for 'elpd_waic'
# Note: -2*sum(lppd) yield the WAIC$estimate for 'waic'

# Effective number of parameters (penalty term)
#
(p_waic <-  WAIC$pointwise[,"p_waic"])
# Note: sum(p_waic) yields the WAIC$estimates "p_loo"

# OOS predictive accuracy (pointwise)
# ..corrected for the overfitting risk
#
-2*(lppd-p_waic)
# Note: ..to compare a single value
-2*(sum(lppd)-sum(p_waic))

# Approximate WAIC standard error 
#
N <- dim(LL)[3]
# The number of cases (the sample size) is the third dimension of the array.
# Note: you can also use: n_cases <- nrow(d)
WAIC_i <- -2*(lppd-p_waic)
# Approximate standard error
sqrt(N * var(WAIC_i))

# Visualize!
#

# p_waic diagnostics
plot(p_waic, pch=20) ; abline(h=0.5, lty=2)


#
# Handcrafted!
#


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

# ..for efficiency (see: rethinking package!) 
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

# Note: Big deviation between calculations! 
# TODO: Search for possible mistakes.
