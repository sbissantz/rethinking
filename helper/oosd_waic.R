#
# OOS Deviance
#
# The file is a mix of code from Aki Vethari & Richard McElreath!
# see: https://mc-stan.org/loo/articles/loo2-with-rstan.html

# Calculate the OOS deviance!
# ..use WAIC & start from a log-likelihood matrix (LL)
# Note: LL is actually a N x M x K array
#
# LL <- fit$draws("log_lik")
oosd_waic <- function(LL) {
  out_ls <- list()
  values <- loo::waic(LL)
  lppd <- values$pointwise[,"elpd_waic"]
  p_waic <- values$pointwise[,"p_waic"]
  # Deviance (pointwise)
  out_ls$pointwise <- -2*(lppd - p_waic)
  # Deviance (sum)
  out_ls$sum <- sum(out_ls$pointwise)
  # LL's dim: NxMxK where K equals nrow(d)
  # ..i.e., makes "n" in "function(LL,n) where "n <- nrow(d)" redundant!
  n_cases <- dim(LL)[3]
  # Standard error
  out_ls$se <- sqrt(n_cases * var(out_ls$pointwise))
  out_ls
}
# oosd_waic(LL)
