#
# OOS Deviance
#
# The file is a mix of code from Aki Vethari & Richard McElreath!
# see: https://mc-stan.org/loo/articles/loo2-with-rstan.html

# Calculate the OOS deviance!
# ..use PSIS & start from a log-likelihood matrix (LL)
# Note: LL is really a N x M x K array
#
# OOS
#
# LL <- fit$draws("log_lik")
oosd_psis <- function(LL) {
  out_ls <- list()
  rel_n_eff <- loo::relative_eff(exp(LL))
  values <- loo::loo(LL, r_eff=rel_n_eff, is_method="psis")
  lppd <- values$pointwise[,"elpd_loo"]
  p_waic <- values$pointwise[,"p_loo"]
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
# oosd_psis(LL)

# Note: You cannot compute competing models contrast distribution by sticking
# the difference LL1 - LL2 into the function. You have to compute it via:
# m1_vs_m2 <- oosd_psis(LL_1)$pointwise-oosd_psis(LL_2)$pointwise
# sum(m1_vs_m2) ; sqrt(n * var(out_ls$pointwise))
