#
# OOS Deviance
#
# The file is a mix of code from Aki Vethari & Richard McElreath!
# see: https://mc-stan.org/loo/articles/loo2-with-rstan.html

# Calculate the OOS deviance!
# ..use WAIC & start from a log-likelihood matrix (LL)
#
# LL <- fit$draws("log_lik")
# n_cases <- nrow(d)
oosd_waic <- function(LL, n) {
  out_ls <- list()
  values <- loo::waic(LL)
  lppd <- values$pointwise[,"elpd_waic"]
  p_waic <- values$pointwise[,"p_waic"]
  # Deviance (pointwise)
  out_ls$pointwise <- -2*(lppd - p_waic)
  # Deviance (sum)
  out_ls$sum <- sum(out_ls$pointwise)
  # Standard error
  out_ls$se <- sqrt(n_cases * var(out_ls$pointwise))
  out_ls
}
# oosd_waic(LL)

# Calculate the OOS deviance!
# ..use PSIS & start from a log-likelihood matrix (LL)
#
# OOS
#
# LL <- fit$draws("log_lik")
# n_cases <- nrow(d)
oosd_psis <- function(LL, n) {
  out_ls <- list()
  rel_n_eff <- loo::relative_eff(exp(LL))
  values <- loo::loo(LL, r_eff=rel_n_eff, is_method="psis")
  lppd <- values$pointwise[,"elpd_loo"]
  p_waic <- values$pointwise[,"p_loo"]
  # Deviance (pointwise)
  out_ls$pointwise <- -2*(lppd - p_waic)
  # Deviance (sum)
  out_ls$sum <- sum(out_ls$pointwise)
  # Standard error
  out_ls$se <- sqrt(n * var(out_ls$pointwise))
  out_ls
}
# oosd_psis(LL)

# Note: Compute the contrast distribution between competing models using:
# delta_oospd <- oospd_1$pointwise oospd  oospd_2$pointwise
# sqrt(n_cases * var(out_ls$pointwise))
#
#...or stick the differences in LL into the function!
# TODO: LL_1 - LL2 is different from LL2 - LL1
