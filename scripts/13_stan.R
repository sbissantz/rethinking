#
# Chapter 13: Multilevel Models
#

# Load packages
library(rethinking)
library(cmdstanr)

# Load data
data(reedfrogs)

# Abbreviate data
d <- reedfrogs  

# Show data
str(d)
#View(d)

# Note: Each row is a tank, and in the first one there were 10 tadpoles
# (density), 9 survived (surv),

# Goal: Modeling the survival of tadpoles with a varying intercept model
# outcome: surv (S)

# No-pooling model with anterogate amnesia
# Model sketch:
#
# S_i ~ Binomial(N_i, p_i) 
# Binomial, because we have different numbers of tadpoles in each tank
# Important not to use proportions, to keep the sample size information
#
# logit(p_i) = alpha_[tank[i]]
# alpha_j ~ Normal(0, 1.5), j = 1, ..., 48

# Data processing
#
# Each row is a tank, so can use the row number
n_tanks <- nrow(d)
# Tank ID (1, ..., 48), since 
d$tank <- seq(n_tanks)
# Data list 
dat_ls <- list("n_tanks" = n_tanks, "S"=d$surv, "N"=d$density, "T"=d$tank)

# TODO: Prior predictive checks

# Fit the first model (no pooling)
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "1.stan")
mdl1 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit1 <- mdl1$sample(data=dat_ls)
# Note that the alpha parameters are on the log-odds scale!
fit1$print(max_rows=150)

# Diagnostics
#
fit1$sampler_diagnostics()
fit1$cmdstan_diagnose()
fit1$diagnostic_summary()
# fit1_stan <- rstan::read_stan_csv(fit1$output_files())
# rethinking::trankplot(fit1_stan)

# Extract posterior draws
post1 <- fit1$draws(format="matrix")
# Extract the log likelihood matrix
LL1 <- fit1$draws("log_lik")
# Relative effective sample size
reff1 <- loo::relative_eff(exp(LL1))
loo1 <- loo::loo(LL1, r_eff=reff1)
# PSIS
psis1 <- loo::loo(LL1, r_eff=reff1, is_method="psis")
pareto_k1 <- psis1$diagnostics$pareto_k

# Fit the second model (partial pooling)
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "2.stan")
mdl2 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit2 <- mdl1$sample(data=dat_ls)
# Note that the alpha parameters are on the log-odds scale!
fit2$print(max_rows=150)

# Diagnostics
#
fit2$sampler_diagnostics()
fit2$cmdstan_diagnose()
fit2$diagnostic_summary()
# fit2_stan <- rstan::read_stan_csv(fit1$output_files())
# rethinking::trankplot(fit1_stan)

# Extract posterior draws
post2 <- fit2$draws(format="matrix")
# Extract the log likelihood matrix
LL2 <- fit2$draws("log_lik")
# Relative effective sample size
reff2 <- loo::relative_eff(exp(LL2))
loo2 <- loo::loo(LL2, r_eff=reff2)
# PSIS
psis2 <- loo::loo(LL2, r_eff=reff2, is_method="psis")
pareto_k2 <- psis2$diagnostics$pareto_k

# Model comparison
#
comp <- loo::loo_compare(loo1, loo2)
print(comp, simplify=FALSE)


# TODO: Prior predictive simulation
# TODO: Posterior predictive checking 
