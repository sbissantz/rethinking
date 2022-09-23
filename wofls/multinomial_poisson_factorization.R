#
# Multinomial as Poisson
#
library(rethinking)
data(UCBadmit)
d <- UCBadmit

# Reduction
#
dat_ls <- list(N=nrow(d), admit=d$admit, rejct = d$reject)

# Fit the model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_12.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$summary()

# Posterior
#
post <- fit$draws(format="data.frame")
# Crux: recive the probability of admission from the Poisson
calc_p <- function(x1, x2) {
  exp(x1) / (exp(x1) + exp(x2))
}
(p_admit <- calc_p(post$alpha_1, post$alpha_2))
mean(p_admit)
