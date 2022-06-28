#
# Ch. 11: Good spiked the integer
#

# Logistic regression
#
library(rethinking)
data(chimpanzees)
d <- chimpanzees
# Create a treatment variable
d$treatment <- 1 + d$prosoc_left + 2*d$condition

# Model
# L_i ~ Bernoulli(p)
# logit(p_i) = alpha[actor_i] + beta[treatment_i]
# alpha_j ~ ?
# beta_k ~ ?

# Prior predictive simulaion
#
path <- "~/projects/stanmisc/stan/10/"
file <- file.path(path, "pps_1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
N <- 1e3
x <- seq(0, 100)
data_ls <- list(N=1e3, x=x)
pps <- mdl$sample(data=data_ls, fixed_param=TRUE)


