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

# TODO: Delete and restart from Gabrys example
#

# Prior predictive simulaion
#
path <- "~/projects/stanmisc/stan/10/"
file <- file.path(path, "pps_1.stan")
N <- length(x)
x <- d$treatment 
data_ls <- list(N=N, x=x)
pps <- mdl$sample(data=data_ls, fixed_param=TRUE)
samples <- pps$draws(format="df")


