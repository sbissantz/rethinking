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

# Prior implications (R)
#
N <- 1e6
alpha_logodds <- rnorm(N, 0, 1.5)
plot(density(alpha_logodds))
alpha_prob <- plogis(alpha_logodds)
plot(density(alpha_prob), xlim=c(0,1))

# Prior predictive simulaion (Stan)
#
path <- "~/projects/stanmisc/stan/11/"
file <- file.path(path, "pps_1.stan")
N <- length(x)
x <- d$treatment 
data_ls <- list(N=N, x=x)
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
pps <- mdl$sample(data=data_ls, fixed_param=TRUE)
samples <- pps$draws(format="df")

# Prior implications
#

# alpha (log_odds)
alpha_lo <- samples$alpha
beta_lo <- samples$beta

# alpha (prob) - R version
alpha_p <- plogis(alpha_lo)       # NEAT! 
beta_p <- plogis(beta_lo)       # NEAT! 

# General version
inv_logit <- function(x) exp(x) / (1 + exp(x))
alpha_p <- inv_logit(alpha_lo)
beta_p <- inv_logit(beta_lo)

x <- rnorm(4e3, 0,10)
plot(density(inv_logit(alpha_lo + beta_lo * x)))

# TODO: Produce these S-curves!
#






