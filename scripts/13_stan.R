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
# Each row is a tank, so can use the row number
n_tanks <- nrow(d)
# Tank ID (1, ..., 48), since 
d$tank <- seq(n_tanks)
# Data list 
dat_ls <- list("n_tanks" = n_tanks, "S"=d$surv, "N"=d$density, "T"=d$tank)

# Fit the model
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
# TODO: Make probability scale in Stan

