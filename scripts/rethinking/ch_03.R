#
#
# Sampling the Imaginary
#
#

#
# Sampling from a grid-approximate posterior
# 

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior_ust <- prior * likelihood
posterior <- posterior_ust/sum(posterior_ust)
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)

#
# Intervals of defined boundaries (looking for probability /mass/)
#

# Q: What is the posterior probability that the proportion of water is
# less than 0.5? 

# Inference from "full posterior"

sum(posterior[ p_grid < 0.5 ])
# [1] 0.1718746
# Answer: About 17% of the posterior probability is below 0.5.

# Inference fromrandom samples from the posterior

no_samples <- 1e4
sum(samples < 0.5) / no_samples
# [1] 0.1729
# Answer: About 17% of the posterior probability is below 0.5.

# Q: What is the posterior probability that the proportion of water is
# between 0.5 and 0.75? 

# Inference from samples form the posterior 
no_samples <- 1e4
sum( samples > 0.5 & samples < 0.75 ) / no_samples
# [1] 0.6053
# A: About 61% of the posterior probability is between "0.75" and "0.5".


#
# Intervals of defined mass (looking for /values/)
#

# Q: Where lies the boundary of the lower 80% posterior probability? Task:
# Find the value of 80% quantile value.

quantile( samples , .8 )
#       80% 
# 0.7597598 
# 80% Posterior probability exists below a parameter value of about .75

# Q: What are the boundaries of the middle 80% of the posterior
# probability?

# Find the value of the 10% and 90% quantile
quantile( samples , c( 0.1, 0.9 ) )
#       10%       90% 
# 0.4474474 0.8128128 

#
#  The problem of percentile intervals
# 

no <- 1000
p_grid <- seq(0, 1, length.out = no)
prior <- rep(1, no)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior_ust <- prior * likelihood
posterior <- posterior_ust / sum(posterior_ust)
samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE )

# Q: What is the 50% compatible interval? TasK: Try to find the values
# at the 75% - 25% boundary. this interval includes 50% of the probability mass.

require(rethinking)
PI( samples, prob=.5 )
#       25%       75% 
# 0.7117117 0.9339339 

# Problem: This intervall omits much of the most probable/plausible parameter
# values for p near p = 1. Therefore we use the 50% HPDI.

#
# Highest Posterior Density Interval (HPDI)
#

# The HPDI is the narrowest interval that is consistent with the data.  The
# interval captures the parameter with the highest posterior probability. Here:
# the narrowest region with the 50% of the Posterior probability. 

require(rethinking)
HPDI( samples, prob = .5 )
#      |0.5      0.5| 
# 0.8418418 1.0000000 

# Note: HPDI's are sensible to the n° posterior samples & not widely used in the 
# non-bayesian world.


#
# Point estimates 
#

# The point estimate is literaly a one-point summary of the Posterior (compared
# to an intervall, a two-point summary). It is really not useful, because, in
# comparison, suspress info. But if its mandatory: use the MAP

#
# Maximum a posterior estimate (MAP) -- Posterior Mode
#

# Map from full posterior
p_grid[which.max(posterior)]
# [1] 1

# Map from samples

require(rethinking)
chainmode(samples, adj=.1)
# [1] 0.9932861

# Posterior mean 

mean(samples)
# [1] 0.801149

# Posterior median

median(samples)
# [1] 0.8418418

#
# A loss function (choosing between estimates)
#

# Calculating the loss for a decision means using the posterior to average over
# our uncertinty in the true value. Therefore we use the whole model info (about
# p) -- the posterior. Betting on the true value "p = .5", the weighted average
# loss is:

sum(posterior * abs(.5 - p_grid))
# [1] 0.3128752

# Apply this to all assumptions (p) and find the minimum of that result. Hint: It
# turns out that this is the posterior median (0.84).

measure_distance <- function(d) {
    sum(posterior * abs(d - p_grid))
}
loss <- sapply(p_grid, measure_distance)
# Find the mnimum value
p_grid[which.min(loss)]
# [1] 0.8408408

# Conclusion: If you can, report as much of the posterior as you can, i.e. leave
# behind point estimates. If not using any of these values should cover a
# weighting of the relevant costs and benefits of such a choice.


#
# Sampling to simulate predictions
#


# Dummy data from a binomial LL

# In the globe tossing example the dummy data arise from a binomial likelihood.
# For N=2 tosses der are 3 outcomes {0, 1, 2 (W)}, so the probability for each
# is:

possible_out <- c(0,1,2) 
n_toss <- 2
dbinom(possible_out, size=n_toss, prob = .7)
# [1] 0.09 0.42 0.49

# There is 9% chance of observing w=0, 42% for w=1, 49% for w=2. Now simulate
# observations (1 random dummy datum) using these probabilities:


n_dummy <- 1
n_toss <- 2
# dummy datum
rbinom(n_dummy, n_toss, prob = .7 )
# [1] 1

This mean 1 Water in two tosses. Generate 10 dummy data or simulations from the
binomial likelihood:

n_dummy <- 10
n_tosses <- 2
# dummy data
rbinom(n_dummy, n_tosses, prob = .7)
#  [1] 1 2 2 0 1 2 1 1 2 2

# Now we show that each value {0,1,2} occures in proportion to its likelihood:

n_dummy <- 1e5
n_tosses <- 2
dummy_sims <- rbinom(n_dummy, n_tosses, prob = .7)

# Graphical output
plot(table(dummy_sims))

# "Proportionalize"
table(dummy_sims) / n_dummy
# dummy_sims
#       0       1       2 
# 0.09223 0.41884 0.48893 

# Now increase the n° tosses, e.g. N=9 and plot the distribution of simulated
# sample observations from 9 tosses of the globe. This samples assumes that
# either .7 is the true proportion of water. 

dum_no <- 1e5 ; tos_no <- 10
dummy <- rbinom(dum_no, tos_no, prob = .5)
plot(table(dummy))

# Hint: In non-bayesian methods inference (about p) is made through the sampling
# distribution. In Bayesian methods the posterior is not samples (1) it is
# deduced logically and then (2) samples can be drawn to aid inference.

#
# Model checking
#

# Since bayesian models are generative we can simulate observations to examine
# the models empirical expectations. We use a posterior predictive distribution.
# To simulate predictions for a single of p, e.g.: 0.6. we simulazr 10.000
# predictions of 9 globe tosses, assuming p = .6.

w <- rbinom(1e4, size = 9, prob = .6)
plot(table(w))

# Now we propagate parameter uncertainty into these predictions. Therefore we
# replace .6 with the samples from the posterior. Since the sampled values appear
# in their in proportion to their posterior probabiities. the resulting simulated
# observations are averaged over the posterior

w <- rbinom(1e4, size = 9, prob = samples)
plot(table(w))

# Function to build sampling distributions
distributer <- function(pr) {
    dum_no <- 1e5 ; tos_no <- 10
    dummy <- rbinom(dum_no, tos_no, prob = pr)
    plot(table(dummy), xlab = "Probability of Water")
    title(main = paste("Prob of water = ", pr))
}

sqs <- seq(from = 0.1, to = 0.9, by = 0.1)
old_par <- par(no.readonly = TRUE)
par(mfrow = c(3,3))
lapply(sqs, distributer)
par(old_par)


