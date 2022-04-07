#
#
# Small world and Large Worlds 
#
#

# Especially the development branch from Richards repo.

install.packages(c("coda","mvtnorm","devtools"))
library(devtools)
devtools::install_github("rmcelreath/rethinking",ref="Experimental")}

ways <- c(0, 3, 8, 9, 0)
sum_ways <- sum(ways)
print(plausibility <- ways/sum(ways))

# Think & formualte the model

W_no <- 6
toss_no <- 9
dbinom(W_no, size = toss_no, prob = .5)

# Given the data .16 is the relative number of ways to get six “W” out of 9
# independent tosses.


#### Binomial distribution

# Chance to get 6 heads in 9 independent tosses with a fair coin?

heads <- 6
toss_no <- 9
dbinom(6, 9, prob=.5)
# [1] 0.1640625

# Chance of a basketball player to score 10 times in 20 tosses, with a
# probability to succed of .4 in 1 trial

shots <- 10
toss_no <- 20
dbinom(10, 20, prob=.4)
# [1] 0.1171416

#
# Grid approximation engine
#

old_par <- par(no.readonly=TRUE)

grid_approx <- function(m){
    # define the refinement of grid
    p_grid <- seq( from=0 , to=1 , length.out=m )
    # define prior
    prior <- rep( 1 , m )
    # compute likelihood at each value in grid
    ikelihood <- dbinom( 6 , size=9 , prob=p_grid )
    # compute product of likelihood and prior
    unstd.posterior <- likelihood * prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior / sum(unstd.posterior)
    plot( p_grid , posterior , type="b" ,
         xlab="probability of water" , ylab="posterior probability" )
    mtext( paste0(m, "points") )
}

par(mfrow = c(2,2))
    series <- c(10, 50, 100, 1000) 
    lapply(series, FUN = grid_approx)
par(old_par)

# Hint: to see what do we mean by saying "defining a data raster" is the following 

# The grid
p_grid <- seq(0, 1, length.out = 1000)
hist(p_grid)

#
# Grid approximation ~ Priors
#

old_par <- par(no.readonly=TRUE)
grid_approx <- function(prior_frml, desc = as.character()) {
    # define grid
    p_grid <- seq( from=0 , to=1 , length.out=100 )
    prior <- eval(prior_frml)
    # compute likelihood at each value in grid
    likelihood <- dbinom( 6 , size=9 , prob=p_grid )
    # compute product of likelihood and prior
    unstd.posterior <- likelihood * prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior / sum(unstd.posterior)
    plot( p_grid , posterior , type="b" ,
         xlab="probability of water" , ylab="posterior probability" )
    mtext(paste0(desc))

}

par(mfrow = c(2,1))
    unif_prior <- quote(ifelse(p_grid < .5, 0, 1))
    grid_approx(unif_prior, desc = "uniform or flat prior")

    exp_prior <- quote(exp(-5 * abs(p_grid - .5)))
    grid_approx(exp_prior, desc = "exponential prior")
par(old_par)

#
# Quadratic approximation engine
#

library(rethinking)

globe.qa <- quap(alist(W ~ dbinom(W+L, p), # binomial Likelihood 
                       p ~ dunif(0,1) # uniform prior), 
                       data = list(W=6, L=3))
# displaysummary of quadratic approximation
precis(globe.qa)
# /Assuming the posterior is Gaussian/, it is maximize at 0.67 (posterior mean),
# and its (local, posterior) standard deviation is .16.

#
# Quadratic approximation ~ sample size
#

# Show that the quadratic approximation improves with the amount of data

W <- 6
L <- 3
curve(dbeta(x, W+1, L+1), from=0, to=1,
      xlab = "Proportion of Water", ylab = "Density")
# quadratic approx
curve(dnorm(x, .67, .16), lty=2, add=TRUE)
legend("topleft", c("Analytic Posterior", "Quadratic Approximation"), 
           lty = 1:2)
mtext("n = 9")

#
# MCMC -- start thinking in samples
#

# Number of samples to draw 
n_samples <- 1000
# Set up an NA vector
p <- rep(NA, n_samples)
# Set the start value to .5
p[1] <- 0.5
# n=9: 6 Water, 3 Land
W <- 6
L <- 3
# Loop
for(i in 2:n_samples) {
    p_new <- rnorm(1, p[i-1], 0.1)
    if (p_new < 0) p_new <- abs(p_new)
    if (p_new > 1) p_new <- 2 - p_new
    q0 <- dbinom(W, W+L, p[i-1])
    q1 <- dbinom(W, W+L, p_new)
    p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}
dens(p, xlim = c(-1, 1))
curve(dbeta(x, W+0, L+1), lty=2, add=TRUE)
