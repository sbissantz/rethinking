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

# Prior predictive simulation (R)
#

# Prior implications (R)
N <- 1e3

alpha_lo <- rnorm(N, 0, 1.5)
alpha_p <- plogis(alpha_lo) 
plot(density(alpha_prob), xlim=c(0,1))

beta_lo <- rnorm(N, 0,0.5)
beta_p <- plogis(beta_lo) 
plot(density(beta_p), xlim=c(0,1))

# Alternative
inv_logit <- function(x) exp(x) / (1 + exp(x))
alpha_p <- inv_logit(alpha_lo)
beta_p <- inv_logit(beta_lo) 

# Visualize
# logit(pi) = X_i*beta + alpha
# pi = logit^-1(X_i*beta + alpha)
plot(c(-4,4), c(0,1), type="n", ylab="Pr(pull_left)",
     xlab="Predictor values", main="Prior predictive simulation")
for(i in 1:50) {
    curve(inv_logit(alpha_lo[i] + beta_lo[i] * x), from=-4, to=4, add=TRUE)
}

# TODO


# Prior predictive simulaion (Stan)
#
path <- "~/projects/stanmisc/stan/11/"
file <- file.path(path, "pps_1.stan")
x <- d$treatment 
N <- nrow(d)
data_ls <- list(N=N, x=x)
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
pps <- mdl$sample(data=data_ls, fixed_param=TRUE)
samples <- pps$draws(format="df")
mu <- pps$draws("mu", format="matrix")
x_seq <- seq(1,4, length.out=N)


##########################

# TODO

fit$cmdstan_diagnose()
fit$print()

# Samples
#
mu <- fit$draws(variables = "mu", format = "matrix")
# D_tilde <- fit$draws(variables = "D_tilde", format = "matrix")

# Parameter preparation
#
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
D_tilde_HPDI <- apply(D_tilde, 2, rethinking::HPDI)

# Posterior predictive plots
#
plot(mu_mean ~ d$D, ylab = "Predicted divorce", xlab = "Observed divorce", 
     pch = 20, col = "lightblue")
abline(a = 0, b = 1, lty = 2)  
# Posterior line uncertainty 
for (i in seq(1:nrow(d))) lines(rep(d$D[i], 2), mu_HPDI[, i], 
                                 col = alpha("black", 0.7))
# Posterior uncertainty 
for (i in seq(1:nrow(d))) lines(rep(d$D[i], 2), D_tilde_HPDI[,i], 
                                 col = alpha("black", 0.3))


##########################





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
plot(density(alpha_p))
beta_p <- inv_logit(beta_lo)
plot(density(beta_p))

# Samples
#



#
# Code a Stan version. Switching to Stan is necessary, because it allows to
# constraint the p's more easily. E.g. a lower bound for the predictor.
#
















