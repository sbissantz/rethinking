#
# Prior predictive simulation for a bernoulli model
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

# Helper function
#
sigmoid <- function(x) 1 / (1 + exp(-x))
# Alpha prior
alpha_lo <- rnorm(N, 0, 1.5)
alpha_p <- sigmoid(alpha_lo)
#alpha_p <- plogis(alpha_lo) 
plot(density(alpha_p), xlim=c(0,1))
# Beta prior
beta_lo <- rnorm(N, 0,0.5)
beta_p <- sigmoid(beta_lo) 
#beta_p <- plogis(beta_lo) 
plot(density(beta_p), xlim=c(0,1))

# Visualize
# logit(pi) = X_i*beta + alpha
# pi = logit^-1(X_i*beta + alpha) = sigmoid(X_i*beta + alpha)
plot(c(-4,4), c(0,1), type="n", ylab="Pr(pull_left)",
     xlab="Predictor values", main="Prior predictive simulation")
for(i in 1:50) {
    curve(sigmoid(alpha_lo[i] + beta_lo[i] * x), from=-4, to=4, add=TRUE)
}

# Stan Workflow
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "pps_1.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)

# 504 trials 
dat_ls <- list(N=nrow(d), x=d$treatment)
fit <- mdl$sample(data=dat_ls, fixed_param=TRUE)

fit$cmdstan_diagnose()
fit$summary()

# Samples from the posterior 
samples <- fit$draws(format="data.frame")

# Prior implications
#

# alpha (log_odds)
alpha_lo <- samples$alpha
beta_lo <- samples$beta

# Helper function 
sigmoid <- function(x) 1 / (1 + exp(-x))

# alpha (prob)
alpha_p <- sigmoid(alpha_lo) 
plot(density(alpha_p))
#alpha_p <- plogis(alpha_lo) 
beta_p <- sigmoid(beta_lo)  
plot(density(beta_p))
#beta_p <- plogis(beta_lo) 

# Simulated data sets
y_tilde <- fit$draws("y_tilde", format="matrix")

# Posterior line predictions
#
plot(c(-4,4), c(0,1), type="n", ylab="Pr(pull_left)",
     xlab="Predictor values", main="Prior predictive simulation")
for(i in 1:50) {
    curve(sigmoid(alpha_lo[i] + beta_lo[i] * x), from=-4, to=4, add=TRUE)
}

# Prior preditive distribution
#
plot(density(y_tilde[1,]), xlim=c(-1, 2), ylim=c(0,35), main="Prior
     predictive", xlab="y_tilde")
for(i in 1:500) {
  lines(density(y_tilde[i,]), col=col.alpha("black", 0.2))
}
