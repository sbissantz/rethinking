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

# Prior predictive checks
#
N <- 1e5

# Prior implications
#
# Alpha prior
# alpha_lo <- rnorm(N, 0, 10) # redicolous!
# hist(alpha_lo)
# alpha_lo <- rnorm(N, 0, 1) # maybe too narrow (although plausible)
# alpha_lo <- rnorm(N, 0, 0.5) # flat! Given the plot below, this is a good
# choice however to remain consistent with the book, I will use the next one
alpha_lo <- rnorm(N, 0, 1.5) # flat! 
alpha_p <- plogis(alpha_lo) 
hist(alpha_p)

# Visualize
# logit(pi) = X_i*beta + alpha
# pi = logit^-1(X_i*beta + alpha) = sigmoid(X_i*beta + alpha)
plot(c(-4,4), c(0,1), type="n", ylab="Probability of survival",
     xlab="Predictor values", main="Prior predictive simulation")
for(i in 1:50) {
    curve(plogis(alpha_lo[i] * x), from=-4, to=4, add=TRUE)
}

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
(post1 <- fit1$draws(format="matrix"))
# Plausibility check: Compare mean to precis output (see: below)
mean(post1[,"alpha[2]"])
# Extract the log likelihood matrix
LL1 <- fit1$draws("log_lik")
# Relative effective sample size
reff1 <- loo::relative_eff(exp(LL1))
loo1 <- loo::loo(LL1, r_eff=reff1)
# PSIS
psis1 <- loo::loo(LL1, r_eff=reff1, is_method="psis")
pareto_k1 <- psis1$diagnostics$pareto_k

# See if my model does the same as the one in the book
#
# Make the tank cluster variable
d$tank <- 1:nrow(d)
dat <- list(
    S = d$surv,
    N = d$density,
    tank = d$tank )
# Approximate posterior
m13.1 <- ulam(
    alist(
        S ~ dbinom( N , p ) ,
        logit(p) <- a[tank] ,
        a[tank] ~ dnorm( 0 , 1.5 )
    ), data=dat , chains=4 , log_lik=TRUE )
precis(m13.1,depth = 2)
# Compare to the book's results
rethinking::stancode(m13.1)
post1_ulam <- extract.samples(m13.1)
post1_ulam 
# TODO: different results? Why?

# Prior implications
#
N <- 1e5
#
# Alpha prior
# alpha_lo <- rnorm(N, 0, 10) # redicolous!
# hist(alpha_lo)
# alpha_lo <- rnorm(N, 0, 1) # maybe too narrow (although plausible)
# alpha_lo <- rnorm(N, 0, 0.5) # flat! Given the plot below, this is a good
# choice however to remain consistent with the book, I will use the next one
alpha_bar_lo <- rnorm(N, 0, 1.5)
sigma_lo <- rexp(1)
alpha_lo <- rnorm(alpha_bar_lo, sigma_lo)  # flat! 

# Prior predictive checks 
# logit(pi) = X_i*beta + alpha
# pi = logit^-1(X_i*beta + alpha) = sigmoid(X_i*beta + alpha)
plot(c(-4,4), c(0,1), type="n", ylab="Probability of survival",
     xlab="Predictor values", main="Prior predictive simulation")
for(i in 1:50) {
    curve(plogis(alpha_lo[i] * x), from=-4, to=4, add=TRUE)
}

# Fit the second model (partial pooling)
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "2.stan")
mdl2 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit2 <- mdl2$sample(data=dat_ls)
# Note that the alpha parameters are on the log-odds scale!
fit2$print(max_rows=200)

# Diagnostics
#
fit2$sampler_diagnostics()
fit2$cmdstan_diagnose()
fit2$diagnostic_summary()
# fit2_stan <- rstan::read_stan_csv(fit1$output_files())
# rethinking::trankplot(fit1_stan)

# Extract posterior draws
post2 <- fit2$draws(format="matrix")
# Plausibility check: Compare mean to precis output (see: below)
colnames(post2)
mean(post2[,"alpha[10]"])
# Extract the log likelihood matrix
LL2 <- fit2$draws("log_lik")
# Relative effective sample size
reff2 <- loo::relative_eff(exp(LL2))
loo2 <- loo::loo(LL2, r_eff=reff2)
# PSIS
psis2 <- loo::loo(LL2, r_eff=reff2, is_method="psis")
pareto_k2 <- psis2$diagnostics$pareto_k

# See if my model does the same as the one in the book
#
m13.2 <- ulam(
    alist(
        S ~ dbinom( N , p ) ,
        logit(p) <- a[tank] ,
        a[tank] ~ dnorm( a_bar , sigma ) ,
        a_bar ~ dnorm( 0 , 1.5 ) ,
    sigma ~ dexp( 1 )
), data=dat , chains=4 , log_lik=TRUE )

precis(m13.2,depth = 2)
# Compare to the book's results
rethinking::stancode(m13.2)
# Check!

# Model comparison
#
comp <- loo::loo_compare(loo1, loo2)
print(comp, simplify=FALSE)

# Plot the results
#
# Posterior distribution of the intercepts (probabilities)
pat <- grep("alpha", colnames(post1))
# Posterior mean of the partial pooling estimates of the intercepts (probabilities)
alpha_hat_pp <- plogis(colMeans(post2[, pat]))

n_tanks <- length(alpha_hat_np)
plot(c(1, n_tanks), c(0, 1), type="n", xlab="Tank", ylab="Intercept",
     main="Posterior distribution of the intercepts")
     points(seq(n_tanks), d$propsurv, pch=16, col="steelblue")
     points(seq(n_tanks), alpha_hat_pp, pch=1)
     # draw vertical dividers between tank densities
    axis( 1 , at=c(1,16,32,48) , labels=c(1,16,32,48) )
abline( h=mean(alpha_hat_np), lty=2 )
abline( v=16.5 , lwd=0.5 )
abline( v=32.5 , lwd=0.5 )
text( 8 , 0 , "small tanks" )
text( 16+8 , 0 , "medium tanks" )
text( 32+8 , 0 , "large tanks" )

alpha_bar <- post2[,"alpha_bar"]
sigma <- post2[,"sigma"]
# Infered population-distribution of survival
# show first 100 populations in the posterior
plot( NULL , xlim=c(-3,4) , ylim=c(0,0.35) ,
    xlab="log-odds survive" , ylab="Density" )
for ( i in 1:100 )
    curve( dnorm(x,alpha_bar[i],sigma[i]) , add=TRUE ,
    col=col.alpha("black",0.2) )

# Sample 5e3 imaginary tanks from the posterior averaged over the posterior
sim_tanks <- rnorm(5e3, alpha_bar, sigma)
plot(density(plogis(sim_tanks), adj=0.1), xlab = "Probability of survival")

#
# No-pooling simulation
#

# Assign values to the parameters
#
# Grand mean log odds of survival
a_bar <- 1.5
# Standard deviation around the grand mean
sigma <- 1.5
# Number of ponds
nponds <- 60
# Be explicit about the class (stan)!
Ni <- as.integer( rep( c(5,10,25,35) , each = 15 ))
# Simulate a population of ponds (log-odds of survival)) 
a_pond <- rnorm( npons , a_bar , sigma )
# Put everything in a data frame
dsim <- data.frame( pond = seq(npons) , Ni = Ni, true_a = a_pond )
# Convert to probability (logit link)
dsim$p_true <- plogis(dsim$true_a)
# Simulate the number of survivors in each pond
dsim$Si <- rbinom( nponds, prob=p_pond, size=dsim$Ni )

# No-pooling estimates
#
dsim$p_nopool <- dsim$Si / dsim$Ni

# Partial-pooling model 
#
dsim$P <- seq(nponds) 
dat_ls <- list("n_ponds" = length(dsim$Ni),  "P" = dsim$P, "N" = dsim$Ni, "S" =
               dsim$Si)

# Fit the first model (no pooling)
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "3.stan")
mdl3 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit3 <- mdl3$sample(data=dat_ls)
# Note that the alpha parameters are on the log-odds scale!
fit3$print(max_rows=150)

# Diagnostics
#
fit3$sampler_diagnostics()
fit3$cmdstan_diagnose()
fit3$diagnostic_summary()
# fit1_stan <- rstan::read_stan_csv(fit1$output_files())
# rethinking::trankplot(fit1_stan)

# Extract posterior draws
(post3 <- fit3$draws(format="matrix"))
# Plausibility check: Compare mean to precis output (see: below)

# Partial pooling estimates
#
# Extract only alpha and sigma from the posterior
# (pat <- grepl("alpha|sigma" , colnames(post3)))
(pat <- grepl("alpha" , colnames(post3)))
alpha <- colMeans(post3[,pat])

nopool_error <- abs( dsim$p_nopool - dsim$p_true )  
# Exclude alpha_bar
partpool_error <- abs( plogis(alpha[-61]) - dsim$p_true)

# Averages
nopool_avg <- aggregate( nopool_error , list(dsim$N) , mean )
partpool_avg <- aggregate( partpool_error , list(dsim$N) , mean )

# Plot the results
plot( seq(60), nopool_error, xlab = "pond", ylab = "absolute error", pch = 20)
points( seq(60), partpool_error, pch = 20, col = "red" )






