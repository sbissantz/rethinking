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

#
# 13.3 More than one type of cluster
#

library(rethinking)
data(chimpanzees)

View(chimpanzees)
?chimpanzees

# Prior implications
#
N <- 1e5
#
# Alpha prior
a_bar_lo <- rnorm(N, 0, 1.5)
sigma_a_lo <- rexp(N,1)
a_lo <- rnorm(a_bar_lo, sigma_a_lo)  # flat! 
hist(a_lo)
# Gamma prior
g_bar_lo <- rnorm(N, 0, 1.5)
sigma_g_lo <- rexp(N,1)
g_lo <- rnorm(rep(0,N), sigma_g_lo)  # flat! 
hist(g_lo)
# Beta prior
# b_lo <- rnorm(N, 0, 0.25) # seems more plausible 
b_lo <- rnorm(N, 0, 0.5)

# Prior predictive checks 
# logit(pi) = X_i*beta + alpha
# pi = logit^-1(X_i*beta + alpha) = sigmoid(X_i*beta + alpha)
plot(c(-4,4), c(0,1), type="n", ylab="Probability to pull left",
     xlab="Predictor values", main="Prior predictive simulation")
for(i in 1:50) {
    curve(plogis(a_lo[i] + g_lo[i] + b_lo[i]  * x), from=-4, to=4, add=TRUE)
}

# Load the data
#
library(rethinking)
data(chimpanzees)
d <- chimpanzees
# Create a treatment variable
d$treatment <- 1 + d$prosoc_left + 2*d$condition

# Data list
#
dat_ls <- list("N" = nrow(d), "tid" = d$treatment, 
"tno" = length(unique(d$treatment)),
"aid" = d$actor, "ano" = length(unique(d$actor)), "bid" = d$block, 
"bno" = length(unique(d$block)),
"y" = d$pulled_left)
dat_ls
# Fit the model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "4.stan") 
mdl1 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit1 <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit1$sampler_diagnostics()
fit1$cmdstan_diagnose()
fit1$diagnostic_summary()

# Outputs
#
# Approximately the same as the book
fit1$cmdstan_summary()
fit1$summary(variables=c("alpha_j", "beta_j", "gamma_j", "sigma_a", "sigma_g"))

# Draws from the posterior 
post1 <- fit1$draws(format="data.frame")

# Fit the model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "5.stan") 
mdl2 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit2 <- mdl2$sample(data=dat_ls)

# Diagnostics
#
fit2$sampler_diagnostics()
fit2$cmdstan_diagnose()
fit2$diagnostic_summary()

# Outputs
#
# Approximately the same as the book
fit2$cmdstan_summary()
fit2$summary(variables=c("alpha_j", "beta_j", "sigma_a"))

# Draws from the posterior 
post2 <- fit2$draws(format="data.frame")

# Model comparison
#
# Extract the log likelihood matrix
LL1 <- fit1$draws("log_lik")
LL2 <- fit2$draws("log_lik")
# Relative effective sample size
reff1 <- loo::relative_eff(exp(LL1))
reff2 <- loo::relative_eff(exp(LL2))
# LOO
loo1 <- loo::loo(LL1, r_eff=reff1)
loo2 <- loo::loo(LL2, r_eff=reff2)
comp <- loo::loo_compare(loo1, loo2)
print(comp, simplify=FALSE)

# Fit the model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "6.stan") 
mdl3 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit3 <- mdl3$sample(data=dat_ls)

# Diagnostics
#
fit3$sampler_diagnostics()
fit3$cmdstan_diagnose()
fit3$diagnostic_summary()

# Outputs
#
# Approximately the same as the book
fit3$cmdstan_summary()
fit3$summary(variables=c("beta_j"))

# Draws from the posterior 
post3 <- fit3$draws(format="data.frame")

#
# Divergent transitionss
#

# The Devil's Funnel
#
dat_ls <- list(N=1)
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "7.stan") 
mdl7 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
# With fixed params I get no divergent transition warnings
# fit7 <- mdl7$sample(fixed_param=TRUE)
fit7 <- mdl7$sample(data=dat_ls)
# Can also adapt_delta
# ...does not help. Reparameterize!
fit7 <- mdl7$sample(data=dat_ls, adapt_delta=0.99)

# Book Version
#
m13.7 <- rethinking::ulam(
    alist(
        v ~ normal( 0 , 3 ) ,
        x ~ normal( 0 , exp(v) )
    ), data=dat_ls , chains=4 , log_lik=TRUE ) 
rethinking::stancode(m13.7)

# Diagnostics
#
fit7$sampler_diagnostics()
fit7$cmdstan_diagnose()
fit7$diagnostic_summary()

# Outputs
#
fit7$cmdstan_summary()

# Reparameterized model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "8.stan") 
# With fixed params I get no divergent transition warnings
mdl8 <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit8 <- mdl8$sample(dat_ls)

# Diagnostics
#
fit8$sampler_diagnostics()
fit8$cmdstan_diagnose()
fit8$diagnostic_summary()

# Outputs
#
fit8$cmdstan_summary()

#
# Non-centered chimpanzees
#
library(rethinking)
data(chimpanzees)
d <- chimpanzees
# Create a treatment variable
d$treatment <- 1 + d$prosoc_left + 2*d$condition
# Create data list
dat_ls <- list("N" = nrow(d), "tid" = d$treatment, 
"tno" = length(unique(d$treatment)),
"aid" = d$actor, "ano" = length(unique(d$actor)), "bid" = d$block, 
"bno" = length(unique(d$block)),
"y" = d$pulled_left)
dat_ls

# Book version

m13.6nc <- rethinking::ulam(
    alist(
        y ~ bernoulli( p ) ,
        logit(p) <- a_bar + z[aid] * sigma_a + x[bid]*sigma_g + b[tid],
        b[tid] ~ normal( 0 , 0.5 ) ,
        z[aid] ~ normal( 0 , 1 ) ,
        x[bid] ~ normal( 0 , 1 ) ,
        a_bar ~ normal( 0 , 1.5 ) ,
        sigma_a ~ exponential( 1 ) ,
        sigma_g ~ exponential( 1 ) ,
        gq> vector[aid]:a <<- a_bar + z * sigma_a ,
        gq> vector[bid]:g <<- x * sigma_g 
    ), data=dat_ls , chains=4)

rethinking::stancode(m13.6nc)

path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "13", "6nc.stan") 
mdl6nc <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit6nc <- mdl6nc$sample(data=dat_ls)

# Diagnostics
#
fit6nc$sampler_diagnostics()
fit6nc$cmdstan_diagnose()
fit6nc$diagnostic_summary()

# Outputs
#
# Approximately the same as the book
fit6nc$cmdstan_summary()
fit6nc$summary(variables=c("alpha_j", "beta_j", "gamma_j"))

# Draws from the posterior 
post6nc <- fit6nc$draws(format="data.frame")

# 
# Posterior predictions
#

# Posterior retrodictions (same cluster)
str(post6nc$`a_j[1]`)
colnames(post6nc)
# TODO: Should I recode it, so that there is a matrix?



