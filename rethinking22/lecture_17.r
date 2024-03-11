#
# Lecture 17 (Measurement error)
# 

# Myth: Measurement error only reduces effect estimates, never increases them.

# Draw a DAG
library(dagitty)
dag <- dagitty( 'dag {
parent_income [outcome,pos="0,0"]
child_income [exposure,pos="1,0"]
parent_income_recall [outcome,pos="0,0.25"]
e_parent [outcome,pos="0,0.5"]
parent_income -> child_income
parent_income -> parent_income_recall
e_parent -> parent_income_recall
child_income -> e_parent 
}')
plot(dag)
# child_income -> e_parent: children remember their income to be more similar to
# their parents, therefore this is a recall bias!

# Generative simulation
set. seed(112)
# children-parent income example
# recall bias on parental income
N <- 5e2
P <- rnorm(N)
# Vary the simulation: (a), (b), (c)
# (a) Causal effect of parent on child income is 0!
C <- rnorm(N, 0*P)
# (b) Causal effect of parent on child large & positive!
# C <- rnorm(N, 0.75*P)
# (c) Causal effect of parent on child large & negative!
# C <- rnorm(N, -0.75*P)
# Mixture: Simulating th recall bias
# Since there are two paths(!) enter P_ast, we need a mixture, i.e., the "+"
P_ast <- rnorm(N, 0.8 * P + 0.2 * C)

# Data list
dat_list <- list(N=N, C=C, X = cbind(P_ast))

# Stan model
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "17", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences!
fit$diagnostic_summary()

# Posterior 
b <- fit$draws("b", format = "matrix")
# Visualize effect of parent on chil income
# Almost all posterior mass above 0, the model thinks there is an effect!!
# Remmber: Effect was set to 0!
hist(b)  

# Modeling measurement
#
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

# New variables 
d$Divorce.SE ; d$Marriage.SE

# Visualize
plot( d$Divorce ~ d$Marriage , ylim=c(4,15) , xlim=c(12,35) ,
    xlab="Marriage rate" , ylab="Divorce rate" , lwd=3)


# Standard errors
for ( i in 1:nrow(d) ) {
    ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
    x <- d$Marriage[i]
    lines( c(x,x) , ci , lwd=4 , col=col.alpha(1,0.2) )
    ci <- d$Marriage[i] + c(-1,1)*d$Marriage.SE[i]
    y <- d$Divorce[i]
    lines( ci , c(y,y) , lwd=4 , col=col.alpha(1,0.2) )
}

# DAG 
dag <- dagitty::dagitty( 'dag {
divorce_rate [outcome,pos="1,0"]
e_divorce [outcome,pos="1,0.1"]

marriage_rate [selected,pos="0,0"]
e_marriage [selcted,pos="0,0.1"]

age [selected,pos="0.5,0.25"]
e_age [selected,pos="0.5,0.35"]

e_age -> age 

e_divorce -> divorce_rate
e_marriage -> marriage_rate 

marriage_rate -> divorce_rate
divorce_rate <- age -> marriage_rate

}')
plot(dag)

# Visualize as a function of log population
# Import: Smaller states have lower error; less precise estimates!
# P -> e_D  
# Note: Need to run the time series longer to get the same precision
plot( d$Divorce ~ log(d$Population) , ylim=c(4,15) ,
    xlab="Population (log)" , ylab="Divorce rate" , lwd=3 , col=2 )
for ( i in 1:nrow(d) ) {
    ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
    x <- log(d$Population)[i]
    lines( c(x,x) , ci , lwd=6 , col=col.alpha(2,0.5) )
}

# Full DAG 
dag <- dagitty::dagitty( 'dag {
divorce_rate [outcome,pos="1,0"]
divorce_rate_ast [outcome,pos="1,0.1"]
e_divorce [outcome,pos="1,0.2"]

marriage_rate [selected,pos="0,0"]
marriage_rate_ast [selected,pos="0,0.1"]
e_marriage [selcted,pos="0,0.2"]

age [selected,pos="0.5,0.15"]
age_ast [selected,pos="0.5,0.25"]
e_age [selected,pos="0.5,0.35"]

population [selected,pos="0.5,0.5"]

e_age -> age_ast
age -> age_ast

e_divorce -> divorce_rate_ast
e_marriage -> marriage_rate_ast

marriage_rate -> divorce_rate
divorce_rate <- age -> marriage_rate

marriage_rate -> marriage_rate_ast 
divorce_rate -> divorce_rate_ast 

e_marriage<- population -> e_age
population -> e_divorce
}')
plot(dag)

# Start by modeling only the measurement error in the outcome
#

# Measurement error in the outcome 
dag <- dagitty::dagitty( 'dag {
divorce_rate [outcome,pos="1,0"]
divorce_rate_ast [outcome,pos="1,0.1"]
e_divorce [outcome,pos="1,0.2"]

marriage_rate [selected,pos="0,0"]
age [selected,pos="0.5,0.15"]

marriage_rate <- age -> divorce_rate
marriage_rate -> divorce_rate
divorce_rate -> divorce_rate_ast
e_divorce -> divorce_rate_ast
}')
plot(dag)

# Ulam model
#

dlist <- list(
    D_obs = rethinking::standardize( d$Divorce ),
    # Divide the standard error by the sd of the data to stanrdize it!
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M = standardize( d$Marriage ),
    A = standardize( d$MedianAgeMarriage ),
    N = nrow(d)
)

m15.1 <- rethinking::ulam(
    alist(
        # model for D* (observed)
        D_obs ~ dnorm( D_true , D_sd ),

        # model for D (unobserved)
        vector[N]:D_true ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M,
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=dlist , chains=4 , cores=4 )

# Get the stancode
rethinking::stancode(m15.1)

# Stan model
#

# Prior predictive simulation 
# Note: Since the variables are all standardized, we can stick with 
# the default priors for scaled variables.
pps <- list()
N <- 1e3
set.seed(112)
# Assumptions
pps$sigma <- rexp(N, 1) ; hist(pps$sigma) 
pps$D_sd <- rexp(N, 1) ; hist(pps$D_sd)
pps$a <- rnorm(N, 0, 0.1) ; hist(pps$D_a)
pps$bA <- rnorm(N, 0, 0.5) ; hist(pps$bA) 
pps$bM <- rnorm(N, 0, 0.5) ; hist(pps$bM)
pps$A <- seq(-2,2, length.out = N) 
pps$M <- seq(-2,2, length.out = N) 
# Generative model
pps$mu <- with(pps, a + bA*A + bM*M ) ; hist(pps$mu)
pps$D_true <- with(pps, rnorm(N, mu, sigma))
# Measurement model
pps$D_obs <- with(pps, rnorm(N, D_true, D_sd))
# Visualize the mean
# Intercept
plot(NULL, xlim = c(-2,2), ylim = c(-2,2))
for(i in 1:30) {
# Since all predictors are standardized the intercept a: E(Y | X = X-bar) = 0
 curve(pps$a[i] * x, from = -2, to = 2, add = TRUE)   
}
# Effect of a standardized predictor
plot(NULL, xlim = c(-2,2), ylim = c(-4,4))
for(i in 1:30) {
 curve(pps$a[i] + pps$bA[i] * x, from = -2, to = 2, add = TRUE)   
}
# Effect of a standardized predictor
# Note: Since the priors are the same the outcome is the same
plot(NULL, xlim = c(-2,2), ylim = c(-4,4))
for(i in 1:30) {
 curve(pps$a[i] + pps$bM[i] * x, from = -2, to = 2, add = TRUE)   
}
# pending
# Visualize the outcome
density(pps$D_true) |> plot(lwd = 3, main = "Density: Divorce")
# The observed distribution of values is wider (blue)
# Note: Mean is the same; uncertainty increased
density(pps$D_obs) |> lines(lwd = 3, col = "steelblue")

# Stan model
#
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

dat_list <- list(
    N = nrow(d),
    D_obs = rethinking::standardize( d$Divorce ),
    # Divide the standard error by the sd of the data to stanrdize it!
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M = rethinking::standardize( d$Marriage ),
    A = rethinking::standardize( d$MedianAgeMarriage )
)

# Centered parametrization
# Note: normal_id_glm() should be faster!
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "17", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences!
fit$diagnostic_summary()

# Posterior draws
#
post1 <- fit$draws(format = "data.frame") 
D_true <- fit$draws("D_true", format = "matrix") 
D_true_mean <- colMeans(D_true)

# Visualize
#
plot(dat_list$A, dat_list$D_obs, pch = 20, xlab = "Age at Marriage", 
ylab = "Divorce rate")
points(dat_list$A, D_true_mean, pch = 20, col = "steelblue")
for ( i in 1:nrow(d) ) {
    x <- dat_list$A[i]
    lines( c(x,x) , c(dat_list$D_obs[i],D_true_mean[i]) , lwd=3 , col="grey" )
}
N_samples <- nrow(post1)
Aseq <- seq(-3,3, length.out = N_samples) 
a_map <- mean(post1$a)
bA_map <- mean(post1$bA)
# Visualize uncertainty 
for(i in 1:30) {
    curve(post1$a[i] + post1$bA[i] * x, from = -3, to = 3, add = TRUE, 
    col = "grey")
}
# Add posterior mean line
curve(a_map + bA_map * x, from = -3, to = 3, add = TRUE, lwd = 2)
# Important: It would be best to compare the posterior mean line from the model
# withouth measurement error and the uncertainty distribtuon to see what has 
# changed!

# Extend by modeling the measurement error in marriage rate 
#
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

# Data list
#
dat_list <- list(
    N = nrow(d),
    D_obs = rethinking::standardize( d$Divorce ),
    M_obs = rethinking::standardize( d$Marriage ),
    # Divide the standard error by the sd of the data to stanrdize it!
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M_sd = d$Marriage.SE / sd( d$Marriage ),
    M = rethinking::standardize( d$Marriage ),
    A = rethinking::standardize( d$MedianAgeMarriage )
)

# Ulam model
#
m15.2 <- ulam(
    alist(
        # D* model (observed)
        D_obs ~ dnorm( D_true , D_sd ),

        # D model (unobserved)
        vector[N]:D_true ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M_true[i],
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp( 1 ),

        # M* model (observed)
        M_obs ~ dnorm( M_true , M_sd ),

        # M model (unobserved)
        vector[N]:M_true ~ dnorm( nu , tau ),
        nu <- aM + bAM*A,
        aM ~ dnorm(0,0.2),
        bAM ~ dnorm(0,0.5),
        tau ~ dexp( 1 )

    ) , data=dat_list , chains=4 , cores=4 )

precis(m15.2)

       #mean   sd  5.5% 94.5% n_eff Rhat4
#a     -0.03 0.10 -0.18  0.13  2218     1
#bA    -0.48 0.19 -0.77 -0.18  1537     1
#bM     0.29 0.24 -0.11  0.69  1225     1
#sigma  0.56 0.11  0.40  0.73   777     1
#aM    -0.11 0.08 -0.23  0.01  2779     1
#bAM   -0.66 0.08 -0.80 -0.53  2022     1
#tau    0.44 0.07  0.34  0.56  1189     1

# Stancode
rethinking::stancode(m15.2)

# Stan model
#

# Centered parametrization
# Note: normal_id_glm() should be faster!
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "17", "3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences!
fit$diagnostic_summary()

# Posterior draws
#
post2 <- fit$draws(format = "data.frame") 
D_true <- fit$draws("D_true", format = "matrix") 
D_true_mean <- colMeans(D_true)
M_true <- fit$draws("M_true", format = "matrix") 
M_true_mean <- colMeans(M_true)

# Visualize (2D shrinkage)
#
plot( dat_list$M_obs , dat_list$D_obs , pch=20 , lwd=3 , col = "blue",
xlab="marriage rate (std)" , ylab="divorce rate (std)" )
points( M_true_mean , D_true_mean , lwd=3 , pch=20, col="black")
for ( i in 1:nrow(d) ) { 
    lines( c( dat_list$M_obs[i] , M_true_mean[i] ) , 
    c( dat_list$D_obs[i] , D_true_mean[i] ) , lwd=2 , col="steelblue" )
}

# Visualize
#
plot(dat_list$A, dat_list$D_obs, pch = 20, xlab = "Age at Marriage", 
ylab = "Divorce rate")
points(dat_list$A, D_true_mean, pch = 20, col = "steelblue")
for ( i in 1:nrow(d) ) {
    x <- dat_list$A[i]
    lines( c(x,x) , c(dat_list$D_obs[i],D_true_mean[i]) , lwd=3 , col="grey" )
}
N_samples <- nrow(post2)
Aseq <- seq(-3,3, length.out = N_samples) 
a_map <- mean(post2$a)
bA_map <- mean(post2$bA)
# Visualize uncertainty 
for(i in 1:30) {
    curve(post2$a[i] + post2$bA[i] * x, from = -3, to = 3, add = TRUE, 
    col = "grey")
}
# Add posterior mean line
curve(a_map + bA_map * x, from = -3, to = 3, add = TRUE, lwd = 2)

# Compare old result and new result
#
# Effect of A on D
# With error
plot(density(post2$bA), lwd = 2)
# Without error 
lines(density(post1$bA), col = "red", lwd = 2)

# Effect of M on D
# With error
# Without error 
plot(density(post1$bM), col = "red", lwd = 3)
lines(density(post2$bM), lwd = 3)

# Different results! Ignore if my model specificication is wrong Different results! Ignore if my model specificication is wrong
#

#
# Misclassification
#

# Draw a DAG
dag <- dagitty::dagitty( 'dag {
    Y_obs [selected,pos="0,0"]
    e_Y [selected,pos="1,0"]
    Y [outcome,pos="0,1"]
    M [selected,pos="1,1"]
    T_MF [selected,pos="0.5,2"]
    F [selected,pos="0,3"]
    Y_obs <- e_Y
    Y -> Y_obs 
    M -> Y
    F -> Y
    T_MF -> Y
    M-> T_MF <- F

}')
plot(dag)

# Ulam model
#

# Data not available!
d <- read.csv( "Himba.csv" )
dat <- as.list(d)
dat$X <- dat$y

# Make up non-sensical data to get the Stan cod
N <- 100
dad_id <- 1:N
mom_id <- 1:N
dyad_id <- 1:N
X <- rbinom(N, size = 1, prob = 70/N)

dat <- list(
    "N" = N,
    "mom_id" = mom_id,
    "dyad_id" = mom_id,    
    "X" = X
)

# without false positive rate
mXn <- ulam(
    alist(
        X ~ bernoulli(p),
        logit(p) <- a + z[mom_id]*sigma + x[dyad_id]*tau,
        a ~ normal(0,1.5),
        z[mom_id] ~ normal(0,1),
        sigma ~ normal(0,1),
        x[dyad_id] ~ normal(0,1),
        tau ~ normal(0,1)
    ) , data=dat , chains=4 , cores=4 , iter=4000 ,
    constraints=list(sigma="lower=0",tau="lower=0") )

# Stancode
rethinking::stancode(mXn)

# Stan version
#
# Note: bernoulli_logit() should be more stable!
# Note: bernoulli_logit_glm() should be faster!
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "17", "4.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() 
fit$diagnostic_summary()

# Posterior draws
#
post1 <- fit$draws(format = "data.frame") 

#
# Modeling the false positive rate
#

# False positive rate
dat$f <- 0.05

# Ulam version
#
mX <- ulam(
    alist(
        X|X==1 ~ custom( log( p + (1-p)*f ) ),
        X|X==0 ~ custom( log( (1-p)*(1-f) ) ),
        logit(p) <- a + z[mom_id]*sigma + x[dyad_id]*tau,
        a ~ normal(0,1.5),
        z[mom_id] ~ normal(0,1),
        sigma ~ normal(0,1),
        x[dyad_id] ~ normal(0,1),
        tau ~ normal(0,1)
    ) , data=dat , chains=4 , cores=4 , iter=4000 ,
    constraints=list(sigma="lower=0",tau="lower=0") )

# Stancode
rethinking::stancode(mX)

# Stan version
#
# Note: bernoulli_logit() should be more stable!
# Note: bernoulli_logit_glm() should be faster!
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "17", "5a.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() 
fit$diagnostic_summary()

# Posterior draws
#
post2 <- fit$draws(format = "data.frame") 

# Visualize
#
# Note: The results are the same, because I made up some random data
# In the original data including the error reduces the effect estimate a bit!
#
# Ignore error 
dens( inv_logit(post1$a) , xlim=c(0,1) , lwd=4 , col=grau() , 
    xlab="probability extra-pair paternity" )
# Modeling the error 
dens( inv_logit(post2$a) , add=TRUE , lwd=4 , col=2 )

# numerically kosher version

# Ulam version
#
mX2 <- ulam(
    alist(
        #X|X==1 ~ custom( log( p + (1-p)*f ) ),
        X|X==1 ~ custom( log_sum_exp( log(p) , log1m(p)+log(f) ) ),
        #X|X==0 ~ custom( log( (1-p)*(1-f) ) ),
        X|X==0 ~ custom( log1m(p) + log1m(f) ),
        logit(p) <- a + z[mom_id]*sigma + x[dyad_id]*tau,
        a ~ normal(0,1.5),
        z[mom_id] ~ normal(0,1),
        sigma ~ normal(0,1),
        x[dyad_id] ~ normal(0,1),
        tau ~ normal(0,1)
    ) , data=dat , chains=4 , cores=4 , iter=4000 ,
    constraints=list(sigma="lower=0",tau="lower=0") )

precis(mX)
precis(mX2)

# Stancode
#
stancode(mX2)

# Stan version
#
# Note: bernoulli_logit() should be more stable!
# Note: bernoulli_logit_glm() should be faster!
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "17", "5b.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() 
fit$diagnostic_summary()

# Posterior draws
#
post3 <- fit$draws(format = "data.frame") 

# double kosher

# Ulam version
# 
mX3 <- ulam(
    alist(
        #X|X==1 ~ custom( log( p + (1-p)*f ) ),
        X|X==1 ~ custom( log_sum_exp( log_p , log1m_exp(log_p)+log(f) ) ),

        #X|X==0 ~ custom( log( (1-p)*(1-f) ) ),
        X|X==0 ~ custom( log1m_exp(log_p) + log1m(f) ),

        # Different link function!
        log_p <- log_inv_logit( a + z[mom_id]*sigma + x[dyad_id]*tau ),
        a ~ normal(0,1.5),
        z[mom_id] ~ normal(0,1),
        sigma ~ normal(0,1),
        x[dyad_id] ~ normal(0,1),
        tau ~ normal(0,1)
    ) , data=dat , chains=4 , cores=4 , iter=4000 ,
    constraints=list(sigma="lower=0",tau="lower=0") )

# Stancode
stancode(mX3)

# Stan version
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "17", "5c.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() 
fit$diagnostic_summary()

# Posterior draws
#
post4 <- fit$draws(format = "data.frame") 
