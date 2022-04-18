
# 2M1 ---------------------------------------------------------------------

y <- c(1,1,1)
y <- c(1,1,1,0)
y <- c(0,1,1,0,1,1)
y <- rbinom(100, 1, 0.73)
N <- length(y)

dat_list <- list(
                 y = y,
                 N = N
)
mdl.stan <- "
data{
    int<lower=0> N;
    int<lower=0,upper=1> y[N];
}
parameters{
    real<lower=0,upper=1> p;
}
model{
    y ~ bernoulli(p);
    p ~ uniform(0,1);
}
"
fit <- rstan::stan(model_code=mdl.stan, data=dat_list)
smpl <- rstan::extract(fit)
hist(smpl$p, xlab="Probability of Water", 
     ylab="number of ways the data can happen")

# 2M2 ---------------------------------------------------------------------




# 2M3 ---------------------------------------------------------------------

#
# Analytic solution
#

# Generative process
#
N <- 1e4
(card <- rbinom(N, 2, 1/3))
(ifelse(card==2, 1, rbinom(1, 1, 1/2)))

# Probability of Water on Earth
Pr_LE <- 0.3 
# Probability of Earth
Pr_E <- 0.5
# Probability of L 
Pr_L <- 0.15 + 0.5
# Probability of Earth|Land
(Pr_EL <- (Pr_LE * Pr_E) / Pr_L)

#
# Simulated version
#

# Generative process
#
sample_combi <- function(){
    earth <- sample(1:2, 1)
    if(earth==1){
        c(earth, rbinom(1, 1, .3))
    } else {
        c(earth, 1)
    }
}
N <- 1e5 
combis <- replicate(N, sample_combi())
planet <- combis[1,]
land <- combis[2,]
sum(planet == 1 & land==1) / sum(land==1)


# 2M4 ---------------------------------------------------------------------

sim_card <- function(){
    deck <- 1:3
    card <- sample(deck, 1)
    sides <- matrix(c(1,1,1,0,0,0),2,3)[,card]
    sample(sides)
}
N <- 1e4
cards <- replicate(N, sim_card())
up <- cards[1,]
down <- cards[2,]
sum(up == 1 & down == 1) / sum(up==1)

# 2M5 ---------------------------------------------------------------------

# Generative process 
#
sim_card <- function(){
    deck <- 1:4
    card <- sample(deck, 1)
    sides <- matrix(c(1,1,1,0,0,0,1,1),2,4)[,card]
    sample(sides)
}
N <- 1e4
cards <- replicate(N, sim_card())
up <- cards[1,]
down <- cards[2,]
sum(up == 1 & down == 1) / sum(up==1)

# 2M6 ---------------------------------------------------------------------

# Generative process 
#
sim_card <- function(){
    deck <- 1:6
    card <- sample(deck, 1)
    sides <- matrix(c(1,1,1,0,1,0,0,0,0,0,0,0),2,6)[,card]
    sample(sides)
}
N <- 1e4
cards <- replicate(N, sim_card())
up <- cards[1,]
down <- cards[2,]
sum(up == 1 & down == 1) / sum(up==1)

# E exercises -------------------------------------------------------------

# Requires a specific set of samples

# Generative proces
#
N <- 6 
trials <- 9
y <- rbinom(N, trials, 0.73)

dat_list <- list(
  y = y,
  N = length(y)
)
mdl.stan <- "
data{
  int<lower=0> N;
  int<lower=0> y[N];
}
parameters{
 real<lower=0, upper=1> p;
}
model{
  y ~ binomial(9, p);
  p ~ uniform(0,1);
}
"
fit <- rstan::stan(model_code = mdl.stan, data = dat_list)
samples <- rstan::extract(fit)$p
hist(samples) # Pretty good approximation

# 3E1 ---------------------------------------------------------------------

dsamples <- density(samples)
plot(dsamples, xlim=c(0,1)) ; abline(v=0.2)

N_samples <- length(samples)
boundary <- 0.5
sum(samples < boundary)/N_samples
# Mass 0.5%

# 3E2 ---------------------------------------------------------------------

dsamples <- density(samples)
plot(dsamples) ; abline(v=0.8)

N_samples <- length(samples)
boundary <- 0.8
sum(samples < boundary)/N_samples
# Mass: 98% 

# 3E3 ---------------------------------------------------------------------

dsamples <- density(samples)
plot(dsamples, xlim=c(0,1)) ; abline(v=0.2) ; abline(v=0.8)

N_samples <- length(samples)
boundary <- c(0.2, 0.8)
sum(samples > boundary[1] & samples < boundary[2]) / N_samples
# Mass: 98%

# 3E4 ---------------------------------------------------------------------

mass <- 0.2
(qv <- quantile(samples, mass))

# Quantile value: 0.63
dsamples <- density(samples)
plot(dsamples) ; abline(v=qv)

# 3E5 ---------------------------------------------------------------------

mass <- 0.2
(qv <- quantile(samples, 1-mass))

# Quantile value: 0.63
dsamples <- density(samples)
plot(dsamples) ; abline(v=qv)

# 3E6 ---------------------------------------------------------------------

mass <- 0.66
hpdi <- rethinking::HPDI(as.vector(samples), prob=mass)

dsamples <- density(samples)
plot(dsamples) ; abline(v=hpdi)

# 3E7 ---------------------------------------------------------------------

mass <- 0.66
# ASM: equal PP below and above the interval
pi <- rethinking::PI(as.vector(samples), prob=mass)

dsamples <- density(samples)
plot(dsamples) ; abline(v=pi)

# 3M1 ---------------------------------------------------------------------

tosses <- 15 
w <- 8 ; l <- tosses - w 
y <- c(rep(0,l), rep(1,l))

dat_list <- list(
  y = y,
  N = length(y)
)

mdl.stan <- "
data{
  int<lower=0> N;
  int<lower=0, upper=1> y[N];
}
parameters{
  real<lower=0, upper=1> p;
}
model{
  p ~ uniform(0,1);
  y ~ bernoulli(p);
}
"
#
# HCOIT
# Hairy caterpillar ocular inspection test
fit <- rstan::stan(model_code = mdl.stan, data = dat_list)
rstan::traceplot(fit)
pairs(fit) ; print(fit)
# Samples from the Posterior 
#
samples <- rstan::extract(fit)$p
# Visualize the Posterior
#
dsamples <- density(samples) 
plot(dsamples)

# 3M2 ---------------------------------------------------------------------

boundary <- 0.9
(hpdi <- rethinking::HPDI(as.vector(samples), boundary))

# Visualize: Posterior & HPDI
#
dsamples <- density(samples) 
plot(dsamples) ; abline(v=hpdi)

# 3M3 ---------------------------------------------------------------------

# PPC
# Posterior Predictive Checks
N <- 1e4
ppd <- rbinom(N, tosses, prob = samples)
plot(table(ppd))

# Standardize 
#
plot(table(ppd) / N) # around 15%
# table(ppd == 8)/N
(table(ppd)/N) * 100

# 3M4 ---------------------------------------------------------------------

# PPD
#
tosses <- 9
ppd_ast <- rbinom(N, tosses, prob = samples)
plot(table(ppd_ast))

# Standardize 
#
plot(table(ppd_ast) / N) # around 15%
# table(ppd == 6)/N 
(table(ppd_ast)/N) * 100

# 3M4 ---------------------------------------------------------------------

# Posterior Predictive Checks
# ...this means simulate the distribution of samples, averaging over the
# posterior uncertainty in p

tosses <- 15 
w <- 8 ; l <- tosses - w 
y <- c(rep(0,l), rep(1,l))

dat_list <- list(
  y = y,
  N = length(y)
)

mdl_2.stan <- "
data{
  int<lower=0> N;
  int y[N];
}
parameters{
  real<lower=0,upper=1> p;
}
model{
  y ~ bernoulli(p);
  if (p < 0.5)  
    p ~ uniform(0.5, 1);
  else 
    p ~ normal(0.7,0.1);
}
"
fit_2 <- rstan::stan(model_code = mdl_2.stan, data = dat_list)



# Pre
#
fits <- list(fit, fit_2)
samples <-  lapply(fits, function(ps) rstan::extract(ps)$p)
dsamples <- lapply(samples, density)
N_samples <- lapply(samples, length)
IODB <- function(samples, boundary, N_samples) {
    sum(samples < boundary)/N_samples
}
IODB_2 <- function(samples, N_samples) {
     sum(samples > boundary[1] & samples < boundary[2]) / N_samples
}
calc_qv <- function(samples, mass) quantile(samples, mass)

# IODB (Graphic)
# 
plot(dsamples[[1]], ylim=c(0,6)) 
    lines(dsamples[[2]], lty=2)
boundary <- c(0.2, 0.8)
    abline(v=boundary, lty=3, lwd=0.5)

# IODB (Calc)
# 
boundary <- 0.5
mapply(IODB, samples, boundary, N_samples)
# Mass: 0.5% and 0.1%


boundary <- 0.8
mapply(IODB, samples, boundary, N_samples)
# Mass: 98% 

# IODB
#
boundary <- c(0.2, 0.8)
mapply(IODB_2, samples, N_samples)
# Mass: 98%

# CI
#
mass <- 0.2
(qvs <- lapply(samples, calc_qv, mass=mass))

plot(dsamples[[1]], ylim=c(0,6)) 
    lines(dsamples[[2]], lty=2)
    abline(v=qvs, lty=3, lwd=0.5)
# Quantile value: 0.63

# CI
#
mass <- 0.2
(qvs <- lapply(samples, calc_qv, mass=1-mass))

plot(dsamples[[1]], ylim=c(0,6)) 
    lines(dsamples[[2]], lty=2)
    abline(v=qvs, lty=3, lwd=0.5)

# CI
#
mass <- 0.66
calc_hpdi <- function(samples, mass) {
    rethinking::HPDI(as.vector(samples), prob=mass)
}
hpdis <- lapply(samples, calc_hpdi, mass)

plot(dsamples[[1]], ylim=c(0,6)) 
    lines(dsamples[[2]], lty=2)
lapply(hpdis, function(hpdi) abline(v=hpdi,lty=c(3,4), lwd=0.5 ))

# PI
#
calc_pi <- function(samples, mass) {
    rethinking::PI(as.vector(samples), prob=mass)
}
mass <- 0.66
# ASM: equal PP below and above the interval
pis <- lapply(samples, calc_pi, mass) 
plot(dsamples[[1]], ylim=c(0,6)) 
    lines(dsamples[[2]], lty=2)
lapply(pis, function(pi) abline(v=pi,lty=c(3,4), lwd=0.5 ))


# 3M5 ---------------------------------------------------------------------

pi_diff <- vector(length=1e6)
N <- 1

repeat{
    (N <- N+1)
    trials <- 1 
    y <- rbinom(N, trials, 0.73)
    dat_list <- list(
                     y = y,
                     N = length(y)
    )
    mdl.stan <- "
    data{
        int<lower=0> N;
        int<lower=0, upper=1> y[N];
    }
    parameters{
        real<lower=0, upper=1> p;
    }
    model{
        p ~ uniform(0,1);
        y ~ bernoulli(p);
    }
    "
    #
    # HCOIT
    # Hairy caterpillar ocular inspection test
    fit <- rstan::stan(model_code = mdl.stan, data = dat_list)
    # Samples from the Posterior 
    #
    samples <- rstan::extract(fit)$p
    mass <- 0.99
    # ASM: equal PP below and above the interval
    (pi <- rethinking::PI(as.vector(samples), prob=mass))
    (pi_diff[N] <- pi[2] - pi[1])
    if(pi_diff[N] < 0.05 ) break
}
plot(seq(N), pi_diff[seq(N)])

# 4M1 ---------------------------------------------------------------------

# Prior Predictive Simulation (R)
# y ~ normal(mu, sigma)
# mu ~ normal(0,10)
# sigma ~ exponential(1)
N <- 1e4
sample_mu <- rnorm(N,0,10)
sample_sigma <- rexp(N, 1)
sample_y <- rnorm(N, mean=sample_mu, sd=sample_sigma)
plot(density(sample_y))

# Prior predictive checks (Stan)
# y ~ normal(mu, sigma)
# mu ~ normal(0,10)
# sigma ~ exponential(1)
dat_ls <- list(N=1e4)
file <- file.path(getwd(), "stan", "mdl4M1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls, fixed_param = TRUE)
fit$print()
y_sim <- fit$draws(format = "matrix", variables = "y_sim")
plot(density(y_sim))

# 4M4 ---------------------------------------------------------------------

# PPS
# alpha ~ normal( N, 150, 20 )
# beta ~ normal( N, 0, 10 )

N <- 1e2
# Ren Keyu 221.03 cm (world largest teenager 14 yrs old)
alpha <- rnorm(N,150,20)
curve(dnorm(x,150,20), from=0, to=300, xlab="height")

beta <- rnorm(N, mean = 0, sd = 10)
curve(dlnorm(x,0,1.5), from=-2, to=10, xlab="timepoint", ylab="log height")

plot(NULL, xlim=c(1,3), ylim=c(-50,300), 
     xaxt="n", xlab="timepoint", ylab="height", pch=20)
axis(side=1, at = 1:3)
abline(h=c(0,221), lty=2)
for (i in 1:N) curve( alpha[i] + beta[i] * x, add=TRUE )

# 4M5 ---------------------------------------------------------------------

N <- 1e2
# Ren Keyu 221.03 cm (world largest teenager 14 yrs old)
alpha <- rnorm(N,150,20)
curve(dnorm(x,150,20), from=0, to=300, xlab="height")
beta <- rlnorm(N, meanlog = 0, sdlog = 1.5)
# Switched to a lognormal prior since students height increases every year.
# Thus, there are no negative association between timepoints and heights.
curve(dlnorm(x,0,1.5), from=-2, to=10, xlab="timepoint", ylab="log height")

plot(NULL, xlim=c(1,3), ylim=c(-50,300), 
     xaxt="n", xlab="timepoint", ylab="height", pch=20)
axis(side=1, at = 1:3)
abline(h=c(0,221), lty=2)
for (i in 1:N) curve( alpha[i] + beta[i] * x, add=TRUE )

# 4M6 ---------------------------------------------------------------------

N <- 1e2
# Ren Keyu 221.03 cm (world largest teenager 14 yrs old)
alpha <- rnorm(N,150,20)
curve(dnorm(x,150,20), from=0, to=300, xlab="height")
beta <- rlnorm(N, meanlog = 0, sdlog = 1.5)
curve(dlnorm(x,0,1.5), from=-2, to=10, xlab="timepoint", ylab="log height")

plot(NULL, xlim=c(1,3), ylim=c(-50,300), 
     xaxt="n", xlab="timepoint", ylab="height", pch=20)
axis(side=1, at = 1:3)
abline(h=c(0,221), lty=2)
for (i in 1:N) curve( alpha[i] + beta[i] * x, add=TRUE )
#
# Sigma note: choose a uniform prior (instead of a exponential prior), because
# we have information on the maxmimum value of deviation (64 cm)

# 4M7 ---------------------------------------------------------------------

library( rethinking )
data( "Howell1" )
d <- Howell1 ; d2 <- d[d$age >= 18,]
dat_ls <- list(
               N = nrow(d2),
               w = d2$weight,
               h = d2$height
)

# Definition 
#
fml_1 <- file.path(getwd(), "stan", "mdl32.stan")
mdl_1 <- cmdstanr::cmdstan_model(fml_1, pedantic=TRUE)
fit_1 <- mdl_1$sample(
  data = dat_ls, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500
)

fml_2 <- file.path(getwd(), "stan", "mdl4M7.stan")
mdl_2 <- cmdstanr::cmdstan_model(fml_2, pedantic=TRUE)
fit_2 <- mdl_2$sample(
  data = dat_ls, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500
)

# Diagnostics
#
fit_1$cmdstan_diagnose()
# If we don't standardize the variables, we get ~600 divergent transitions!
# Thus the we get a worse aproximation of the posterior..
fit_2$cmdstan_diagnose()

# 4M8 ---------------------------------------------------------------------

# Data wrangling
library(rethinking)
data("cherry_blossoms")
d <- cherry_blossoms
dcc <- d[complete.cases(d$doy), ]

# Spline
N_knots <- 100 
# Evenly spaced quantile values
probs <- seq(0,1,length.out=N_knots)
knots <- quantile(dcc$year, probs) 
# B-Spline Basis Matrix
B <- splines::bs(dcc$year, knots = knots[-c(1,N_knots)],  degree=3, 
                 intercept=TRUE)

# Prepare a data list
dat_ls <- list(
  B = B,
  N = nrow(dcc),
  K = ncol(B),
  D = dcc$doy
)
# Define & fit the model
fml <- file.path(getwd(), "stan", "mdl4M8.stan")
mdl <- cmdstanr::cmdstan_model(fml)
fit <- mdl$sample(data=dat_ls)
# Evaluate
fit$cmdstan_diagnose()
fit$print(variables="w")
# Extract
mu <- fit$draws(variables = "mu", format = "matrix")

# Posterior line predictions 
#
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)

# Visualize
#
plot( dcc$year, dcc$doy, xlab="year", ylab="Day in year", 
      col=col.alpha(rangi2, 0.3), pch=16 )
lines(dcc$year, mu_mean, lwd=1.5)
rethinking::shade(mu_HPDI, dcc$year, col=col.alpha("black", 0.5))
# 
# Decresing/incresing N_knots decreases/increases the wigglyness of the spline
# N_knots = 50 vs. N_knots = 15
# Narrowing/flatten the (sd) via the prior decreases/increases the wigglyness
# w ~ normal(0,100) vs. w ~ normal(0,1)

# 5M1 ---------------------------------------------------------------------

dag <- dagitty::dagitty('dag {
                        X [pos="0,0"]
                        Y [pos="1,0"]
                        Z [pos="0,1"]
                        Z <- X -> Y 
}')
plot(dag)

# Functional relationships
#
N <- 1e3
X <- rnorm(N)
# Z: common cause of X & Y
Y <- rnorm(N, X)
Z <- rnorm(N, -X)
cor(data.frame(X,Y,Z))

# Sketch
#
# Y ~ normal(mu_i, sigma)
# mu_i = alpha + beta_X * X_i + beta_Z * Z_i
# alpha ~ normal(0, 0.2)
# beta_X ~ normal(0, 0.5)
# beta_Y ~ normal(0, 0.5)
# sigma ~exponential(1)

# PPS
#
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
# Relationship X,Y (same for Z,Y)
alpha <- rnorm(N, 0, 0.2)
beta_X <- rnorm(N, 0, 0.5)
for(i in 1:100) abline(a = alpha[i], b = beta_X[i],  
                       col=scales::alpha("steelblue", .3))

# Reduction
#
dat_ls <- list(N=N,X=X,Y=Y,Z=Z)

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_5M1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format="data.frame")
bayesplot::mcmc_trace(samples)

# Inferene
#
par(mfrow=c(1,2))
plot(X,Z, pch=20, col=scales::alpha("steelblue", .3))
for(i in 1:100) abline(a=samples$alpha[i], b=samples$beta_Z[i], 
                       col=scales::alpha("black", 0.3))
title("Z & Y | Z")
plot(X,Y, pch=20, col=scales::alpha("steelblue", .3))
for(i in 1:100) abline(a=samples$alpha[i], b=samples$beta_X[i], 
                       col=scales::alpha("black", 0.3))
title("X & Y | Z")
par(mfrow=c(1,1))

# 5M2 ---------------------------------------------------------------------

# Masked associations
#
dag <- dagitty::dagitty('dag {
                        X [pos="0,0"]
                        Y [pos="0.5,1"]
                        Z [pos="1,0"]
                        X -> Y <- Z
                        X -- Z
}')
plot(dag)

# Functional relationships
#
N <- 1e3
X <- rnorm(N)
Z <- rnorm(N, X)
Y <- rnorm(N, -X+Z)
# Check correlations
cor(data.frame(X,Y,Z))

# Model Sketch
#
# Y_i ~ Normal(mu_i, sigma)
# mu_i = alpha + beta_X*X + beta_Z*Z 
# alpha ~ normal(0, 0.2) 
# beta_X ~ normal(0, 0.5) 
# beta_Z ~ normal(0, 0.5) 

# Reduction
#
dat_ls <- list(N=N,X=X,Z=Z,Y=Y)

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_5M3.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format = "data.frame")
bayesplot::mcmc_trace(samples)

# Summary
#
par(mfrow=c(1,2))
plot(X, Y,pch=20, col=scales::alpha("black", .3))
for(i in 1:100) abline(a=samples$alpha[i], b=samples$beta_X[i],
                       col=scales::alpha("steelblue", .3))

plot(Z, Y,pch=20, col=scales::alpha("black", .3))
for(i in 1:100) abline(a=samples$alpha[i], b=samples$beta_Z[i],
                       col=scales::alpha("steelblue", .3))
par(mfrow=c(1,1))

# 5M3 ---------------------------------------------------------------------

# DAG
# Idea: The higher the divorce rate the more singles around, and the more
# chances to find another partner and get married.
#
dag <- dagitty::dagitty('dag {
D [pos="1,0"]
M [pos="0.5,1"]
A [pos="0,0"]
D <- A -> M 
D -> M
}')
plot(dag)

# Data
#
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

# System sketch
#
# M_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_D*D_i + beta_A*A_i
# alpha ~ normal(0, 0.2)
# beta_D ~ normal(0, 0.5)
# beta_A ~ normal(0, 0.5)
# sigma ~ exponential(1)
# 
# D_i ~ normal(mu_i, sigma)
# mu_i = nu + beta_A*A_i
# eta ~ normal(0, 0.2)
# theta ~ normal(0, 0.5)
# tau ~ exponential(1)

# Date wrangling
#
d$M <- scale(d$Marriage)
d$D <- scale(d$Divorce)
d$A <- scale(d$MedianAgeMarriage)

# Reduction
#
dat_ls <- list(n=nrow(d), M=as.numeric(d$M), D=as.numeric(d$D), 
               A=as.numeric(d$A))

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_4M3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format="data.frame")

# Posterior inference
#
# Counterfactual effect 
# ...of manipulating D | DAG
#
N_sim <- 1e3
D <- seq(-2,2, length.out=N_sim)

sim_M <- function(D, A=0) {
    with(samples, {
             rnorm(N_sim, 
             mean=alpha + beta_D*D + beta_A*A, 
             sd=sigma
             )})
}
M_tilde <- sapply(D, FUN=sim_M, A=0)

plot(D, colMeans(M_tilde), pch=20, xlab="Manipulated D", ylab="Counterfactual M",
col=scales::alpha("steelblue", .7))

# Causal Inference | DAG
#
plot(d$M ~ d$D,pch=20, xlab="Divorce rate", ylab="Marriage rate")
for(i in 1:100) abline(a=samples$alpha[i], b=samples$beta_D[i], 
                       col=scales::alpha("steelblue", .3))

# 5M4 ---------------------------------------------------------------------

library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

# DAG
#
dag <- dagitty::dagitty('dag {
D [pos="1,0"]
M [pos="0,0"]
A [pos="0,.5"]
L [pos="0,1"]
A -> M -> D
A -> D
L -> A 
L -> A -> M -> D 
L -> D
                        }')

# Data wrangling
#
# CSV from source
# https://worldpopulationreview.com/state-rankings/mormon-population-by-state
d2 <- read.csv("~/Downloads/csvData.csv")
colnames(d2)[1] <- "Location"
d3 <- merge(x = d, y = d2)
d3$p_mormons <- (d3$mormonPop/d3$Pop)*100

# Standardization
#
D <- scale(d3$Divorce) 
L <- scale(d3$p_mormons)
A <- scale(d3$MedianAgeMarriage)
M <- scale(d3$Marriage)

# Reduction
#
dat_ls <- list(n=nrow(d3), D=as.numeric(D), L=as.numeric(L), A=as.numeric(A),
               M=as.numeric(M))

# Model sketch
#
# D_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_L*L + beta_A*A + beta_M*M  
# alpha ~ normal(0, 0.2) 
# beta_L ~ normal(0, 0.5)
# beta_A ~ normal(0, 0.5)
# beta_M ~ normal(0, 0.5)
# sigma ~ exponential(1)

# Fit 
#
file <- file.path(getwd(), "stan", "mdl_5M4.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnistics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format = "data.frame")
bayesplot::mcmc_trace(samples)
round(cor(samples), digits = 2)

#
#
N_sim <- 1e3
s <- seq(-3,8, length.out=N_sim)
sim_D <- function(s, n=N_sim) {
  with(samples, {
    rnorm(
      n=N_sim, 
      mean=alpha + beta_L*s + beta_A*s + beta_M*s,
      sd=sigma)})
  }
D_tilde <- vapply(s, sim_D, FUN.VALUE = double(N_sim))

# Visual inference 
#
plot(NULL, xlim=c(-0.5,6), ylim=c(-6,2), xlab="LD-Saint rate (std)",
     ylab="Divorece rate (std)")
for(i in 1:200) lines(s, D_tilde[i,], col=scales::alpha("grey", .2))
points(d3$D ~ d3$L, pch=20)
for(i in 1:200) abline(a=samples$alpha[i], b=samples$beta_L[i], 
                       col=scales::alpha("steelblue", 0.3), lwd=4)
abline(a=mean(samples$alpha), b=mean(samples$beta_L), col="white", lwd=2)
title("L ~ D | A, M")

# 5M5 ---------------------------------------------------------------------

# DAG
#
dag <- dagitty::dagitty('dag{
gas_price [pos="0,0"] 
driving [pos="0,.5"] 
exercise [pos="0,1"]
eating_out [pos="1,1"]
meal_size [pos="1,1.5"]
obesity [pos="0,2"] 
gas_price -> driving -> exercise -> obesity
gas_price -> driving -> eating_out -> meal_size -> obesity
}')
plot(dag)

# A regression which estimates the influence of gas_price on obesity is done
# thorugh the following model:
# obesity_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_gas_price
# alpha ~ normal(0, 0.2)
# beta_gas_price ~ normal(0, 0.5)
# sigma ~ exponential(1)

# Since there are no "adjustment sets", the influence can be directly estimated
adjustmentSets(dag, exposure="gas_price", outcome="obesity")
# Conditionng on any predictor blocks the TX-effect (pipe)

# 5H1 ---------------------------------------------------------------------

# Dag
#
dag <- dagitty::dagitty('dag { M -> A -> D }')

# Implied conditional independencies
#
dagitty::impliedConditionalIndependencies(dag)
# D _||_ M | A

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
lm(d$Divorce ~ d$Marriage)
lm(d$Divorce ~ d$Marriage + d$MedianAgeMarriage)
# Effect greatly does diminish! 

# 6M2 ---------------------------------------------------------------------

# Draw a DAG
#
dag <- dagitty::dagitty("dag{X -> Z -> Y}")
plot(dag)

# Fake data 
#
N <- 1e2
X <- rnorm(N)
Z <- rnorm(N, 10*X)
Y <- rnorm(N, 1*Z)
d <- data.frame(X,Z,Y)
round(cor(d), digits=2)

# Model sketch
#
# Y ~ normal(mu, sigma)
# mu_i = alpha + beta_1* X_i + beta_2*Z_i
# alpha ~ normal(0.0.2)
# beta_1 ~ normal(0.0.5)
# beta_2 ~ normal(0.0.5)
# sigma ~ exponential(1)

# Reduction
#
dat_ls <- list(N=nrow(d), X=X, Y=Y, Z=Z)

# Fitting
#
file <- file.path(getwd(), "stan", "exercises", "mdl_6M2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# 7E2 ---------------------------------------------------------------------

p <- c(0.7, 0.3)
-sum(p * log(p))

# 7E3 ---------------------------------------------------------------------

p <- c(0.2, 0.25, 0.25, 0.3)
-sum(p * log(p))

# 7E4 ---------------------------------------------------------------------

# Since log(0) = -Inf the result is NaN. But using the rule of l'hopital,
# log(0) = 0 and we can caluclate the entropy as follows:

p <- c(0, rep(0.2,5))
-sum(p * ifelse(p==0, 0, log(p)))

# 7M3 ---------------------------------------------------------------------

# Simulate data
#
# Note: Simulate correlated variables using a cholesky factorization
# Correlation matrix
R <- matrix(c(1,0.7,0.7,1),2,2)
# Cholesky decomposition (R=L^TL)
L <- chol(R)
# t(L) %*% L # since: R=L^T*L
# Matrix of standard deviations
sigma <- diag(c(1,2))
# Multiply sigma with the lower triangular square root of the correlation
# matrix: t(L)
Lambda <- sigma %*% t(L)
# RNG
set.seed(123)
N <- 1e2
# z ~ normal(0,1)
Z <- rbind(rnorm(N),rnorm(N))
X <- Lambda %*% Z
# rho
# cor(X[1,], X[2,])

# Helper function
#
oosd_psis <- function(LL) {
  out_ls <- list()
  rel_n_eff <- loo::relative_eff(exp(LL))
  values <- loo::loo(LL, r_eff=rel_n_eff, is_method="psis")
  lppd <- values$pointwise[,"elpd_loo"]
  p_waic <- values$pointwise[,"p_loo"]
  # Deviance (pointwise)
  out_ls$pointwise <- -2*(lppd - p_waic)
  # Deviance (sum)
  out_ls$sum <- sum(out_ls$pointwise)
  # LL's dim: NxMxK where K equals nrow(d)
  # ..i.e., makes "n" in "function(LL,n) where "n <- nrow(d)" redundant!
  n_cases <- dim(LL)[3]
  # Standard error
  out_ls$se <- sqrt(n_cases * var(out_ls$pointwise))
  out_ls
}
options(mc.cores = 4)

# OOSD_PSIS (N=100)
#
dat_ls <- list(N=N, x=X[1,], y=X[2,])
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "exercises", "mdl_7M3.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)
# log likelihood array only!
LL <- fit$draws("log_lik")
oosd_psis(LL)
# 
# $sum
# [1] 358.8503
# 
# $se
# [1] 17.14617
# 

# Simulation function
#
simulatr <- function(s) {
    dat_ls <- list(N=s, x=X[1,seq(s)], y=X[2,seq(s)] )
    path <- "~/projects/stanmisc"
    file <- file.path(path, "stan", "exercises", "mdl_7M3.stan") 
    mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
    fit <- mdl$sample(dat_ls)
    # log likelihood array only!
    LL <- fit$draws("log_lik")
    oosd_psis(LL)
}
N_sims <- 100
out_ls <- parallel::mclapply(seq(2,N_sims), simulatr)

# Visualize
#
plot(NULL, xlim=c(2,100), ylim=c(0,375), xlab="Observations (N)", 
     ylab="PSIS +/-SE")
add_lines <- function(i) {
    y_1 <- out_ls[[i]]$sum - out_ls[[i]]$se
    y_2 <- out_ls[[i]]$sum + out_ls[[i]]$se
    points(i, out_ls[[i]]$sum, pch=20, cex=.5)
    lines(c(i,i), c(y_1, y_2), lty=2)
}
lapply(seq(2:N), add_lines)

# 7M4 ---------------------------------------------------------------------

# Note: Cant directly write a simulation function -- can't use map2stan
# ...and I don't find it necessary to learn it yet. So I play arround in R.

# Model sketch
#
# y ~ normal(mu, sigma)
# mu = alpha + beta_x * x
# alpha ~ normal(0,0.2)
# beta ~ normal(0,100)
# beta ~ normal(0,10)
# beta ~ normal(0,1)
# beta ~ normal(0,0.1)
# sigma ~ exponential(1)

# Reduction
# Note: Use dat_ls from 7M3
#

# Fit
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "exercises", "mdl_7M3.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostic
# 
fit$cmdstan_diagnose()
fit$print()

# Samples
#
# log likelihood array only!
LL <- fit$draws("log_lik")

# p_loo
#
rel_n_eff <- loo::relative_eff(exp(LL))
values <- loo::loo(LL, r_eff=rel_n_eff, is_method="psis")
# p_loo
sum(values$pointwise[,"p_loo"])
# Note: p_loo gets smalller! Since in an ordinary linear regression the sum of
# all parameter values tends to be equal to the number of free parameters in
# the model -- for flat priors (AIC)! 











