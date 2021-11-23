
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




head(pi_diff, n=1e4)



