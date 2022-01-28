#
# Causal inference as some special kind of prediction 
# (i.e., correctly predicting the consequences of an intervention)
#
#
# Simulating interventions
#

# DAG
#
# S: S influences height & weight
# H=f_H(S): H is influenced by S & influences W
# W=f_W(S,W): W is influences by sex & height 
dag <- dagitty::dagitty('dag {
 H [pos="0,0"]
 W [pos="1,0"]
 S [pos="0,1"]
 S->H->W
 S->W
}')
plot(dag)

# Causal questions
#
# (Q1: causal effect of H on W | Stats mdl: W=f_W(H))
# ..we're only interessted on effect of sex
#
# Q2: causal effect of S on W | Stats mdl: W=f_W(S)
# Q3: Direct causal effect of S on W | Stats mdl: W=f_W(H,S)
#

# Data wrangling
#
library(rethinking)
data(Howell1)
d <- Howell1
d <- d[d$age>=18,]
d$W <- scale(d$weight)
d$S <- d$male + 1
# d$H <- d$height

plot(d$W, col=ifelse(d$S==1, "pink", "blue"), pch=20)
abline(h=c(mean(d$W[d$S==1])))
abline(h=c(mean(d$W[d$S==2])))

# Q2: causal effect of S on W 
# Functional relationship: W=f_W(S)

# Model definition
# W_i ~ normal(mu_i, sigma)
# mu_i = alpha_S[i]
# sigma ~ exponential(1)

# Data list
#
dat_ls <- list(N=nrow(d), K=2, W=as.numeric(d$W), S=as.integer(d$S))

# Fitting
#
file <- file.path( getwd(), "stan", "mdl_1_sim_intervent.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format="matrix")

# Posterior correlation
#
cor(samples)

# Contrast distribution (mean weight)
# Note: "+" since alpha|1] > 0
mu_cont <- samples[,"alpha[2]"] + samples[,"alpha[1]"]

# Causal contrast in means (men, women)
plot(density(mu_cont))

# Contrast distribution (predicted weight) 
# (samplewise differences in PPDs)
#
N <- 1e4
W_tilde_1 <- rnorm(N, samples[,"alpha[1]"], samples[,"sigma"])
W_tilde_2 <- rnorm(N, samples[,"alpha[2]"], samples[,"sigma"])
W_tilde_cont <- W_tilde_2 - W_tilde_1

# Visualization
#
plot(NULL, xlim=c(-4,4), ylim=c(0,0.5))
  lines(density(W_tilde_1), col="red")
  lines(density(W_tilde_2), col="blue")
    abline(v = colMeans(samples[,"alpha[1]"]), lty=2)
    abline(v = colMeans(samples[,"alpha[2]"]), lty=2)

plot(NULL, xlim=c(-4,4), ylim=c(0,0.5))
  lines(density(W_tilde_cont), )
  polygon(density(W_tilde_cont), col="steelblue")
    abline(v = mean(W_tilde_cont), lty=2, )

# Proportion above zero (men > women)
sum(W_tilde_cont > 0) / N 
# Proportion below zero (women < men)
sum(W_tilde_cont < 0) / N 

# Q2: Direct causal effect of S on W 
# Functional relationship: W=f_W(S, H)

# Model definition
# W_i ~ normal(mu_i, sigma)
# mu_i = alpha_S[i] + beta_S[i]
# sigma ~ exponential(1)

# Data wrangling
# 
library(rethinking)
data(Howell1)
dat <- Howell1
d <- Howell1[dat$age > 18,]
d$W <- scale(d$weight)
d$H <- scale(d$height)
d$S <- d$male + 1

# Date list
#
dat_ls <- list(N=as.integer(nrow(d)), K=2, W=as.numeric(d$W), H=as.numeric(d$H), 
               S=as.integer(d$S))

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_2_sim_intervent.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
# 
samples <- fit$draws(format = "matrix")

# Contrast distribution
#
N_s <- 1e2
x_seq <- seq(-2,2, length.out=N_s)
calc_muF <- function(x_seq) samples[,"alpha[1]"] + samples[,"beta[1]"] * x_seq
# Predicted weight for females
muF <- vapply( x_seq, calc_muF, double(nrow(samples)) )
calc_muM <- function(x_seq) samples[,"alpha[2]"] + samples[,"beta[2]"] * x_seq
# Predicted weight for males 
muM <- vapply( x_seq, calc_muM, double(nrow(samples)) )
# Contrast distribution for the predicted average weight difference
mu_cont <- muM - muF

# Visualize
#
plot( NULL, xlim=c(-2,2), ylim=c(-3,3), xlab="height (std)", 
      ylab="weight contrast F-M (std)")
mu_cont_mean <- apply(mu_cont,2,mean)
for(i in 1:1e2) lines(x_seq, mu_cont[i,], lwd=5, col=alpha("steelblue", 0.1) )
lines(x_seq, mu_cont_mean, lwd=2 )

# Full Luxury Bayes
#

# Causal system
#
# S 
# H=f(S)
# W=f(S,H)
# 
# weight
# W_i ~ normal(mu_i, sigma)
# mu_i = a_S[i] + b_S[i]*(H-Hbar)
# a_S[i] ~ normal(60,10)
# b_S[i] ~ lognormal(0,1)
# sigma ~ uniform(0,10)
# height
# H_i ~ normal(nu_i, tau)
# nu_i = h_S[i] 
# tau ~ uniform(0,10)

library(rethinking)
data("Howell1")
d <- Howell1
d <- d[d$age > 18,]
dat <- list(
  W=d$weight,
  H=d$height,
  Hbar=mean(d$height),
  S=d$male+1
)
dat
m_SHW <- ulam(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S] + b[S]*(H-Hbar),
    a[S] ~ dnorm(60,10),
    b[S] ~ dlnorm(0,1),
    sigma ~ dunif(0,10)
  ), dat=dat, chains = 4, cores = 4 )



