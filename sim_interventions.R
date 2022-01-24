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
d$W <- d$weight
d$S <- d$male + 1
d$H <- d$height

# Q2: causal effect of S on W 
# Functional relationship: W=f_W(S)

# Model definition
# W_i ~ normal(mu_i, sigma)
# mu_i = alpha_S[i]
# sigma ~ exponential(1)

# Data list
#
dat_ls <- list(N=nrow(d), K=2, W=d$W, S=d$S)

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

# Contrast distribution (mean weight)
#
mu_cont <- samples[,"alpha[1]"] - samples[,"alpha[2]"]
# Causal contrast in means (men, women)
plot(density(mu_cont))

# Contrast distribution (predicted weight) 
# (Pointwise differences in PPDs)
#
W_cont <-  samples[,"W_tilde[1]"] - samples[,"W_tilde[2]"]
plot(density(samples[,"W_tilde[1]"]))
plot(density(samples[,"W_tilde[2]"]))
plot(density(W_cont))

# Proportion above zero
sum(W_cont > 0) / nrow(samples) 
# Proportion below zero
sum(W_cont < 0) / nrow(samples) 