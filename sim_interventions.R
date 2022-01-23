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

# Fitting
#

file <- file.path( getwd(), "stan", "mdl_1_sim_intervent.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample

#
#













