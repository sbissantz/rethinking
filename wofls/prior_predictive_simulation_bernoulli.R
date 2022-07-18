#
# Prior predictive simulation 
# (Logistic regression model)
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
# Model sketch
#
# y_i ~ bernoulli(p_i)
# aid: actor id ; txid: treatment id
# logit(p_i) = alpha_aid[i] + beta_txid[i]
# alpha_aid[i] ~ normal(0,1.5)
# beta_txid[i] ~ normal(0,0.5)
# Note: I dont simulate alpha as categorical variable. I just do this for beta.
# If you want to do the same for alpha, simple add the categorical predictor to
# the stan model, and use the same code for alpha as you did for beta.

path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "pps_1b.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)

dat_ls <- list(N=nrow(d), tid=d$treatment)
fit <- mdl$sample(data=dat_ls, fixed_param=TRUE)

fit$cmdstan_diagnose()
fit$summary()

# Samples from the posterior 
samples <- fit$draws(format="data.frame")

# Helper function 
sigmoid <- function(x) 1 / (1 + exp(-x))

# alpha (log_odds)
(alpha_lo <- samples$alpha)
# beta (log odds)
(beta_lo <-  fit$draws("beta", format="matrix"))

# alpha (prob)
alpha_p <- sigmoid(alpha_lo)
plot(density(alpha_p))
# beta (prob)
beta_p <- sapply(1:4, function(k) sigmoid(beta_lo[,k]))
op <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
for (i in 1:4) {
    plot(density(beta_p[,i]))
}
par(op)

# Prior line predictive simulation
#
par(mfrow=c(2,2))
for(k in 1:4) {
  plot(c(-4,4), c(0,1), type="n", ylab="Pr(pull_left)",
     xlab="Predictor values", main="Prior line predictive simulation")
  for(i in 1:50) {
    curve(sigmoid(alpha_lo[i] + beta_lo[i,k] * x), from=-4, to=4, add=TRUE)
}}
par(op)

# Prior predictive contrasts between treatments
#
p <- sapply(1:4, function(k) sigmoid(alpha_lo + beta_lo[,k]))
plot(density(( abs( p[,1]  - p[,2]) )))
# Prior predictive difference
mean( abs( p[,1]  - p[,2]) )
# [1] 0.09678439
#...10% average prior difference between Txs 

# Prior predictions
#
# Comprare the distribution of zero and ones across experiments
y_tilde <- fit$draws("y_tilde", format="matrix")

plot(density(y_tilde[1,]), xlim=c(-1, 2), ylim=c(0,35), main="Prior
     predictive", xlab="y_tilde")
for(i in 1:500) {
  lines(density(y_tilde[i,]), col=col.alpha("black", 0.2))
}

# Prior predictions
#
# Comprare minimum and maximum of zero & ones across experiments
N <- 5e2
plot(x=c(1,N), y=c(0,1), type="n")
for(i in 1:N) { 
  y_cord_1 <- length(which(y_tilde[i,] == 0)) / 504
  y_cord_2 <- length(which(y_tilde[i,] == 1)) / 504
  points(rep(i,2), c(y_cord_1, y_cord_2), pch=20, cex=.4)
  lines(rep(i,2), c(y_cord_1, y_cord_2), pch=20, lwd=.5, col="steelblue")
}

# 
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_1.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)

(ano <- length(unique(d$actor)))
(tno <- length(unique(d$treatment)))
dat_ls <- list(N=nrow(d), ano=ano, tno=tno, tid=d$treatment, aid=d$actor, y=d$pulled_left)
fit <- mdl$sample(data=dat_ls)

fit$cmdstan_diagnose()
fit$cmdstan_summary()
fit$summary(variables=c("alpha", "beta"))

# Samples from the posterior 
samples <- fit$draws(format="data.frame")
# Helper function 
# sigmoid <- function(x) 1 / (1 + exp(-x))

# alpha & beta on the probability scale
(alpha_p <-  fit$draws("alpha_p", format="matrix"))
(beta_p <-  fit$draws("beta_p", format="matrix"))

# Visualize
op <- par(no.readonly=TRUE)
# alpha
par(mfrow=c(3,3))
for(i in 1:ano) {
 plot(density(alpha_p[,i])) 
}
# beta
par(mfrow=c(2,2))
for(i in 1:tno) {
 plot(density(beta_p[,i])) 
}
par(op)



