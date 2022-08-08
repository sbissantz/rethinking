#
# Ch. 11: Good spiked the integer
#

# Logistic regression
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
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "pps_1a.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)

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

# Second model – Stan workflow!
#
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
# approx. the same as bayesplot::ppc_dens_obverlay()
y_tilde <- fit$draws("y_tilde", format="matrix")

plot(density(y_tilde[1,]), xlim=c(-1, 2), ylim=c(0,35), main="Prior
     predictive", xlab="y_tilde")
for(i in 1:500) {
  lines(density(y_tilde[i,]), col=col.alpha("black", 0.2))
}

# Prior predictive checks
# (bayesplot version)
y <- d$pulled_left 
yrep <- fit$draws("y_tilde", format="matrix") 
bayesplot::color_scheme_set("brightblue")
bayesplot::ppc_dens_overlay(y, yrep[1:100, ])

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

# Condition the prior on the data!
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_1.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)

(ano <- length(unique(d$actor)))
(tno <- length(unique(d$treatment)))
dat_ls <- list(N=nrow(d), ano=ano, tno=tno, tid=d$treatment, aid=d$actor,
               y=d$pulled_left)
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
(alpha_lo <-  fit$draws("alpha", format="matrix"))
(beta_p <-  fit$draws("beta_p", format="matrix"))
(beta_lo <-  fit$draws("beta", format="matrix"))

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

# Handedness preferences
#
actors <- fit$draws("alpha_p", format="matrix")
actors_mu <- apply(actors, 2, mean)
actors_HPDI <- apply(actors, 2, rethinking::HPDI)
dotchart(actors_mu, xlim=c(0,1), pch=20)
#points(c(actors_HPDI[1,], actors_HPDI[2,]), rep(seq(ncol(actors)),2), pch=20)
for (i in 1:ncol(actors)) {
  lines(c(actors_HPDI[1,i], actors_HPDI[2,i]), rep(i,2), lwd=1, lty=2)
}
mtext("Posterior means & 89% HPDIs")

actors <- fit$draws("alpha_p", format="matrix")
actors_mu <- apply(actors, 2, mean)
actors_HPDI <- apply(actors, 2, rethinking::HPDI)
dotchart(actors_mu, xlim=c(0,1), pch=20)
#points(c(actors_HPDI[1,], actors_HPDI[2,]), rep(seq(ncol(actors)),2), pch=20)
for (i in 1:ncol(actors)) {
  lines(c(actors_HPDI[1,i], actors_HPDI[2,i]), rep(i,2), lwd=1, lty=2)
}
abline(v = .5, lty=3)
mtext("Posterior means & 89% HPDIs")

# Difference between TXs
#
txs <- fit$draws("beta_p", format="matrix")
txs_mu <- apply(txs, 2, mean)
txs_HPDI <- apply(txs, 2, rethinking::HPDI)
dotchart(txs_mu, xlim=c(0,1), pch=20)
#points(c(actors_HPDI[1,], actors_HPDI[2,]), rep(seq(ncol(actors)),2), pch=20)
for (i in 1:ncol(txs)) {
  lines(c(txs_HPDI[1,i], txs_HPDI[2,i]), rep(i,2), lwd=1, lty=2)
}
abline(v = .5, lty=3)
mtext("Posterior means & 89% HPDIs")

# Contrasts distribution
#
# Differences on the log-odds scale
txs <- fit$draws("beta", format="matrix")
# Differences on the probability scale
txs <- fit$draws("beta_p", format="matrix")

# Contrast distributions "cd"
#
(cdtx13 <- txs[,1] - txs[,3])
(cdtx24 <- txs[,2] - txs[,4])

# Mean difference in contrasts
mu_dtx13 <- apply(cdtx13, 2, mean)
mu_dtx24 <- apply(cdtx24, 2, mean)
mu_dtxs <- cbind(mu_dtx13, mu_dtx24)
# Contrast HPDIS
HPDI_dtx13 <- apply(cdtx13, 2, rethinking::HPDI)
HPDI_dtx24 <- apply(cdtx24, 2, rethinking::HPDI)
HPDI_dtxs <- cbind(HPDI_dtx13, HPDI_dtx24)

# Visualize
#
# c(mu_dtxs) to get a numeric vector 
dotchart(c(mu_dtxs), xlim=c(-1,1), labels=c("mu_dtx13", "mu_dtx24"), pch=20) 
for (i in 1:ncol(HPDI_dtxs)) {
  lines(c(HPDI_dtxs[1,i], HPDI_dtxs[2,i]), rep(i,2), lwd=1, lty=2)
}
abline(v=0, lty=3)

# TODO: Posterior predictions p.333
#

# Proportion: How often does each chimpanzee pulled the left lever?
pl <- by(d$pulled_left, list(d$actor, d$treatment), mean)
pl[1,] 

#
# Visualize the obsered data & model expectations
#
par(mfrow=c(2,7))

# Visualize the observed data
#
for(actor in 1:7) { 
  plot(NULL, xlim=c(1,4), ylim=c(0,1), ylab="Proportion left lever") 
  mtext(concat("actor ", actor))
  abline(h=.5, lty=2)
  txs_left <- c(1,3) ; txs_right <- c(2,4)
  lines(txs_left, pl[actor, txs_left]) 
  lines(c(2,4), pl[actor, txs_right])
  txs_np <- c(1,2) ; txs_p <- c(3,4) 
  points(txs_np, pl[actor, txs_np], pch=20)
  points(txs_p, pl[actor, txs_p], pch=20, col="steelblue")
}

# Visualize the models expectations
#
sigmoid <- function(x) 1 / (1 + exp(-x))
# Crux: use an array!
(n_samples <- nrow(samples)) 
post_p <- array(NA, dim=c(ano, tno, n_samples))
for(i in 1:ano) for(j in 1:tno) post_p[i,j,] <- sigmoid(alpha_lo[,i] + beta_lo[,j])
# Crux: calc mean over actors and treatments across all samples 
(post_p_mu <- apply(post_p, c(1,2), mean))
(post_p_HPDI <- apply(post_p, c(1,2), rethinking::HPDI))

for(actor in 1:7) { 
  txs <- 1:4 ; txs_left <- c(1,3) ; txs_right <- c(2,4) 
  txs_np <- c(1,2) ; txs_p <- c(3,4) 
  for(actor in 1:7) { 
  plot(NULL, xlim=c(1,4), ylim=c(0,1), ylab="Proportion left lever")
  mtext(concat("actor ", actor))
  abline(h=.5, lty=2)
  lines(txs_left, post_p_mu[actor, txs_left]) 
  lines(txs_right, post_p_mu[actor, txs_right])
  for (i in txs) points(rep(i, 2), post_p_HPDI[c(1,2),actor,i], type="l")
  points(txs_np, post_p_mu[actor, txs_np], pch=20)
  points(txs_p, post_p_mu[actor, txs_p], pch=20, col="steelblue")
  }
}

#
# Model comparison
#

# Extract LL for model 1
LL1 <- fit$draws("log_lik")
rel_eff1 <- loo::relative_eff(exp(LL1))
PSIS1 <- loo::loo(LL1, r_eff = rel_eff1, is_method="psis")

# Deviance
#
(lppd1 <-  PSIS1$pointwise[,"elpd_loo"])
(p_psis1 <-  PSIS1$pointwise[,"p_loo"])
D1 <- -2*(lppd1-p_psis1)
-2*(sum(lppd1)-sum(p_psis1))
# [1] 548.8448

# Approximate PSIS standard error 
N1 <- dim(LL1)[3]
PSIS_i1 <- -2*(lppd1-p_psis1)
sqrt(N1 * var(PSIS_i1))
# [1] 19.73713

# Model 2
#
# Model without an interaction
d$side <- d$prosoc_left + 1 #right 1, left 2
d$cond <- d$condition + 1 #no partner 1, partner 2

path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_2.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)

(ano <- length(unique(d$actor)))
(sno <- length(unique(d$side)))
(cno <- length(unique(d$cond)))
dat_ls <- list(N=nrow(d), ano=ano, sno=sno, cno=cno, aid=d$actor, sid=d$side,
               cid = d$cond, y=d$pulled_left)
fit2 <- mdl$sample(data=dat_ls)

fit2$cmdstan_diagnose()
fit2$cmdstan_summary()

# Extract LL for model 2
LL2 <- fit2$draws("log_lik")
rel_eff2 <- loo::relative_eff(exp(LL2))
PSIS2 <- loo::loo(LL2, r_eff = rel_eff2, is_method="psis")
# Deviance
(lppd2 <-  PSIS2$pointwise[,"elpd_loo"])
(p_psis2 <-  PSIS2$pointwise[,"p_loo"])
D2 <- -2*(lppd2-p_psis2)
-2*(sum(lppd2)-sum(p_psis2))
# [1] 545.9158
# Approximate PSIS standard error 
N2 <- dim(LL2)[3]
PSIS_i2 <- -2*(lppd2-p_psis2)
# Approximate standard error
sqrt(N2 * var(PSIS_i2))
# [1] 19.83971

# Comparing the differences
#
sum(D_diff <- D1 - D2)
N1 <- dim(LL1)[3]
sqrt(N1 * var(D_diff))

# Model comparison (loo)
#
loo::loo_compare(PSIS1, PSIS2)
lpd_point <- cbind(PSIS1$pointwise[,"elpd_loo"], PSIS2$pointwise[,"elpd_loo"])
#waic_wts <- exp(waics) / sum(exp(waics))
pbma_wts <- loo::pseudobma_weights(lpd_point, BB=FALSE)
pbma_BB_wts <- loo::pseudobma_weights(lpd_point) # default is BB=TRUE
stacking_wts <- loo::stacking_weights(lpd_point)
round(cbind(pbma_wts, pbma_BB_wts, stacking_wts), 2)

# Cool trick
#
# aggregate(, sum) across factors (X) with respect to y to get a aggregated
# data set. Neat!
d_agg <- aggregate(d$pulled_left, #y: variate, 
                   list(tid=d$treatment, aid=d$actor,
                         sid=d$side, cid=d$cond), #X: covariates
                   sum) 
colnames(d_agg)[5] <- "y"

range(d_agg$y)
# [1]  2 18

(ano <- length(unique(d$actor)))
(sno <- length(unique(d$side)))
(cno <- length(unique(d$cond)))
dat_ls <- with(d_agg, list(N=nrow(d_agg), ano=ano, sno=sno, cno=cno, aid=aid, 
                           sid=sid, cid = cid, y=y))
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_3.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit3 <- mdl$sample(data=dat_ls)

fit3$cmdstan_diagnose()
fit3$cmdstan_summary()
fit3$summary()

# Samples from the posterior 
samples <- fit3$draws(format="data.frame")

# Extract LL for model 2
LL2 <- fit2$draws("log_lik")
LL3 <- fit3$draws("log_lik")
rel_eff2 <- loo::relative_eff(exp(LL2))
rel_eff3 <- loo::relative_eff(exp(LL3))
(PSIS2 <- loo::loo(LL2, r_eff = rel_eff2, is_method="psis"))
(PSIS3 <- loo::loo(LL3, r_eff = rel_eff3, is_method="psis"))

# Model comparison not possible since not all models have the same number of
# Obs! But we know these are the same date (aggregated vs not)
loo::loo_compare(PSIS2, PSIS3)
lpd_point <- cbind(PSIS1$pointwise[,"elpd_loo"], PSIS2$pointwise[,"elpd_loo"])
#
#
(lppd2 <-  PSIS2$pointwise[,"elpd_loo"])
(p_psis2 <-  PSIS2$pointwise[,"p_loo"])
D2 <- -2*(lppd2-p_psis2)
-2*(sum(lppd2)-sum(p_psis2))
# [1] 546.4991
# Approximate PSIS standard error 
N2 <- dim(LL2)[3]
PSIS_i2 <- -2*(lppd2-p_psis2)
sqrt(N2 * var(PSIS_i2))
# [1] 19.73713

(lppd3 <-  PSIS3$pointwise[,"elpd_loo"])
(p_psis3 <-  PSIS3$pointwise[,"p_loo"])
D2 <- -2*(lppd3-p_psis3)
-2*(sum(lppd3)-sum(p_psis3))
# [1] 546.4991
# Approximate PSIS standard error 
N3 <- dim(LL3)[3]
PSIS_i3 <- -2*(lppd3-p_psis3)
sqrt(N3 * var(PSIS_i3))
# [1] 19.73713

# Import: If you want to calculate PSIS or WAIC use the non-aggregated form!
# Inference remains the same in both methods

#
# Aggregated binomial 2
#
library(rethinking)
data(UCBadmit)
d <- UCBadmit

# Model sketch
#
# A_i ~ binomial(N_i, p_i) # Note: N_i is a vector of different Ns
# logit(p_i) = alpha_GID[i]
# alpha_j ~ Normal(0, 1.5)

# Data reduction
#
d$gid <- ifelse(d$applicant.gender=="male", 1, 2) 
gno <- length(unique(d$gid))
dno <- length(unique(d$dept))
dat_ls <- list(N=nrow(d), aid=d$applications, gid=d$gid, A=d$admit, gno=gno)
# Fit the model (Note: less efficient than b)
#
#file <- "~/projects/stanmisc/stan/11/mdl_4a.stan"
#mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
#fit <- mdl$sample(data=dat_ls)


# Diagnostics
#
#fit$cmdstan_diagnose()
#fit$cmdstan_summary()
#fit$summary()

#
# Binomial_logit model
# ...to estimate the TOTAL causal effect of gender on admission
# 
file <- "~/projects/stanmisc/stan/11/mdl_4b.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$cmdstan_summary()
fit$summary()

# Posterior draws
#
post <- fit$draws(format="matrix")

# Contrasts
#
# Relative effect
reldiff <- post[,"alpha_lo[1]"] - post[,"alpha_lo[2]"]
cat(mean(reldiff), "+/-", sd(reldiff), "\n")
# Proportional odds(?)
(q <- mean(exp(reldiff)))
# [1] 1.846992
# 84% increase in odds
# Absolute effect
absdiff <- post[,"alpha_p[1]"] - post[,"alpha_p[2]"]
cat(mean(absdiff), "+/-", sd(absdiff), "\n")
c(mean(absdiff) - sd(absdiff), mean(absdiff) + sd(absdiff))
# 0.1418794 +/- 0.01374009 
# About 12-15% increase in probability of admission...

# PPC
#
x_max <- nrow(d)
x_range <- 1:x_max 
d$A_rate <- d$admit / d$applications 
plot(x_range, d$A_rate, ylim=c(0,1), pch=20)
x1 <- seq(1,x_max,by=2) ; y1_A <- d$A_rate[x1]
x2 <- seq(2,x_max,by=2) ; y2_A <- d$A_rate[x2]
for(i in 1:x_max) {
  lines(c(x1[i], x2[i]), c(y1_A[i], y2_A[i]))
}

# Visualize the models expectations
#
p <- fit$draws("p", format="matrix")
(p_mu <- apply(p, 2, mean))
(p_HPDI <- apply(p, 2, rethinking::PI))
for(i in seq(x_max)) {
  points(i, p_mu[i], pch=20, col="steelblue")
  lines(rep(i,2), c(p_HPDI[1,i], p_HPDI[2,i]))
}
y1_mu <- p_mu[x1] ; y2_mu <- p_mu[x2]
for(i in 1:x_max) {
  lines(c(x1[i], x2[i]), c(y1_mu[i], y2_mu[i]))
}
# For a simlle three-point summary a one-liner is enough
#points(rep(i,3), c(p_HPDI[1,i], p_mu[i], p_HPDI[2,i]), type="b", lwd=2, cex=.3, pch=20) 

#
# Extended model
# ...to estimate the DIRECT causal effect of gender on admission
#

# Data reduction
#
d$gid <- ifelse(d$applicant.gender=="male", 1, 2) 
dno <- length(unique(d$dept))
d$did <- as.numeric(d$dept)
dat_ls <- list(N=nrow(d), aid=d$applications, gid=d$gid, A=d$admit, gno=gno, did=d$did, dno=dno)

file <- "~/projects/stanmisc/stan/11/mdl_5a.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$cmdstan_summary()
fit$summary()

# Posterior draws
#
post <- fit$draws(format="matrix")

# PPC
#
x_range <- 1:x_max 
x_max <- nrow(d)
d$A_rate <- d$admit / d$applications 
plot(x_range, d$A_rate, ylim=c(0,1), pch=20)
x1 <- seq(1,x_max,by=2) ; y1_A <- d$A_rate[x1]
x2 <- seq(2,x_max,by=2) ; y2_A <- d$A_rate[x2]
for(i in 1:x_max) {
  lines(c(x1[i], x2[i]), c(y1_A[i], y2_A[i]))
}
# Visualize the models expectations
#
p <- fit$draws("p", format="matrix")
(p_mu <- apply(p, 2, mean))
(p_HPDI <- apply(p, 2, rethinking::PI))
for(i in seq(x_max)) {
  points(i, p_mu[i], pch=20, col="steelblue")
  lines(rep(i,2), c(p_HPDI[1,i], p_HPDI[2,i]), col="steelblue")
}
y1_mu <- p_mu[x1] ; y2_mu <- p_mu[x2]
for(i in 1:x_max) {
  lines(c(x1[i], x2[i]), c(y1_mu[i], y2_mu[i]))
}

#
# Additional exercise from lecture
#
d$gid <- ifelse(d$applicant.gender=="male", 1, 2) 
(gno <- length(unique(d$gid)))
(dno <- length(unique(d$dept)))
d$did <- as.numeric(d$dept)
dat_ls <- list(N=nrow(d), aid=d$applications, gid=d$gid, A=d$admit, gno=gno,
               did=d$did, dno=dno)

# Reformulate the model
#
file <- "~/projects/stanmisc/stan/11/mdl_5b.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$cmdstan_summary()
fit$summary()

#
# Direct causal effect accross departments
#

# Posterior & additional variables
#
post <- fit$draws(format="data.frame")
Alpha_p <- fit$draws("Alpha_p", format="matrix")
p_diff <- fit$draws("p_diff", format="matrix")
plot(density(p_diff))

# Posterior correlations 
#
round(cor(Alpha_p), digits=2)
# If the posterior correlations are too high, bump up the number of iterations

# Different version – same result
(PrA_G1 <- Alpha_p[,1:6])
(PrA_G2 <- Alpha_p[,7:12])
diff_PrA <- sapply(1:6, function(i) PrA_G1[,i] - PrA_G2[,i])
plot(density(diff_PrA), xlab="Effect of gender perception")

# Poststratification
#
(cont.tab <- xtabs(d$applications ~ as.integer(d$dept)))
# Poststratification weights
(w <- cont.tab / sum(d$applications))

# Visualize
#
plot(c(-0.4,0.4), c(0,25), type="n", xlab="Gender contrast (probability)")
for(i in 1:6) {
  lines(density(diff_PrA[,i]), col=i+1, lwd=2+20*w[i])
}
abline(v=0, lty=2)
text(-0.1, 20, "men adv" ) ; text(0.1, 20, "fem adv" )
legend("topright", legend=paste("Dep", LETTERS[1:6]), lty=1, col=1:6, lwd=2)

# PPD
#
y_tilde <- fit$draws("y_tilde", format="matrix")

# If binomal_logit_rng would be implemented
plot(density(d$amit))
bayesplot::ppc_dens_overlay(d$admit, y_tilde[1:50,])

# Poststratification weights
(w <- cont.tab / sum(d$applications))

#
# Additional lecture stuff
# "Confounded admission"
# 
rbern <- function(N, p=.5) {
  rbinom(N, size=1, p)
}
rbern(10)

# Generative model
# Gender (u): no parents
# unobserved condound, e.g. ability (u): no parent
# Department: D = f(G,u)
# Admission: A = f(G, D, A)

# Simulation
N <- 2e3 # Number of applicants
# G=1: female, G=2: male
G <- rbern(N) + 1 
# u=0: low ability (10%) u=1: high ability
u <- rbern(N,0.1) 
# Chances of applying to a particular appartment
# G=1 (female) have 80% chance to apply to D=1 (psychology) 
# G=1 (female) & high ability (u=1) have 50% chance to apply to D=2 (maths) 
# D=1 (maths) discriminates against G=1 (females); but still apply, becuase
# they thinkin they can overcome the penalty with high ability.
p_apply <- ifelse(G==1, .5*u, .8) 
D <- rbern(N, p_apply) + 1
# Matrix of acceptance rates for average ability [D,G]
(accept_rate_u0 <- matrix(c(0.1, 0.1, 0.1, 0.3), nrow=2))
# Matrix of acceptance rates for high ability [D,G]
(accept_rate_u1 <- matrix(c(0.2, 0.3, 0.2, 0.5), nrow=2))
calc_p_admit <- function(i) {
  ifelse(u[i] == 0, accept_rate_u0[D[i], G[i]], accept_rate_u1[D[i], G[i]])
}
p_admit <- sapply(1:N, calc_p_admit) 
A <- rbern(N,p_admit)

#
# Model sketch
#
# Total causal effect
# A ~ Bernoulli(p_i)
# logit(p_i) = alpha_G[i]
# alpha ~ normal(0,1) 
#
# Direct causal effect
# A ~ Bernoulli(p_i)
# logit(p_i) = alpha_GD[i]
# alpha ~ normal(0,1) 
#

# Data reduction 
#
dat_ls <- list(N=N, A=A, did=D, gid=G, dno=length(unique(D)), gno=(length(unique(G)))  )

# Fit the model (total)
# 
file <- "~/projects/stanmisc/stan/11/mdl_6a.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
#fit$cmdstan_diagnose()
#fit$cmdstan_summary()
fit$summary("alpha_p")

# Total causal effect accross departments
#
post <- fit$draws(format="data.frame")
alpha_p <- fit$draws("alpha_p", format="matrix")
alpha_diff <- alpha_p[,1] - alpha_p[,2]
plot(density(alpha_diff), xlim=c(-.4,0))

# Discrimination
#
fit$summary("alpha_p")
fit$summary("alpha_lo")

# Fit the model
#
file <- "~/projects/stanmisc/stan/11/mdl_6b.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
#fit$cmdstan_diagnose()
#fit$cmdstan_summary()
#fit$summary()
fit$summary("Alpha_p")
fit$summary("Alpha_lo")

# Total causal effect accross departments
# confounded: collider bias
#
post <- fit$draws()
Alpha_p <- fit$draws("Alpha_p")
Alpha_lo <- fit$draws("Alpha_lo")

# Posterior correlations
#
Alpha_p_cor <- fit$draws("Alpha_p", format="matrix")
round(cor(Alpha_p_cor), 2)

# Contrasts on p sclae
D1_FM_contrast_p <- Alpha_p[,,1] - Alpha_p[,,3]
D2_FM_contrast_p <- Alpha_p[,,2] - Alpha_p[,,4]
D1_FM_contrast_lo <- Alpha_lo[,,1] - Alpha_lo[,,3]
D2_FM_contrast_lo <- Alpha_lo[,,2] - Alpha_lo[,,4]

# Visualization (collider bias)
#
# p-scale
plot(density(D1_FM_contrast_p), col="red", xlim = c(-.3,0.3), lwd=2,
xlab="FM_contast")
lines(density(D2_FM_contrast_p), col="blue", lwd=2)
# lo-scale
plot(density(D1_FM_contrast_lo), col="red", xlim = c(-1,1), lwd=2,
xlab="FM_contast")
lines(density(D2_FM_contrast_lo), col="blue", lwd=2)
# High-ability (u=1) G1s (female) apply to D2 (maths) anyway.
# G1s (female) in D2 (maths) have higher ability on average.
# => Compensate for the discrimination.

# Imaginge we could measure the confound...
#
# In the DAG "u" is an unobserved confound (fork). Need to block u by
# conditioning on it. Thus we need to measure "u".
dat_ls <- list(N=N, A=A, did=D, gid=G, dno=length(unique(D)), gno=(length(unique(G))), u=u)

file <- "~/projects/stanmisc/stan/11/mdl_6c.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(data=dat_ls)

post <- fit$draws()
Alpha_lo <- fit$draws("Alpha_lo")
# Contrasts
D1_FM_contrast_lo_2 <- Alpha_lo[,,1] - Alpha_lo[,,3]
D2_FM_contrast_lo_2 <- Alpha_lo[,,2] - Alpha_lo[,,4]

# Visualization 
#
# If we know the confound we could unmask the discrimination effect!
# But actually we haven't observed u!
plot(density(D1_FM_contrast_lo), col="blue", xlim = c(-2,1), lwd=1,
xlab="FM_contast", main="collider bias")
lines(density(D2_FM_contrast_lo), col="red", lwd=1)
lines(density(D1_FM_contrast_lo_2), col="blue", lwd=3)
lines(density(D2_FM_contrast_lo_2), col="red", lwd=3)
abline(v=0, lty=2)

# 3 Solutions for deconfounding
# 1. Experiments
# 2. Sensitivity analysis
# 3. Measure proxy of confound

#
# Sensitivity analysis
# Q: How strong must the confound be to change the conclusion?
#

# Model sketch
# I. Oldje model + condound
# G -> A ; D -> A ; u -> A
# A_i ~ Bernoulli(p_i)
# logit(p_i) = Alpha[G_i, D_i] + beta_G[i]u_i
# Note. We could also stick u inside Alpha; but then we can't let is vary by
# gender. If we want to to so, we need an extra parameter and include gender as
# well as ability.
# II. Sub model for application to D1
# G -> D <-u | (D_i=2): Pr. to apply to D1
# (D_i=2) ~ Bernoulli(q_i)
# q_i = delta[G_i] + gamma_G[i]*u_i
# Note: delta is the probability to apply to D1, instead of D2

dat_ls <- list(N=N, A=A, D=D, G=G)
dat_ls$gno <- length(unique(G))
dat_ls$dno <- length(unique(D))
dat_ls$D2 <- ifelse(D==2,1,0)
# Asm: effect sizes for beta, gamma, because we haven't measured u
# Feed it in the model to get an idea of u conditional on the DAG
#
# The effect of ability on the probability of admission
# Here, both given for men & women (stratified by gender)
dat_ls$beta <- c(1,1) #import! Vary these asms!
# The effect of ability on the probability to apply to D1 
# Here, both given for men & women (stratified by gender)
dat_ls$gamma <- c(1,0) #import! Vary these asms!

# FIt the modek
#
file <- "~/projects/stanmisc/stan/11/mdl_6d.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
#fit$cmdstan_diagnose()
#fit$cmdstan_summary()
fit$summary("u")

# Posterior draws
#
post <- fit$draws(format="data.frame")
ncol(post) ; length(D)
Alpha <- fit$draws("Alpha")


# Contrasts
D1_FM_contrast <- Alpha[,,1] - Alpha[,,3]
D2_FM_contrast <- Alpha[,,2] - Alpha[,,4]

# Visualize – demask the collider bias
#
plot(density(D1_FM_contrast_lo), col="blue", xlim = c(-2,1), lwd=1,
xlab="FM_contast", main="collider bias")
lines(density(D2_FM_contrast_lo), col="red", lwd=1)
lines(density(D1_FM_contrast), col="blue", lwd=3)
lines(density(D2_FM_contrast), col="red", lwd=3)
abline(v=0, lty=2)

plot(density(D2_FM_contrast), col="blue", lwd=1,
xlab="FM_contast", main="collider bias")
lines(density(D2_FM_contrast), col="red", lwd=1)
abline(v=0, lty=2)

# Visualize the posterior ability estimates
#
u <- fit$draws("u", format="matrix")
pos_applied <- which(dat_ls$D2==1)
N_D2 <- length(applied_D2)
u_mu <- colMeans(u)
u_HPDI <- apply(u, 2, rethinking::HPDI)
u_mu_D2 <- u_mu[pos_applied]
u_HPDI_D2 <- u_HPDI[,pos_applied]
gender_D2 <- dat_ls$G[pos_applied]
u_combi <- cbind(u_mu_D2, u_HPDI_D2[1,], u_HPDI_D2[2,], gender_D2)
sort_pos <- order(u_mu_D2)
plot_dat <- u_combi[sort_pos,]
plot(plot_dat[,1], cex=.3, pch=21, ylim=c(-2,3), xlab="Application to D2",
ylab="Posterior Ability estimates (u)")
for (i in 1:N_D2) {
  lines(rep(i,2), plot_dat[i,2:3], col=ifelse(plot_dat[i,4]==1, "red", "blue"))
}

#
# Decondound with a proxy
#

# Imagune we had measured a couple of independent test scores for the
# applicants abilities.
# 
T1 <- rnorm(N, u, 0.1)
T2 <- rnorm(N, u, 0.5)
T3 <- rnorm(N, u, 0.25)

# Model sketch
# I.
# A_i ~ Bernoulli(p_i)
# logit(p_i) = Alpha[G[i]],D[i]] + beta_u * u_i
# Alpha ~ normal(0,1)
# beta_u ~ normal(0,1)
# II.
# u_k ~ normal(0,1)
# T_i,j ~ normal(u_i, tau_j)
# Note: could also model as all having the same variance 'tau'
# T_1 ~ norma(u, tau[1])
# T_2 ~ norma(u, tau[2])
# T_3 ~ norma(u, tau[3])

# Data reduction
#
dat_ls <- list(N=N, A=A, D=D, G=G, T1=T1, T2=T2, T3=T3)
dat_ls$gno <- length(unique(G))
dat_ls$dno <- length(unique(D))
dat_ls$tno <- 3 

# Fit the model
#
file <- file.path("..", "stan", "11", "mdl_6e.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$summary("Alpha")

post <- fit$draws(format="data.frame")
ncol(post) ; length(D)
Alpha <- fit$draws("Alpha")

# Contrasts
#
D1_FM_contrast <- Alpha[,,1] - Alpha[,,3]
D2_FM_contrast <- Alpha[,,2] - Alpha[,,4]

# Visualize
#
plot(density(D1_FM_contrast_lo), col="blue", xlim = c(-2,1), lwd=1,
xlab="FM_contast", main="Deconfound the estimates")
lines(density(D2_FM_contrast_lo), col="red", lwd=1)
lines(density(D1_FM_contrast), col="blue", lwd=3)
lines(density(D2_FM_contrast), col="red", lwd=3)
abline(v=0, lty=2)

# Posterior mean u
#
u_est <- fit$draws("u", format="matrix")
mu_u <- colMeans(u_est)

#  Visualize
#
N <- length(mu_u)
j <- rnorm(N,0,0.04)
plot(u+j, mu_u, pch=20, col=ifelse(dat_ls$G==1, "red", "blue"),
     ylab="Posterior mean u", xlab="u (true)") 
mtext("Model correctly detected u, even though prior was uk ~ Normal(0,1)")

#
#
# Poisson regression 
#
#

library(rethinking)
data(Kline)
d <- Kline
d

# Data transformation 
#
# Standardized log population
P_log <- log(d$population)
P <- (P_log - mean(P_log)) / sd(P_log)
# Contact rate id
d$cid <- ifelse(d$contact=="low", 1, 2)

# Model sketch
#
# T_i ~ Poission(lambda_i)
# log(lambda_i) = alpha[CID[i]] + beta[CID[i]] * log P_i #IA: Popl & Tools
# alpha_j ~  
# beta_j ~  

# PPS
#
# Prior implications
# Import: log-normal!
curve(dlnorm(x, 0, 10), from=-1, to=100)
# How long are the tails?
N <- 1e4
alpha <- rnorm(N, 0, 10)
lambda <- exp(alpha)
mean(lambda)










