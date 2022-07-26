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
samples <- fit$draws(format="data.frame")

# Extract LL for model 2
LL2 <- fit2$draws("log_lik")
LL3 <- fit3$draws("log_lik")
rel_eff2 <- loo::relative_eff(exp(LL2))
rel_eff3 <- loo::relative_eff(exp(LL3))
PSIS2 <- loo::loo(LL2, r_eff = rel_eff2, is_method="psis")
PSIS3 <- loo::loo(LL3, r_eff = rel_eff3, is_method="psis")

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


# TODO: check again what is wrong
#



