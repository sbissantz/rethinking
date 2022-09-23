#
# Aggregated binomial 2
#
# Note: see also the wofls on:
# 1. Logistic regression 
# 2. Sensitivity analysis
# 2. Defconfound with a proxy 

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

