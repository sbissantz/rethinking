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
