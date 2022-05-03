#
# Posterior predictions
#

source("prep.R")

# Diagnostics
#
fit$cmdstan_diagnose()
fit$cmdstan_summary()
fit$summary()

# Samples
#
samples <- fit$draws(format="data.frame")
# bayesplot::mcmc_trace(samples)

# Posterior predictions
N_samples <- nrow(samples)
M_seq <- seq(-3,3,length.out=100)

# Posterior mean
calc_mu <- function(M) samples$alpha + samples$beta_M * M
mu <- vapply(M_seq, calc_mu, double(N_samples))

# Posterior predictiv distribution
calc_B <- function(M) {
  rnorm(N_samples,
        mean=samples$alpha + samples$beta_M * M,
        sd=samples$sigma)
}
B_tilde <- vapply(M_seq, calc_B, double(N_samples))


# MAP
mu_mean <- colMeans(mu)
# mu's HPDI
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
# h_tilde's HPDI
B_HPDI <- apply(B_tilde, 2, rethinking::HPDI)

# Visualize (Spaghetti plot)
#
plot(d$M, d$B, pch=20, col="steelblue")
#for(i in seq(100)) {
  ##curve(samples$alpha[i] + samples$beta_M[i]*(x), add=TRUE)
  #abline(a=samples$alpha[i], b=samples$beta_M[i])
#}
lines(M_seq, mu_mean, lwd=2)
for(i in seq(100)) lines(rep(M_seq[i],2), c(mu_HPDI[1,i], mu_HPDI[2,i]),
col=scales::alpha("black", .8))
for(i in seq(100)) lines(rep(M_seq[i],2), c(B_HPDI[1,i], B_HPDI[2,i]),
col=scales::alpha("steelblue", .8))

plot(d$M, d$B, pch=20, col=scales::alpha("steelblue",0.9))
# MAP line 
lines(M_seq, mu_mean, lwd=2)
# High Posterior Density Intervals
# Distribution of mu 
rethinking::shade(mu_HPDI, M_seq)
# High Posterior Density Intervals
# Model expects to find 89% of actual hights within...
rethinking::shade(B_HPDI, M_seq)

stanfit <- rstan::read_stan_csv(fit$output_files())
rstan::traceplot(stanfit)

