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
plot(d$M, d$B, pch=20, col="red")
for(i in seq(100)) {
  #curve(samples$alpha[i] + samples$beta_M[i]*(x), add=TRUE)
  abline(samples$alpha[i], samples$beta_M[i], col)
}
plot(d$M, d$B, pch=20, col=scales::alpha("steelblue",0.9))

polygon(rep(M_seq,2), c(mu_HPDI[1,], mu_HPDI[2,]), col="red")
rethinking::shade


# MAP line 
lines(M_seq, mu_mean, lwd=2)
# High Posterior Density Intervals
# Distribution of mu 
rethinking::shade(mu_HPDI, M_seq)
# High Posterior Density Intervals
# Model expects to find 89% of actual hights within...
rethinking::shade(B_HPDI, M_seq)




w_seq <- 20:70 
wbar <- mean(d2$weight)
N_samples <- nrow(samples)
calc_mu <- function(weight) samples$alpha + samples$beta*(weight-wbar)
# Posterior line prediction
mu <- vapply(w_seq, calc_mu, double(N_samples))
# Posterior predictions
sigma <- samples$sigma
sim_height <- function(weight) {
  rnorm(N_samples,
        mean=samples$alpha + samples$beta * (weight - wbar),
        sd=samples$sigma)
}
h_tilde <- vapply(w_seq, sim_height, double(N_samples))

# MAP
mu_mean <- colMeans(mu)
# mu's HPDI
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
# h_tilde's HPDI
h_HPDI <- apply(h_tilde, 2, rethinking::HPDI)

# Visualize (Spaghetti plot)
#
N_samples <- nrow(samples)
plot(d2$weight, d2$height, col="lightgrey")
for(i in seq(N_samples)) {
  curve(samples$alpha[i] + samples$beta[i]*(x - wbar), add=TRUE)
}
plot(d2$weight, d2$height, col=col.alpha(rangi2,"0.5"))
# MAP line 
lines(w_seq, mu_mean)
# High Posterior Density Intervals
# Distribution of mu 
rethinking::shade(mu_HPDI, w_seq)
# High Posterior Density Intervals
# Model expects to find 89% of actual hights within...
rethinking::shade(h_HPDI, w_seq)






# Alternatively with rstan
stanfit <- rstan::read_stan_csv(fit$output_files())
rstan::traceplot(stanfit)
























