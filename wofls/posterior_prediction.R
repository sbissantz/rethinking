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
vars <- c("alpha", "beta_M", "sigma")
round(cor(samples[vars]), digits=2)

# In a nutshell
#

N_samples <- nrow(samples)
M_seq <- seq(-3,3,length.out=100)

calc_mu <- function(M) samples$alpha + samples$beta_M * M
mu <- vapply(M_seq, calc_mu, double(N_samples))

calc_B <- function(M) {
  rnorm(N_samples,
        mean=samples$alpha + samples$beta_M * M,
        sd=samples$sigma)
}
B_tilde <- vapply(M_seq, calc_B, double(N_samples))

vars <- c("alpha", "beta_M", "sigma")
round(cor(samples[vars]), digits=2)

mu_mean <- colMeans(mu)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
B_HPDI <- apply(B_tilde, 2, rethinking::HPDI)

plot(c(-2,2), c(-1,1.5))
M_seq_rev <- M_seq[seq(length(M_seq),1)]
y <- c(B_HPDI[1, ], B_HPDI[2, ][seq(ncol(B_HPDI),1)])
x <- c(M_seq, M_seq_rev)
polygon(x, y, col="lightgrey") 
y <- c(mu_HPDI[1, ], mu_HPDI[2, ][seq(ncol(mu_HPDI),1)])
x <- c(M_seq, M_seq_rev)
polygon(x, y, col="grey") 
points(d$M, d$B, pch=20, col=scales::alpha("steelblue",0.9))
legend("bottomright", legend=c("mu HPDI", "B_tilde HPDI"), 
       fill=c("lightgrey", "grey"))

# Alternatives! See at the end the spaghetti plotting style the HPDI straight
# line plotting style!

#
# Documented version
#

# Posterior predictions
N_samples <- nrow(samples)
M_seq <- seq(-3,3,length.out=100)

# Calculate the posterior mean
calc_mu <- function(M) samples$alpha + samples$beta_M * M
mu <- vapply(M_seq, calc_mu, double(N_samples))

# Calculate the Posterior predictiv distribution
calc_B <- function(M) {
  rnorm(N_samples,
        mean=samples$alpha + samples$beta_M * M,
        sd=samples$sigma)
}
# Posterior predictive distribution
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

# Visualize (Polygon)
#

plot(d$M, d$B, pch=20, col=scales::alpha("steelblue",0.9))
# MAP line 
lines(M_seq, mu_mean, lwd=2)
# High Posterior Density Intervals
# Distribution of mu 
rethinking::shade(mu_HPDI, M_seq)
# High Posterior Density Intervals
# Model expects to find 89% of actual hights within...
rethinking::shade(B_HPDI, M_seq)
shade(B_HPDI, M_seq)

# Visualize (Polygon II -- Hand made version)
# Note: This is an adapted version of rethinking::shade().
#

# Empty plot within +/-2 Sd
plot(c(-2,2), c(-1,1.5))
# y coordinates
y <- c(B_HPDI[1, ], B_HPDI[2, ][seq(ncol(B_HPDI),1)])
# x coordinates
x <- c(M_seq, M_seq[seq(length(M_seq),1)])
# B_tildes HPDI's
polygon(x, y, col="lightgrey") 
# y coordinates
y <- c(mu_HPDI[1, ], mu_HPDI[2, ][seq(ncol(mu_HPDI),1)])
# x coordinates
x <- c(M_seq, M_seq[seq(length(M_seq),1)])
# mu's HPDI
polygon(x, y, col="grey") 
# Data points
points(d$M, d$B, pch=20, col=scales::alpha("steelblue",0.9))
# Describe the HPDI's
legend("bottomright", legend=c("mu HPDI", "B_tilde HPDI"), 
       fill=c("lightgrey", "grey"))

# Visualize (vertical lines)
#

plot(d$M, d$B, pch=20, col="steelblue")
lines(M_seq, mu_mean, lwd=2)
for(i in seq(100)) lines(rep(M_seq[i],2), c(mu_HPDI[1,i], mu_HPDI[2,i]),
col=scales::alpha("black", .8))
for(i in seq(100)) lines(rep(M_seq[i],2), c(B_HPDI[1,i], B_HPDI[2,i]),
col=scales::alpha("steelblue", .8))
