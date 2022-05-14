#
# categorical interatctions
#
options(mc.cores = parallel::detectCores())

# Data
#
library(rethinking)
data(rugged)
d <- rugged

# Tranformations 
#
d$log_gdp <- log(d$rgdppc_2000)
cond <- complete.cases(d$rgdppc_2000) 
dcc <- d[cond,]

# Rescaling
# 
# Mean scaler 
dcc$log_gdp_std <- with(dcc, log_gdp/mean(log_gdp))
# Maximum scaler 
dcc$rugged_norm <- with(dcc, rugged/max(rugged)) 

# PPS
#

# Value ranges
# y: log(GDP)_std in [1+/-1] 
# X: rugged_norm in [0, 1]
set.seed(123)
N <- 1e2
a <- rnorm(N,1,0.1)
b <- rnorm(N,0,0.25)
plot(NULL, xlim=c(0,1), ylim=c(0,2), xlab="rugged_norm", ylab="log_gdp_std")
for(i in seq(N)) abline(a=a[i], b=b[i], col=scales::alpha("steelblue"), .8)
abline(h = range(dcc$log_gdp_std), lty=2)
text(x=0.1, y=range(dcc$log_gdp_std) + c(-0.1, +0.1), "Data range")

sum(b > 0.6)/N

# Evaluate the consequences of sigma 
r_seq <- seq(0,1, length.out=N)
mu <- a+b*r_seq ; sigma <- rexp(N,2)
y_tilde <- rnorm(N, mu, sigma)
plot(y_tilde ~ r_seq, pch=20, col=scales::alpha("steelblue", .8),
xlab="rugged_norm", ylab="log_gdp_std")
abline(h=c(0, 2), lty=2)
text(x=0.1, y=c(0,2) + c(-0.2, +0.2), "2Sd")
# Problem: Predicts outside the possible range of values!

# Model
#
# log_gdp_std ~ normal(mu, sigma)
# mu = alpha + beta_r * (rugged_norm - mean(rugged_norm))
# alpha ~ normal(1,0.1)
# beta ~ normal(0,0.25)
# sigma ~ exponential(2)

# Reduction
#
stan_ls <- list(N=nrow(dcc), rugged_norm=dcc$rugged_norm,
               log_gdp_std=dcc$log_gdp_std)

# Fit 
#
file <-  file.path("..", "stan", "8", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) ; mdl$print()
fit <- mdl$sample(data=stan_ls)

# Diagnostics & samples
#
fit$cmdstan_diagnose()
fit$print()
#  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#    lp__   247.45 247.77 1.22 0.98 245.06 248.78 1.00     1856     2384
#    alpha    1.21   1.21 0.02 0.02   1.18   1.23 1.00     1933     2020
#    beta_r   0.02   0.02 0.06 0.06  -0.08   0.10 1.00     1881     2119
#    sigma    0.14   0.14 0.01 0.01   0.13   0.15 1.00     2391     2228
samples <- fit$draws(format="data.frame")
LL_1 <- fit$draws("logprob")
#bayesplot::mcmc_trace(samples)

# Indicator variable
#
# Note: Recode the dummy to be an indicator
dcc$cid <- with(dcc, ifelse(cont_africa==1, 1, 2))

# Model
#
# log_gdp_std ~ normal(mu, sigma)
# mu = alpha_cid[i] + beta_r * (rugged_norm - mean(rugged_norm))
# alpha_cid[i] ~ normal(1,0.1)
# beta ~ normal(0,0.25)
# sigma ~ exponential(2)

# Reduction
#
stan_ls <- list(N=nrow(dcc), L=length(unique(dcc$cid)), cid=dcc$cid,
                rugged_norm=dcc$rugged_norm, log_gdp_std=dcc$log_gdp_std)



# Fit
#
file <-  file.path("..", "stan", "8", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) ; mdl$print()
fit <- mdl$sample(data=stan_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()


# Samples
#
samples <- fit$draws(format="matrix")
LL_2 <- fit$draws("logprob")
# bayesplot::mcmc_trace(samples)

# Posterior correlations
#
vars <- c("alpha[1]", "alpha[2]", "beta_r")
round( cor(samples[,vars]), digits=2 )

# P-mean & HPDI's
# 

# Value sequence
rugged_seq <- seq(-0.1, 1.1, length.out=nrow(samples))

# Function for the posterior line
calc_mu <- function(rugged, cid) {
    a <- paste0("alpha[", cid, "]") 
    samples[, a] + samples[,"beta_r"] * rugged
}
# Mu within africa
mu_africa <- sapply(rugged_seq, calc_mu, cid=1)
# Mu outside of africa
mu_noafrica <- sapply(rugged_seq, calc_mu, cid=2)

# Poterior line within africa 
mu_africa_mean <- colMeans(mu_africa)
# Poterior line outside of africa 
mu_noafrica_mean <- colMeans(mu_noafrica)

# HPDI within affrica
mu_africa_HPDI <- apply(mu_africa, 2, rethinking::HPDI)
# HPDI outside of affrica
mu_noafrica_HPDI <- apply(mu_noafrica, 2, rethinking::HPDI)

# Visualize
#

# Reversed ruggedness sequence
rugged_seq_rev <- rugged_seq[seq(length(rugged_seq),1)]
plot(dcc$rugged_norm, dcc$log_gdp_std, xlab="ruggedness (normalized)",
ylab="log GDP (standardized)", pch=20, col=ifelse(dcc$cid==2, "steelblue", "black"))
# ...for the polygon
y <- c(mu_africa_HPDI[1, ], mu_africa_HPDI[2, ][seq(ncol(mu_africa_HPDI),1)])
x <- c(rugged_seq, rugged_seq_rev)
polygon(x, y, col="lightgrey", border="lightgrey") 
# Posterior mean line within africa
lines(rugged_seq, mu_africa_mean, lwd=3, col="black")
# ...for the polygon
y <- c(mu_noafrica_HPDI[1, ], mu_noafrica_HPDI[2, ][seq(ncol(mu_noafrica_HPDI),1)])
x <- c(rugged_seq, rugged_seq_rev)
polygon(x, y, col="lightblue", border = "lightblue") 
# Posterior mean line outside of africa
lines(rugged_seq, mu_noafrica_mean, lwd=3, col="steelblue")
# Legend to clarify the colors
legend("topright", legend=c("Not Africa", "Africa"), fill=c("steelblue", "black"))

# Visualize II
#
N <- 100
plot(dcc$rugged_norm, dcc$log_gdp_std, xlab="ruggedness (normalized)",
ylab="log GDP (standardized)", pch=20, col=ifelse(dcc$cid==2, "steelblue", "black"))
# Posterior mean line within africa
for(i in seq(N)) lines(rugged_seq, mu_africa[i,], col=scales::alpha("black", .4))
lines(rugged_seq, mu_africa_mean, lwd=6)
for(i in seq(N)) lines(rugged_seq, mu_noafrica[i,], col=scales::alpha("steelblue", .4))
lines(rugged_seq, mu_noafrica_mean, lwd=6, col="steelblue")
legend("topright", legend=c("Not Africa", "Africa"), fill=c("steelblue", "black"))

# Model
#
# log_gdp_std ~ normal(mu, sigma)
# mu = alpha_cid[i] + beta_r_cid[i] * (rugged_norm - mean(rugged_norm))
# alpha_cid[i] ~ normal(1,0.1)
# beta_r_cid[i] ~ normal(0,0.25)
# sigma ~ exponential(2)

# Reduction
#
# stan_ls is the same as before!

# Fit
#
file <-  file.path("..", "stan", "8", "3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) ; mdl$print()
fit <- mdl$sample(data=stan_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()


# Samples
#
samples <- fit$draws(format="matrix")
LL_3 <- fit$draws("logprob")
# bayesplot::mcmc_trace(samples)

# Posterior correlations
#
vars <- c("alpha[1]", "alpha[2]", "beta_r[1]", "beta_r[2]")
round( cor(samples[,vars]), digits=2 )

# P-mean & HPDI's
# 

# Value sequence
rugged_seq <- seq(-0.1, 1.1, length.out=nrow(samples))

# Function for the posterior line
calc_mu <- function(rugged, cid) {
    a <- paste0("alpha[", cid, "]") 
    b <- paste0("beta_r[", cid, "]") 
    samples[, a] + samples[, b] * rugged
}
# Mu within africa
mu_africa <- sapply(rugged_seq, calc_mu, cid=1)
# Mu outside of africa
mu_noafrica <- sapply(rugged_seq, calc_mu, cid=2)

# Poterior line within africa 
mu_africa_mean <- colMeans(mu_africa)
# Poterior line outside of africa 
mu_noafrica_mean <- colMeans(mu_noafrica)

# HPDI within affrica
mu_africa_HPDI <- apply(mu_africa, 2, rethinking::HPDI)
# HPDI outside of affrica
mu_noafrica_HPDI <- apply(mu_noafrica, 2, rethinking::HPDI)

mu_noafrica_HPDI <- apply(mu_noafrica, 2, rethinking::HPDI)

# Visualize
#

# Reversed ruggedness sequence
rugged_seq_rev <- rugged_seq[seq(length(rugged_seq),1)]
plot(dcc$rugged_norm, dcc$log_gdp_std, xlab="ruggedness (normalized)",
ylab="log GDP (standardized)", pch=20, col=ifelse(dcc$cid==2, "steelblue", "black"))
# ...for the polygon
y <- c(mu_africa_HPDI[1, ], mu_africa_HPDI[2, ][seq(ncol(mu_africa_HPDI),1)])
x <- c(rugged_seq, rugged_seq_rev)
polygon(x, y, col="lightgrey", border="lightgrey") 
# Posterior mean line within africa
lines(rugged_seq, mu_africa_mean, lwd=3, col="black")
# ...for the polygon
y <- c(mu_noafrica_HPDI[1, ], mu_noafrica_HPDI[2, ][seq(ncol(mu_noafrica_HPDI),1)])
x <- c(rugged_seq, rugged_seq_rev)
polygon(x, y, col="lightblue", border = "lightblue") 
# Posterior mean line outside of africa
lines(rugged_seq, mu_noafrica_mean, lwd=3, col="steelblue")
# Legend to clarify the colors
legend("topright", legend=c("Not Africa", "Africa"), fill=c("steelblue", "black"))

# Visualize II
#
N <- 100
plot(dcc$rugged_norm, dcc$log_gdp_std, xlab="ruggedness (normalized)",
ylab="log GDP (standardized)", pch=20, col=ifelse(dcc$cid==2, "steelblue", "black"))
# Posterior mean line within africa
for(i in seq(N)) lines(rugged_seq, mu_africa[i,], col=scales::alpha("black", .4))
lines(rugged_seq, mu_africa_mean, lwd=6)
for(i in seq(N)) lines(rugged_seq, mu_noafrica[i,], col=scales::alpha("steelblue", .4))
lines(rugged_seq, mu_noafrica_mean, lwd=6, col="steelblue")
legend("topright", legend=c("Not Africa", "Africa"), fill=c("steelblue", "black"))

# Model comparison 
#
rel_eff <- loo::relative_eff(exp(LL_1))
PSIS_1 <- loo::loo(LL_1, r_eff = rel_eff, is_method="psis")

rel_eff <- loo::relative_eff(exp(LL_2))
PSIS_2 <- loo::loo(LL_2, r_eff = rel_eff, is_method="psis")

rel_eff <- loo::relative_eff(exp(LL_3))
PSIS_3 <- loo::loo(LL_3, r_eff = rel_eff, is_method="psis")

comp <- loo::loo_compare(PSIS_1, PSIS_2, PSIS_3)
print(comp, simplify=FALSE)

# Influential obs 
# ..conditional on the model
#
pareto_k <- PSIS_3$pointwise[,"influence_pareto_k"]
plot(pareto_k, pch=20) ; abline(h=0.5, lty=2)
# Note: influential obs, should adapt model expectations!

# Expected Difference in log-GDP
#
delta <- mu_africa - mu_noafrica 
delta_mean <- colMeans(delta)
delta_HPDI <- apply(delta,2, rethinking::HPDI)

# Visualize
#
rugged_seq_rev <- rugged_seq[seq(length(rugged_seq),1)]
plot(NULL, xlim=range(rugged_seq), ylim=range(delta_HPDI), 
xlab="Ruggedness (norm)", ylab="Expected difference in log GDP (std)")
y <- c(delta_HPDI[1, ], delta_HPDI[2, ][seq(ncol(delta_HPDI),1)])
x <- c(rugged_seq, rugged_seq_rev)
polygon(x, y, col="lightblue", border = "lightblue") 
lines(rugged_seq, delta_mean, lwd=2, col="steelblue")
text(x=0.1, y=c(0.02, -0.02), labels = c("Africa higher GDP", "Africa lower GDP"))
abline(h=0, lty=2)
mtext("89% HPDI")

#
# Continous interatctions
#
data(tulips)
d <- tulips

# Transform the data
#
# anyNA(d)
# [1] FALSE
# 
# Center water
d$W <- with(d, water - mean(water))
# Center shade 
d$S <- with(d, shade - mean(shade))
# Normalize blooms. 0: meaningful
d$B <- with(d, blooms/max(blooms))

# Model sketch
#
# B ~ normal(mu, sigma)
# mu_i = alpha + gamma_W,i * W_i + beta_S * S_i
# gamma_W,i = beta_W + beta_WS * S_i
# alpha ~ alpha(0.5, 0.25)
# beta_W ~ normal(0, 0.25)
# beta_S ~ normal(0, 0.25)
# beta_WS ~ normal(0, 0.25)

# PPS
#
N <- 1e3
W <- seq(-1, 1, length.out=N)
simulate_prior <- function(S) {
    alpha <- rnorm(N, 0.5, 0.25)
    beta_S <- rnorm(N, 0, 0.25) 
    beta_W <- rnorm(N, 0,0.25)
    beta_WS <- rnorm(N, 0, 0.25)
    gamma_Wi <- beta_W * W + beta_WS * S
    calc_mu <- function(W, S) {
        alpha + gamma_Wi * W + beta_S * S
    }
    (mu <- sapply(W, calc_mu, S=S)) 
}
S <- -1:1
mu_ls <- lapply(S, simulate_prior)

# Prior visualization 
# (tryptic) 
#
par(mfrow=c(3,1))
plot_prior <- function(S){
    plot(NULL, xlim=c(-1,1), ylim=c(-1.5,1.5), 
         pch=20, type="n", xlab="Water", ylab="Bloom")
    abline(h=c(0,1), lty=2)
    for(i in 1:100) {
        lines(W, mu_ls[[S+2]][i,], col=scales::alpha("steelblue", .3)) 
    }
    mtext(paste0("shade = ", S))
}
lapply(S, plot_prior)

# Note: omit to visualize the influence of sigma
#

# TODO: Posterior predictions
#

# Reduction
#
dat_ls <- list(N = nrow(d), B=d$B, W=d$W, S=d$S)

# Fit
#
file <- "../stan/8/5.stan"
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format="data.frame")










