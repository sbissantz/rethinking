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
bayesplot::mcmc_trace(samples)

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
stan_ls <- list(N=nrow(dcc), rugged_norm=dcc$rugged_norm,
               log_gdp_std=dcc$log_gdp_std, L=unique(dcc$cid), cid=dcc$cid)

# Fit
#
file <-  file.path("..", "stan", "8", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) ; mdl$print()
fit <- mdl$sample(data=stan_ls)


