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
# Evaluate the consequences of sigma 
r_seq <- seq(0,1, length.out=N)
mu <- a+b*r_seq ; sigma <- rexp(N,2)
y_tilde <- rnorm(N, mu, sigma)
plot(y_tilde ~ r_seq, pch=20, col=scales::alpha("steelblue", .8),
xlab="rugged_norm", ylab="log_gdp_std")
abline(h=c(0, 2))
# Problem: Predicts outside the possible range of values!

# Mean at zero
abc <- (dcc$rugged_norm - mean(dcc$rugged_norm))


# Model
#
# log_gdp_std ~ normal(mu, sigma)
# mu = alpha + beta_r * rugged_norm
# alpha ~ normal(1,0.1)
# beta ~ normal(0,0.25)
# sigma ~ exponential(2)








