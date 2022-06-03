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
file <-  file.path("..","..", "stan", "8", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) ; mdl$print()
fit <- mdl$sample(data=stan_ls)
stanfit <- read_stan_csv(fit$output_file())

rethinking::trankplot(stanfit)


# Concentration of measure
# Evaluate the radial distance from the mode
# 
# Use 10, 20, 100, 1000
D <- 1e3
T <- 1e3
Y <- MASS::mvrnorm(1e3, rep(0,D), diag(D))
rad_dist <- function(Y) sqrt(sum(Y^2))
Rd <- sapply(1:T, function(i) rad_dist(Y[i,]))
dens(Rd, xlim=c(0,50))

