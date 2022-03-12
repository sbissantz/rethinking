#
# Overfitting 
#

# Type in data
#
sppnames <- c("afarensis", "africanus", "habilits", "boisei", "rudolfensis",
              "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species=sppnames, brain=brainvolcc, mass=masskg)

# Visualize
#
plot(d$mass, d$brain, ylab="brain volume (cc)", xlab="body mass (kg)", pch=20)
text(x=masskg, y=brainvolcc+50, sppnames)

# Standardize variables
#
d$M <- with(d, (mass - mean(mass))/sd(mass))
d$B <- with(d, (mass - mean(brain))/mean(brain))

# Model sketch
#
# B_i ~ Normal(mu_i, sigma)
# mu_i = alpha + beta_M * M_i
# alpha ~ normal(0,0.1)
# beta_M ~ normal(0,5)
# sigma ~ exponential(1)

# PPD
#
N <- 5e2
alpha <- rnorm(N,0,0.1)
M <- seq(-2,2, length.out=N)
beta_M <- rnorm(N, 0,0.5)
mu <- alpha + beta_M*M
sigma <- rexp(N, 1)
B_tilde <- rnorm(N, mu, sigma)

plot(B_tilde)

?scale

# Reduction
#
dat_ls <- list(B=d$B, M=d$M)

 
file <- file.path(getwd(), "stan", "7", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)



x <- rnorm(1e6,0,1)
summary(x)
summary((x - min(x)) / (max(x)-min(x)))
summary((x / abs(max(x))))



x <- runif(1e6, max=10)
summary(x)
summary(x/max(x))
summary(x-min(x))/(max(x)-min(x))

x <- rnorm(1e6)
summary((x - min(x)) / (max(x)-min(x)))
summary(x / max(x))

















