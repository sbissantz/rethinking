#
# Simulation based calibration
#
# Generative model
alpha <- 0.0                            
beta_M <- 0.5
beta_A <- 0.5
sigma <- .5 
# Simulated data 
N <- 1e2
M <- rnorm(N) 
A <- rnorm(N)
mu <- alpha + beta_M*M + beta_A*A
D <- rnorm(N, mu, sigma)

# Model validation
#
val_dat_ls <- list( N=N, K=2, X=cbind(M,A), D=D )
fml <- file.path(getwd(), "stan", "m53.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(data = val_dat_ls)




# Simulation based calibration
#
alpha <- 70
beta <- 0.5
sigma <- 1
n_individuals <- 50
# predictor
H <- runif(n_individuals, 130, 170)
mu <- alpha + beta * (H - mean(H))
# outcome
W <- rnorm(n_individuals, mu, sigma)
#
calib_ls <- list(H=H, Hbar=mean(H), W=W, N=n_individuals)

fml <- file.path(getwd(), "stan", "m32_calib.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(
  data = calib_ls,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)
# Samples
#
fit$print()


