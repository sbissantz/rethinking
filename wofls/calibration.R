#
# Simulation based calibration
#

# Standardized

# In a nutshell
#

# Generative model
alpha <- 0.0                            
beta_M <- 0.5
beta_A <- 0.5
sigma <- .5 

# Simulated data 
N <- 4e2
M <- rnorm(N) 
A <- rnorm(N)
mu <- alpha + beta_M*M + beta_A*A
D <- rnorm(N, mu, sigma)

dat_ls <- list(N=N, K=2, X=cbind(M,A), D=D)
path <- "~/projects/stanmisc"
fml <- file.path(path, "stan", "5", "3.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

fit$cmdstan_diagnose()
fit$print()

# Documented version
#

# Joint generative model
alpha <- 0.0                            
beta_M <- 0.5
beta_A <- 0.5
sigma <- .5 

# Simulate data 
# D = f(M, N)
# ...from the joint generative model 

# Number of data items
N <- 1e2

# Predictior X_i 
M <- rnorm(N) 

# Predictior X_j
A <- rnorm(N)

# Linear predictor X*beta
mu <- alpha + beta_M*M + beta_A*A

# Outcome variable
D <- rnorm(N, mu, sigma)

# Data list
dat_ls <- list(N=N, K=2, X=cbind(M,A), D=D)

# Fit the validation model
#
# Note: The validation model should not exactly pin down the data from the
# generative process. But the model should cover theses values with high
# posterior probability!

path <- "~/projects/stanmisc"
fml <- file.path(path, "stan", "5", "3.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics 
#
fit$cmdstan_diagnose()
fit$print()


# Unstandardized


# In a nutshell
#
alpha <- 70
beta <- 0.5
sigma <- 1
n_individuals <- 50
# Predictor
H <- runif(n_individuals, 130, 170)
# Linear predictor X*beta
mu <- alpha + beta * (H - mean(H))
# Outcome
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


alpha <- 70
beta <- 0.5
sigma <- 1
n_individuals <- 50
# predictor
H <- runif(n_individuals, 130, 170)
# Linear predictor X*beta
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




