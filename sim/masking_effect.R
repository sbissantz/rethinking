#
# Sim Masking effects
#

# Dag 1
#
dag1 <- dagitty::dagitty(
'dag{ 
M [pos="0,0"]  
K [pos="0.5,1"]  
N [pos="1,0"] 
M -> K <- N
M -> N
}')
plot(dag1)

# Functional relationships
# M 
# N = f_N(M)
# K = f_K(M,N)
n <- 1e3
M <- rnorm(n) 
N <- rnorm(n, -M) 
K <- rnorm(n, M+N) 

# Data set
#
d_sim <- list(n=n,M=M,N=N,K=K)

# Model sketch 
#
# K_i ~ normal(mu_i, sigma)
# mu_i =  alpha + beta_N*N  
# alpha ~ normal(0,0.2)
# beta_N ~ normal(0,0.5)
# sigma ~ exponential(1)
file <- file.path(getwd(), "stan", "mdl_55.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit1 <- mdl$sample(data = d_sim)

# Model sketch 
#
# K_i ~ normal(mu_i, sigma)
# mu_i =  alpha + beta_M*M  
# alpha ~ normal(0,0.2)
# beta_M ~ normal(0,0.5)
# sigma ~ exponential(1)
file <- file.path(getwd(), "stan", "mdl_56.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit2 <- mdl$sample(data = d_sim)

# Model sketch 
#
# K_i ~ normal(mu_i, sigma)
# mu_i =  alpha + beta_N*N + beta_M*M
# alpha ~ normal(0,0.2)
# beta_N ~ normal(0,0.5)
# beta_M ~ normal(0,0.5)
# sigma ~ exponential(1)
file <- file.path(getwd(), "stan", "mdl_57.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit3 <- mdl$sample(data = d_sim)

# Diagnostics
#
fit3$cmdstan_diagnose()

# Output(s)
#
fit1
fit2
fit3

# Posterior correlation
#
samples <- fit3$draws(format="data.frame")
with(samples, cor(beta_N, beta_M))

# -------------------------------------------------------------------------

# Dag 2
#
dag <- dagitty::dagitty(
'dag{ 
K [pos="0.5,1"]  
M [pos="0,0"]  
U [pos="0.5,0"]  
N [pos="1,0"] 
M -> K <- N
M <- U -> N
}')
plot(dag)

# Functional relationships
# U 
# M = f_M(U)
# N = f_N(U)
# K = f_K(M,N)

# Simulate the DAG
#
n <- 1e3
U <- rnorm(n)
M <- rnorm(n, U)
N <- rnorm(n, U)
K <- rnorm(n, M+N)

# Data list
#
d_sim <- list(n=n, U=U, M=M, N=N, K=K)

# Model sketch 
#
# K_i ~ normal(mu_i, sigma)
# mu_i =  alpha + beta_N*N  
# alpha ~ normal(0,0.2)
# beta_N ~ normal(0,0.5)
# sigma ~ exponential(1)
file <- file.path(getwd(), "stan", "mdl_55.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit1 <- mdl$sample(data = d_sim)

# Model sketch 
#
# K_i ~ normal(mu_i, sigma)
# mu_i =  alpha + beta_M*M  
# alpha ~ normal(0,0.2)
# beta_M ~ normal(0,0.5)
# sigma ~ exponential(1)
file <- file.path(getwd(), "stan", "mdl_56.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit2 <- mdl$sample(data = d_sim)

# Model sketch 
#
# K_i ~ normal(mu_i, sigma)
# mu_i =  alpha + beta_N*N + beta_M*M
# alpha ~ normal(0,0.2)
# beta_N ~ normal(0,0.5)
# beta_M ~ normal(0,0.5)
# sigma ~ exponential(1)
file <- file.path(getwd(), "stan", "mdl_57.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit3 <- mdl$sample(data = d_sim)

# Outputs
#
fit1
fit2
fit3

# Samples
#
samples <- fit3$draws(format="data.frame")
with(samples, cor(beta_N, beta_M))

#
# Markov equivalence
#
MElist <- dagitty::equivalentDAGs(dag1)
lapply(MElist, plot)