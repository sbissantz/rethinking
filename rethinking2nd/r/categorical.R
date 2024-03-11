#
# categorical variables
#

# DAG
#
dag <- dagitty::dagitty('dag{
H [pos="0,0"]
W [pos="0.5,0"]
S [pos="0,1"]
S -> H -> W
S -> W
}')

plot(dag)

# https://stackoverflow.com/questions/29183577/how-to-represent-a-categorical-predictor-rstan

# Simulate data 
# S (sex) 
# H (height) = f(S)
# W (weight) = f(S,W)
N <- 50
S <- sample(c(1,2), N, replace = TRUE)
H <- rnorm(N, S) 
W <- rnorm(N, S+W)

# Data wrangling
#
dat_ls = list(N=N, J=2, S=as.integer(S), H=H, W=W)

# Model
#

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_exk_1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE) 
fit <- mdl$sample(dat_ls)
fit$print()
fit$cmdstan_diagnose()
