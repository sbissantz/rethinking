#
# Measurement error
#

# Load required buzz
library(rethinking)
library(dagitty)
data(WaffleDivorce)
d <- WaffleDivorce

# Show measurement error by state
plot(d$MedianAgeMarriage, d$Divorce, xlim = c(22, 32), ylim = c(5,15),
xlab = "Median Age at Marriage", ylab = "Divorce Rate")
# Standard errors
N <- nrow(d)
for( i in 1:N) {
    y_cords <- d$Divorce[i] + c(-1,1) * d$Divorce.SE[i]
    x_cord <- d$MedianAgeMarriage[i]
    lines( c(x_cord, x_cord), y_cords)
}
# Note: Different states have different standard errors!

# Show measurement error by state
d$log_Population <- log(d$Population)
plot(d$log_Population, d$Divorce, xlab = "Population (log)",
ylab = "Divorce Rate")
for( i in 1:N) {
    y_cords <- d$Divorce[i] + c(-1,1) * d$Divorce.SE[i]
    x_cord <- d$log_Population[i]
    lines( c(x_cord, x_cord), y_cords)
}
# Note: smaller population, fewer observation, bigger the standard error

# Masurement in the outcome variable
#

# Draw a DAG
dag <- dagitty("dag{
    A -> D 
    A -> M
    D -> D_obs
    M -> D
    e_D -> D_obs
}")
plot(dag)

# Create a data list
dat_list <- list(
    "D_obs" = standardize(d$Divorce),
    "D_sd"  = d$Divorce.SE / sd(d$Divorce),
    M = standardize(d$Marriage),
    A = standardize(d$MedianAgeMarriage),
    N = nrow(d)
    )

# Ulam model
m15.1 <- ulam(
    alist(
       D_obs ~ normal(D_true, D_sd),
       vector[N]:D_true ~ normal(mu, sigma),
       mu <- a + bA * A + bM *M,
       a ~ normal(0, 0.2),
       bA ~ normal(0, 0.5),
       bM ~ normal(0, 0.5),
       sigma ~ exponential(1)
    ), data=dat_list, chains = 4, cores = 4)

precis(m15.1)

# Stan model
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Adding masurement in the predictor variable
#

# Draw a DAG
dag <- dagitty("dag{
    A -> D
    A -> M
    D -> D_obs
    M -> D
    e_D -> D_obs
    M -> M_obs
    e_M -> M_obs
}")
# Note: Assumption of uncorrelated errors
plot(dag)

# Create a data list
dat_list <- list(
    "D_obs" = standardize(d$Divorce),
    "D_sd"  = d$Divorce.SE / sd(d$Divorce),
    "M_obs" = standardize(d$Marriage),
    "A" = standardize(d$MedianAgeMarriage),
    "N" = nrow(d),
    "M_sd"  = d$Marriage.SE / sd(d$MedianAgeMarriage)
    )

# Ulam version
m15.2 <- ulam(
    alist(
       D_obs ~ normal(D_true, D_sd),
       vector[N]:D_true ~ normal(mu, sigma),
       M_obs ~ normal(M_true, M_sd),
       vector[N]:M_true ~ normal(0, 1),
       mu <- a + bA * A + bM *M_obs,
       a ~ normal(0, 0.2),
       bA ~ normal(0, 0.5),
       bM ~ normal(0, 0.5),
       sigma ~ exponential(1)
    ), data=dat_list, chains = 4, cores = 4)

precis(m15.2)

stancode(m15.2)

# Stan version 
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Posterior
post <- fit$draws(format = "matrix")
Dbar_true <- fit$draws("D_true", format = "matrix") |> apply(2, mean)
Mbar_true <- fit$draws("M_true", format = "matrix") |> apply(2, mean)

plot( dat_list$M_obs, dat_list$D_obs, pch = 16, col = "steelblue" )
points(Mbar_true, Dbar_true)
N <- nrow(d)
for( i in 1:N ) {
    lines( c( dat_list$M_obs[i], Mbar_true[i]),  
    c(dat_list$D_obs[i], Dbar_true[i]) )
}

#
# Missing data
#

# Scenario A: Dog eats randomly homework
# Unconditional missing

# Draw a DAG
dag <- dagitty::dagitty('dag{
    S [pos="0,0"]
    H [pos="1,0"]
    H_ast [pos="1,1"]
    D [pos="0,1"]
    S -> H
    D -> H_ast 
    H -> H_ast 
}')
plot(dag)

# Simulate
set.seed(112)
N <- 1000
# Students effort (measured in time spent learning in h) 
S <- rnorm(N)
# Homework quality (1 to 10) - unobserved
H <- rbinom(N, size = 10, plogis(S))
# Missingness indicator (random sample)
D <- rbinom(N, size = 1, prob = 0.5)
# Homeqork quality – observed 
H_ast <- H 
H_ast[D == 1] <- NA ; H_ast

# Scenario B: Dog eats conditionally on effort 
# Conditional on S missing

# Draw a DAG
dag <- dagitty::dagitty('dag{
    S [pos="0,0"]
    H [pos="1,0"]
    H_ast [pos="1,1"]
    D [pos="0,1"]
    S -> H
    S -> D
    D -> H_ast 
    H -> H_ast 
}')
plot(dag)

# Simulate
set.seed(112)
N <- 1000
# Students effort (measured in time spent learning in h) 
S <- rnorm(N)
# Homework quality (1 to 10) - unobserved
H <- rbinom(N, size = 10, plogis(1*S))
# Missingness indicator: If students studys more than average (0), eat!
D <- ifelse(S > 0, 1, 0) 
# Homeqork quality – observed 
H_ast <- H 
H_ast[D == 1] <- NA ; H_ast

# Scenario C: Dog eats more homeworks in noisy homes

# Draw a DAG
dag <- dagitty::dagitty('dag{
    S [pos="0,0"]
    H [pos="1,0"]
    X [pos="0.5,0.5"]
    H_ast [pos="1,1"]
    D [pos="0,1"]
    S -> H
    D <- X -> H
    D -> H_ast 
    H -> H_ast 
}')
plot(dag)

# Simulate data
set.seed(112)
N <- 1000
# Students effort (measured in time spent learning in h) 
(S <- rnorm(N))
# Noisy level of home (here dogs misbehave)
(X <- rnorm(N))
# Homework quality (1 to 10) - unobserved
(H <- rbinom(N, size = 10, plogis(2 + 2*S - 2*X))) # Effect is 1
# Missingness indicator: If students studys more than average (0), eat!
D <- ifelse(X > 1, 1, 0) 
# Homeqork quality – observed 
H_ast <- H 
H_ast[D == 1] <- NA ; H_ast

# Data list 
dat_list <- list(
   "N" = N,
   "S" = S,
   # Assume we knew H (but H_ast)
   "H" = H
)

# Analyze 
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)
# a = 1.25 – true effect is 1

# Data list 
dat_list <- list(
   "N" = length(H[D==0]),
   "S" = S[D==0],
   # Assume we knew H (but H_ast)
   "H_ast" = H[D==0]
)

# Analyze 
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "4.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)
# a = 1.85 – true effect is 1 
# Measurement error makes thigs worse! 
# Omitted Variable Bias

# Scenario D

# Draw a DAG
dag <- dagitty::dagitty('dag{
    S [pos="0,0"]
    H [pos="1,0"]
    H_ast [pos="1,1"]
    D [pos="0,1"]
    S -> H
    H -> D
    D -> H_ast 
    H -> H_ast 
}')
plot(dag)

# Simulate data
set.seed(112)
N <- 1000
# Students effort (measured in time spent learning in h) 
S <- rnorm(N) ; head(S)
# Homework quality (1 to 10) - unobserved
(H <- rbinom(N, size = 10, plogis(1*S))) # Effect is 1
# Missingness indicator: Dog eat bad homework 
# ...students feed them 
D <- ifelse(H < 5, 1, 0) 
# Homeqork quality – observed 
H_ast <- H 
H_ast[D == 1] <- NA ; H_ast

#
# Imputing Primates
#
library(rethinking)
data(milk)
d <- milk
d$neocortex.perc.prop <- d$neocortex.perc/100
d$logmass <- log(d$mass)
dat_list <- list(
    K = rethinking::standardize(d$kcal.per.g),
    B = rethinking::standardize(d$neocortex.perc.prop),
    M = rethinking::standardize(d$logmass),
    B_missidx = ifelse(is.na(d$neocortex.perc), 1, 0)
)

m15.5 <- rethinking::ulam(
    alist(
        K ~ normal(mu, sigma),
        mu <- a + bB * B + bM*M,
        B ~ normal(nu, sigmaB),
        a ~ normal(0, 0.5),
        nu ~ normal(0, 0.5),
        bB ~ normal(0, 0.5),
        bM ~ normal(0, 0.5),
        sigmaB ~ exponential(1),
        sigma ~ exponential(1)
        ), data = dat_list, chains = 4, cores = 4)

precis(m15.5, depth = 2)

stancode(m15.5)

# TODO: Since ii_B_obs is an array index with upper bound N
# I musst add the position as indexwith

stan_list <- list(
    N = nrow(d),
    N_obs = sum(complete.cases(d)),
    ii_B_obs = which(!is.na(d$neocortex.perc)),
    N_mis = sum(!complete.cases(d)),
    ii_B_mis = which(is.na(d$neocortex.perc)),
    B_obs = dat_list$B[complete.cases(dat_list$B)],
    K = rethinking::standardize(d$kcal.per.g),
    M = rethinking::standardize(d$logmass)
)

stan_list

# Stan model
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "5.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_list)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)


# TODO Some NA values for Rhat....with ULAM not...
