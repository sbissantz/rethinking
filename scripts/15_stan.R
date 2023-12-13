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



