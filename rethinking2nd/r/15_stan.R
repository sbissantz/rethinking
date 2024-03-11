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

# Stan model
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "5.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_list)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Posterior draws
postdraws <- fit$draws(format = "data.frame")
# Imputed Values
B_imp <- fit$draws("B_mis", format = "matrix")
# Mean 
B_imp_mu <- apply(B_imp, 2, mean)
# Compatability Interval
B_imp_ci <- apply(B_imp, 2, rethinking::PI)
# Visualize
plot(dat_list$B, dat_list$K, pch=16, col="steelblue")
Ki <- dat_list$K[stan_list$ii_B_mis]
points( B_imp_mu, Ki) 
for(i in 1:12 ) {
 lines(B_imp_ci[,i], rep(Ki[i],2))   
} 
# Visualize
plot(dat_list$M, dat_list$B, pch=16, col="steelblue")
Mi <- dat_list$M[stan_list$ii_B_mis]
points( B_imp_mu, Mi) 
for(i in 1:12 ) {
 lines(rep(Mi[i],2), B_imp_ci[,i])
} 

# Important: Imputed Values fail to follow the regression trend 
# A and M should be connected through U!

# Ulam version
#
m15.7 <- ulam(
alist(
# K as function of B and M
K ~ dnorm( mu , sigma ),
mu <- a + bB*B_merge + bM*M,
# M and B correlation
MB ~ multi_normal( c(muM,muB) , Rho_BM , Sigma_BM ),
matrix[29,2]:MB <<- append_col( M , B_merge ),
# define B_merge as mix of observed and imputed values
vector[29]:B_merge <- merge_missing( B , B_impute ),
# priors
c(a,muB,muM) ~ dnorm( 0 , 0.5 ),
c(bB,bM) ~ dnorm( 0, 0.5 ),
sigma ~ dexp( 1 ),
Rho_BM ~ lkj_corr(2),
Sigma_BM ~ dexp(1)
) , data=dat_list , chains=4 , cores=4 )

precis( m15.7 , depth=3 , pars=c("bM","bB","Rho_BM" ) )

stancode(m15.7)

# Stan version
#
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

# Stan model
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "6.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_list)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Posterior draws
postdraws <- fit$draws(format = "data.frame")
# Correlation thorugh including the residual confound: U
Rho_BM <- fit$draws("Rho_BM", format = "matrix")
# Imputed Values
B_imp <- fit$draws("B_mis", format = "matrix")
# Mean 
B_imp_mu <- apply(B_imp, 2, mean)
# Compatability Interval
B_imp_ci <- apply(B_imp, 2, rethinking::PI)


# Visualize the correlation BM through U
mean(Rho_BM[,2])
plot(density(Rho_BM[,2]))
# The mean correlation is .6 so this shows the strong correlation
# etween B and M that we knew it existed: U. 
# Import: This info should improve the imputation process and thus inference!

# Visualize
plot(dat_list$B, dat_list$K, pch=16, col="steelblue")
Ki <- dat_list$K[stan_list$ii_B_mis]
points( B_imp_mu, Ki) 
for(i in 1:12 ) {
 lines(B_imp_ci[,i], rep(Ki[i],2))   
} 

# Visualize
# Important! Now the lines follow the regression trend! 
# Modeling the unobserved confound using a multivariate model helped! 
plot(dat_list$M, dat_list$B, pch=16, col="steelblue")
Mi <- dat_list$M[stan_list$ii_B_mis]
points( B_imp_mu, Mi) 
for(i in 1:12 ) {
 lines(rep(Mi[i],2), B_imp_ci[,i])
} 

#
# Categorical errors and discrete absence
#

# Draw a DAG
dag <- dagitty::dagitty('dag{
    C [pos="0,0"]
    N [pos="1,0"]
    C_ast [pos="-1,0"]
    mC [pos="-2,0"]
    C -> N 
    C -> C_ast
    mC -> C_ast
}')
plot(dag)

# Generative simulation
#
# N_i ~ Poisson(lambda_i)
# log(lambda_i) = a + b * C_i
# C_i ~ Bernoulli(k) 
# mC_i ~ Bernoulli(k)

# Number of households
set.seed(9)
N_houses <- 100L
alpha <- 5
beta <- (-3)
k <- 0.5
r <- 0.2
cat <- rethinking::rbern( N_houses , k )
notes <- rpois( N_houses , alpha + beta*cat )
R_C <- rethinking::rbern( N_houses , r )
cat_obs <- cat
cat_obs[R_C==1] <- (-9L)
dat <- list(
notes = notes,
cat = cat_obs,
RC = R_C,
N = as.integer(N_houses) )

m15.8 <- rethinking::ulam(
    alist(
        # singing bird model
        ## cat known present/absent:
        notes|RC==0 ~ poisson( lambda ),
        log(lambda) <- a + b*cat,
        ## cat NA:
        notes|RC==1 ~ custom( log_sum_exp(
        log(k) + poisson_lpmf( notes | exp(a + b) ),
        log(1-k) + poisson_lpmf( notes | exp(a) )
        ) ),
        # priors
        a ~ normal(0,1),
        b ~ normal(0,0.5),
        # sneaking cat model
        cat|RC==0 ~ bernoulli(k),
        k ~ beta(2,2)
    ), data=dat , chains=4 , cores=4 )

precis(m15.8)

stan_ls <- list("N" = N_houses, "RC" = R_C, "notes" = notes, "cat" = cat)

# Stanmodel
#
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "8.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

m15.9 <- ulam(
    alist(
        # singing bird model
        notes|RC==0 ~ poisson( lambda ),
        notes|RC==1 ~ custom( log_sum_exp(
        log(k) + poisson_lpmf( notes | exp(a + b) ),
        log(1-k) + poisson_lpmf( notes | exp(a) )
        ) ),
        log(lambda) <- a + b*cat,
        a ~ normal(0,1),
        b ~ normal(0,0.5),
        # sneaking cat model
        cat|RC==0 ~ bernoulli(k),
        k ~ beta(2,2),
        # imputed values
        gq> vector[N]:PrC1 <- exp(lpC1)/(exp(lpC1)+exp(lpC0)),
        gq> vector[N]:lpC1 <- log(k) + poisson_lpmf( notes[i] | exp(a+b) ),
        gq> vector[N]:lpC0 <- log(1-k) + poisson_lpmf( notes[i] | exp(a) )
), data=dat , chains=4 , cores=4 )

precis(m15.9, depth = 3)

a           1.61 0.06   1.50   1.71  1342     1
b          -0.79 0.12  -0.99  -0.60  1392     1
k           0.46 0.05   0.38   0.55  1305     1
lpC0[1]    -3.96 0.27  -4.40  -3.54  1431     1
lpC0[2]    -2.56 0.12  -2.75  -2.38  1481     1
lpC0[3]    -2.37 0.12  -2.57  -2.18  1300     1
lpC0[4]    -2.59 0.17  -2.87  -2.34  1274     1
lpC0[5]    -2.59 0.17  -2.87  -2.34  1274     1
lpC0[6]    -2.56 0.12  -2.75  -2.38  1481     1
lpC0[7]    -2.37 0.10  -2.54  -2.22  1365     1
lpC0[8]    -2.37 0.12  -2.57  -2.18  1300     1
lpC0[9]    -3.10 0.22  -3.46  -2.76  1277     1
lpC0[10]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[11]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[12]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[13]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[14]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[15]   -2.56 0.12  -2.75  -2.38  1481     1
lpC0[16]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[17]   -5.45 0.38  -6.09  -4.84  1402     1
lpC0[18]   -5.61 0.34  -6.18  -5.09  1292     1
lpC0[19]   -3.37 0.21  -3.72  -3.05  1445     1
lpC0[20]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[21]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[22]   -2.90 0.16  -3.16  -2.66  1466     1
lpC0[23]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[24]   -3.37 0.21  -3.72  -3.05  1445     1
lpC0[25]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[26]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[27]   -2.56 0.12  -2.75  -2.38  1481     1
lpC0[28]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[29]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[30]   -5.61 0.34  -6.18  -5.09  1292     1
lpC0[31]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[32]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[33]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[34]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[35]   -3.96 0.27  -4.40  -3.54  1431     1
lpC0[36]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[37]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[38]   -5.61 0.34  -6.18  -5.09  1292     1
lpC0[39]   -3.96 0.27  -4.40  -3.54  1431     1
lpC0[40]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[41]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[42]   -5.61 0.34  -6.18  -5.09  1292     1
lpC0[43]   -5.61 0.34  -6.18  -5.09  1292     1
lpC0[44]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[45]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[46]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[47]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[48]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[49]   -2.56 0.12  -2.75  -2.38  1481     1
lpC0[50]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[51]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[52]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[53]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[54]   -2.56 0.12  -2.75  -2.38  1481     1
lpC0[55]   -2.90 0.16  -3.16  -2.66  1466     1
lpC0[56]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[57]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[58]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[59]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[60]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[61]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[62]   -3.37 0.21  -3.72  -3.05  1445     1
lpC0[63]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[64]   -3.37 0.21  -3.72  -3.05  1445     1
lpC0[65]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[66]   -3.37 0.21  -3.72  -3.05  1445     1
lpC0[67]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[68]   -2.56 0.12  -2.75  -2.38  1481     1
lpC0[69]   -5.61 0.34  -6.18  -5.09  1292     1
lpC0[70]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[71]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[72]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[73]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[74]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[75]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[76]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[77]   -5.61 0.34  -6.18  -5.09  1292     1
lpC0[78]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[79]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[80]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[81]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[82]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[83]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[84]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[85]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[86]   -4.66 0.32  -5.20  -4.14  1414     1
lpC0[87]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[88]   -2.37 0.10  -2.54  -2.22  1365     1
lpC0[89]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[90]   -4.01 0.28  -4.47  -3.58  1285     1
lpC0[91]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[92]   -2.37 0.12  -2.57  -2.18  1300     1
lpC0[93]   -2.59 0.17  -2.87  -2.34  1274     1
lpC0[94]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[95]   -2.90 0.16  -3.16  -2.66  1466     1
lpC0[96]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[97]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[98]   -3.10 0.22  -3.46  -2.76  1277     1
lpC0[99]   -2.56 0.12  -2.75  -2.38  1481     1
lpC0[100]  -2.56 0.12  -2.75  -2.38  1481     1
lpC1[1]    -8.53 0.74  -9.75  -7.40  1943     1
lpC1[2]    -4.75 0.43  -5.47  -4.10  1892     1
lpC1[3]    -2.97 0.23  -3.36  -2.62  1725     1
lpC1[4]    -2.40 0.15  -2.66  -2.18  1532     1
lpC1[5]    -2.40 0.15  -2.66  -2.18  1532     1
lpC1[6]    -4.75 0.43  -5.47  -4.10  1892     1
lpC1[7]    -3.77 0.32  -4.33  -3.27  1839     1
lpC1[8]    -2.97 0.23  -3.36  -2.62  1725     1
lpC1[9]    -2.12 0.12  -2.32  -1.94  1290     1
lpC1[10]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[11]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[12]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[13]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[14]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[15]   -4.75 0.43  -5.47  -4.10  1892     1
lpC1[16]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[17]  -11.61 0.95 -13.17 -10.14  1954     1
lpC1[18]   -3.05 0.26  -3.47  -2.66  1701     1
lpC1[19]   -7.15 0.63  -8.21  -6.18  1934     1
lpC1[20]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[21]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[22]   -5.88 0.53  -6.77  -5.07  1918     1
lpC1[23]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[24]   -7.15 0.63  -8.21  -6.18  1934     1
lpC1[25]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[26]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[27]   -4.75 0.43  -5.47  -4.10  1892     1
lpC1[28]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[29]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[30]   -3.05 0.26  -3.47  -2.66  1701     1
lpC1[31]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[32]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[33]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[34]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[35]   -8.53 0.74  -9.75  -7.40  1943     1
lpC1[36]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[37]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[38]   -3.05 0.26  -3.47  -2.66  1701     1
lpC1[39]   -8.53 0.74  -9.75  -7.40  1943     1
lpC1[40]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[41]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[42]   -3.05 0.26  -3.47  -2.66  1701     1
lpC1[43]   -3.05 0.26  -3.47  -2.66  1701     1
lpC1[44]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[45]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[46]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[47]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[48]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[49]   -4.75 0.43  -5.47  -4.10  1892     1
lpC1[50]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[51]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[52]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[53]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[54]   -4.75 0.43  -5.47  -4.10  1892     1
lpC1[55]   -5.88 0.53  -6.77  -5.07  1918     1
lpC1[56]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[57]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[58]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[59]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[60]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[61]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[62]   -7.15 0.63  -8.21  -6.18  1934     1
lpC1[63]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[64]   -7.15 0.63  -8.21  -6.18  1934     1
lpC1[65]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[66]   -7.15 0.63  -8.21  -6.18  1934     1
lpC1[67]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[68]   -4.75 0.43  -5.47  -4.10  1892     1
lpC1[69]   -3.05 0.26  -3.47  -2.66  1701     1
lpC1[70]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[71]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[72]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[73]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[74]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[75]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[76]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[77]   -3.05 0.26  -3.47  -2.66  1701     1
lpC1[78]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[79]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[80]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[81]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[82]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[83]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[84]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[85]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[86]  -10.02 0.84 -11.41  -8.72  1950     1
lpC1[87]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[88]   -3.77 0.32  -4.33  -3.27  1839     1
lpC1[89]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[90]   -2.24 0.17  -2.52  -1.99  1454     1
lpC1[91]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[92]   -2.97 0.23  -3.36  -2.62  1725     1
lpC1[93]   -2.40 0.15  -2.66  -2.18  1532     1
lpC1[94]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[95]   -5.88 0.53  -6.77  -5.07  1918     1
lpC1[96]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[97]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[98]   -2.12 0.12  -2.32  -1.94  1290     1
lpC1[99]   -4.75 0.43  -5.47  -4.10  1892     1
lpC1[100]  -4.75 0.43  -5.47  -4.10  1892     1
PrC1[1]     0.01 0.01   0.00   0.03  1442     1
PrC1[2]     0.11 0.04   0.05   0.19  1673     1
PrC1[3]     0.36 0.07   0.25   0.47  1643     1
PrC1[4]     0.55 0.07   0.43   0.65  1414     1
PrC1[5]     0.55 0.07   0.43   0.65  1414     1
PrC1[6]     0.11 0.04   0.05   0.19  1673     1
PrC1[7]     0.20 0.06   0.12   0.31  1728     1
PrC1[8]     0.36 0.07   0.25   0.47  1643     1
PrC1[9]     0.72 0.06   0.62   0.81  1171     1
PrC1[10]    0.85 0.05   0.77   0.91  1081     1
PrC1[11]    0.55 0.07   0.43   0.65  1414     1
PrC1[12]    0.36 0.07   0.25   0.47  1643     1
PrC1[13]    0.72 0.06   0.62   0.81  1171     1
PrC1[14]    0.20 0.06   0.12   0.31  1728     1
PrC1[15]    0.11 0.04   0.05   0.19  1673     1
PrC1[16]    0.85 0.05   0.77   0.91  1081     1
PrC1[17]    0.00 0.00   0.00   0.01  1382     1
PrC1[18]    0.92 0.03   0.86   0.96  1068     1
PrC1[19]    0.03 0.02   0.01   0.06  1511     1
PrC1[20]    0.55 0.07   0.43   0.65  1414     1
PrC1[21]    0.72 0.06   0.62   0.81  1171     1
PrC1[22]    0.06 0.03   0.02   0.11  1589     1
PrC1[23]    0.72 0.06   0.62   0.81  1171     1
PrC1[24]    0.03 0.02   0.01   0.06  1511     1
PrC1[25]    0.55 0.07   0.43   0.65  1414     1
PrC1[26]    0.72 0.06   0.62   0.81  1171     1
PrC1[27]    0.11 0.04   0.05   0.19  1673     1
PrC1[28]    0.36 0.07   0.25   0.47  1643     1
PrC1[29]    0.85 0.05   0.77   0.91  1081     1
PrC1[30]    0.92 0.03   0.86   0.96  1068     1
PrC1[31]    0.55 0.07   0.43   0.65  1414     1
PrC1[32]    0.55 0.07   0.43   0.65  1414     1
PrC1[33]    0.36 0.07   0.25   0.47  1643     1
PrC1[34]    0.55 0.07   0.43   0.65  1414     1
PrC1[35]    0.01 0.01   0.00   0.03  1442     1
PrC1[36]    0.55 0.07   0.43   0.65  1414     1
PrC1[37]    0.55 0.07   0.43   0.65  1414     1
PrC1[38]    0.92 0.03   0.86   0.96  1068     1
PrC1[39]    0.01 0.01   0.00   0.03  1442     1
PrC1[40]    0.85 0.05   0.77   0.91  1081     1
PrC1[41]    0.72 0.06   0.62   0.81  1171     1
PrC1[42]    0.92 0.03   0.86   0.96  1068     1
PrC1[43]    0.92 0.03   0.86   0.96  1068     1
PrC1[44]    0.36 0.07   0.25   0.47  1643     1
PrC1[45]    0.55 0.07   0.43   0.65  1414     1
PrC1[46]    0.36 0.07   0.25   0.47  1643     1
PrC1[47]    0.85 0.05   0.77   0.91  1081     1
PrC1[48]    0.36 0.07   0.25   0.47  1643     1
PrC1[49]    0.11 0.04   0.05   0.19  1673     1
PrC1[50]    0.20 0.06   0.12   0.31  1728     1
PrC1[51]    0.85 0.05   0.77   0.91  1081     1
PrC1[52]    0.20 0.06   0.12   0.31  1728     1
PrC1[53]    0.36 0.07   0.25   0.47  1643     1
PrC1[54]    0.11 0.04   0.05   0.19  1673     1
PrC1[55]    0.06 0.03   0.02   0.11  1589     1
PrC1[56]    0.36 0.07   0.25   0.47  1643     1
PrC1[57]    0.85 0.05   0.77   0.91  1081     1
PrC1[58]    0.72 0.06   0.62   0.81  1171     1
PrC1[59]    0.72 0.06   0.62   0.81  1171     1
PrC1[60]    0.55 0.07   0.43   0.65  1414     1
PrC1[61]    0.20 0.06   0.12   0.31  1728     1
PrC1[62]    0.03 0.02   0.01   0.06  1511     1
PrC1[63]    0.55 0.07   0.43   0.65  1414     1
PrC1[64]    0.03 0.02   0.01   0.06  1511     1
PrC1[65]    0.72 0.06   0.62   0.81  1171     1
PrC1[66]    0.03 0.02   0.01   0.06  1511     1
PrC1[67]    0.36 0.07   0.25   0.47  1643     1
PrC1[68]    0.11 0.04   0.05   0.19  1673     1
PrC1[69]    0.92 0.03   0.86   0.96  1068     1
PrC1[70]    0.85 0.05   0.77   0.91  1081     1
PrC1[71]    0.20 0.06   0.12   0.31  1728     1
PrC1[72]    0.36 0.07   0.25   0.47  1643     1
PrC1[73]    0.55 0.07   0.43   0.65  1414     1
PrC1[74]    0.72 0.06   0.62   0.81  1171     1
PrC1[75]    0.55 0.07   0.43   0.65  1414     1
PrC1[76]    0.55 0.07   0.43   0.65  1414     1
PrC1[77]    0.92 0.03   0.86   0.96  1068     1
PrC1[78]    0.20 0.06   0.12   0.31  1728     1
PrC1[79]    0.55 0.07   0.43   0.65  1414     1
PrC1[80]    0.36 0.07   0.25   0.47  1643     1
PrC1[81]    0.36 0.07   0.25   0.47  1643     1
PrC1[82]    0.20 0.06   0.12   0.31  1728     1
PrC1[83]    0.55 0.07   0.43   0.65  1414     1
PrC1[84]    0.72 0.06   0.62   0.81  1171     1
PrC1[85]    0.55 0.07   0.43   0.65  1414     1
PrC1[86]    0.01 0.01   0.00   0.02  1399     1
PrC1[87]    0.55 0.07   0.43   0.65  1414     1
PrC1[88]    0.20 0.06   0.12   0.31  1728     1
PrC1[89]    0.85 0.05   0.77   0.91  1081     1
PrC1[90]    0.85 0.05   0.77   0.91  1081     1
PrC1[91]    0.36 0.07   0.25   0.47  1643     1
PrC1[92]    0.36 0.07   0.25   0.47  1643     1
PrC1[93]    0.55 0.07   0.43   0.65  1414     1
PrC1[94]    0.72 0.06   0.62   0.81  1171     1
PrC1[95]    0.06 0.03   0.02   0.11  1589     1
PrC1[96]    0.72 0.06   0.62   0.81  1171     1
PrC1[97]    0.72 0.06   0.62   0.81  1171     1
PrC1[98]    0.72 0.06   0.62   0.81  1171     1
PrC1[99]    0.11 0.04   0.05   0.19  1673     1
PrC1[100]   0.11 0.04   0.05   0.19  1673     1

# Stanmodel
#
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "15", "9.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)
