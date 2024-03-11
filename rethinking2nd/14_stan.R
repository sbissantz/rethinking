#
# Adventure in Covariance
#

# 14.1 Varying slopes by construction
#

# Simulate the population
# (Data Generating Process)
#
# Average morging wait time (min)
a <- 3.5
# Average diverence afternoon wait time (min)
b <- (-1) 
# Standard deviation in intercepts 
sigma_a <- 1
# Standard deviation in slopes
sigma_b <- 0.5
# Correlation between intercepts and slopes
rho <- (-0.7)
# Population: 2D-Gaussian (means, variances, covariances)
#
# Mean Vector 
Mu <- c(a,b)

# Covariance Matrix
cov_ab <- sigma_a * sigma_b * rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), nrow = 2, ncol = 2)

# Alternative
sigmas <- c(sigma_a, sigma_b)
Rho <- matrix(c(1, rho, rho, 1), nrow = 2)

# Matrix multiply
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

# Number of cafes
N_cafes <- 20

# Sample from MVN
set.seed(5)
vary_effects <- MASS::mvrnorm(N_cafes, Mu, Sigma)
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

# Visualize
#
plot(a_cafe, b_cafe, xlab = "Intercept", ylab = "Slope", pch = 16, col =
 "steelblue")
library(ellipse)
sequence <- c(0.1, 0.3, 0.5, 0.8, 0.99)
for (l in sequence) {
    lines(ellipse(Sigma, centre = Mu, level = l), col = "black")
}

# Simulate observations
#
set.seed(123)

# Number of visits per cafe
N_visits <- 10

# Indicator for afternoon
afternoon <- rep(0:1, N_visits * N_cafes/2)

# Cafe ID
cafe_id <- rep(1:N_cafes, each = N_visits)

# Simulate average wait time within cafe
mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon

# Standard deviation in wait time within cafe
sigma <- 0.5

# Simulate wait time within cafe
wait <- rnorm(N_visits * N_cafes, mu, sigma)

# Create a balanced data frame
(d <- data.frame("cafe" = cafe_id, "afternoon" = afternoon, "wait" = wait))

curve(dlnorm(x, 1, 0.5), from = 0, to = 10, lwd = 2, col = "steelblue", ylab =
      "Density", xlab = "Wait time (min)")

# Varying slopes model
library(rethinking)
# LKJcorr prior
R <- rethinking::rlkjcorr(1e4, K=2, eta=1)[,2,1] 
plot(density(R) , main = "LKJcorr prior", xlab = "Correlation", lwd = 2, col =
     "steelblue", xlim = c(-1,1))

set.seed(867530)
m14.1 <- ulam(
    alist(
        wait ~ normal(mu, sigma),
        mu <- a_cafe[cafe] + b_cafe[cafe] * afternoon,
        c(a_cafe, b_cafe)[cafe] ~ multi_normal(c(a,b), Rho, sigma_cafe),
        # Prior for the average intercept 
        # (Average waiting time across cafes)
        a ~ normal(5, 2),
        # Prior for the average slope 
        # (Difference morning and afternoon waiting time)
        b ~ normal(-1, 0.5),
        # Prior for the standard deviation within cafe
        sigma_cafe ~ exponential(1),
        # Prior for the standard deviation among cafe
        sigma ~ exponential(1),
        # Prior for the correlation between intercepts and slopes
        Rho ~ lkj_corr(2)
    ), data = d, chains = 4, cores = 4 
)
stancode(m14.1)

# BRMS model
library(brms)
 b14.1 <- 
  brm(data = d, family = gaussian,
      wait ~ 1 + afternoon + (1 + afternoon | cafe),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 2), class = sd),
                prior(cauchy(0, 2), class = sigma),
                prior(lkj(2), class = cor)),
      iter = 5000, warmup = 2000, chains = 2, cores = 2,
      seed = 13)

brms::stancode(b14.1)
post <- posterior_samples(b13.1)

# Divergences....
post <- extract.samples(m14.1)
dens( post$Rho[,1,2] , xlim = c(-1,1) )
R <- rlkjcorr(1e4, K = 2, eta = 2)
dens( R[,1,2] , add = TRUE, lty = 2, lwd = 2, col = "steelblue" ) 

# Data list
#
dat_ls <- list(
    wait = d$wait,
    afternoon = d$afternoon,
    cafe = d$cafe,
    n_i = N_visits*N_cafes,
    n_j = N_cafes
)

# Fit the model 
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "14", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose()
fit$diagnostic_summary()

# Posterior
#
post <- fit$draws(format = "matrix") 
prior_draws_Rho <- rethinking::rlkjcorr(1e4, K = 2, eta = 2)
post_draws_Rho <- post[,grep("Rho", colnames(post), value = TRUE)]
plot(density(post_draws_Rho[,2]), main = "Rho Posterior", xlab = "Correlation", 
lwd = 3, col = "steelblue")
lines( density(prior_draws_Rho[,2,1]) )

# Compute unpooled estimates
a1 <- sapply(1:N_cafes, 
             function(i) mean(d$wait[cafe_id==i & d$afternoon==0]) )
b1 <- sapply(1:N_cafes, 
             function(i) mean(d$wait[cafe_id==i & d$afternoon==1]) ) - a1
# Posterior mean of partially pooled estimates
# Note: Dont name variables a and b! use alpha and beta!
# Why? Because it makes extracting variables with grep easier!
a2 <- apply(post[,grep("a_j", colnames(post))], 2, mean)[1:20]
b2 <- apply(post[,grep("b_j", colnames(post))], 2, mean)
plot(a1, b1, pch = 16, col = "steelblue", xlab = "Intercept", ylab = "Slope")
points(a2, b2, pch = 16, col = "orange")
for( i in 1:N_cafes ) lines( c(a1[i],a2[i]), c(b1[i],b2[i]) )

# Compute the posterior mean bivariate Gaussian
Mu_est <- c( mean( post[,"a"] ) , mean( post[,"b"] ) )
rho_est <- mean( post[,"Rho[1,2]"] )
sa_est <- mean( post[,"sigma_j[1]"] )
sb_est <- mean( post[,"sigma_j[2]"] )
cov_ab <- sa_est * sb_est * rho_est
Sigma_est <- matrix( c(sa_est^2, cov_ab, cov_ab, sb_est^2), ncol = 2 )

for(l in c(0.1,0.3,0.5,0.8,0.99)) {
    lines( ellipse(Sigma_est, centre = Mu_est, level = l), col = "black" )
}

#
# Advanced varying slopes model
#
library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$block_id <- d$block
d$treatment <- 1L + d$prosoc_left + 2L*d$condition

dat <- list(
    L = d$pulled_left,
    tid = d$treatment,
    actor = d$actor,
    block_id = as.integer(d$block_id) )

set.seed(4387510)

m14.2 <- ulam(
    alist(
        L ~ dbinom(1,p),
        logit(p) <- g[tid] + alpha[actor,tid] + beta[block_id,tid],
        # adaptive priors
        vector[4]:alpha[actor] ~ multi_normal(0,Rho_actor,sigma_actor),
        vector[4]:beta[block_id] ~ multi_normal(0,Rho_block,sigma_block),
        # fixed priors
        g[tid] ~ dnorm(0,1),
        sigma_actor ~ dexp(1),
        Rho_actor ~ dlkjcorr(4),
        sigma_block ~ dexp(1),
        Rho_block ~ dlkjcorr(4)
    ) , data=dat , chains=4 , cores=4 )

stancode(m14.2)

dat_ls <- list(
    "L" = d$pulled_left,
    "treatment_id" = d$treatment,
    "actor_id" = d$actor,
    "block_id" = as.integer(d$block_id) 
)

# Fit the model 
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "14", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose()
fit$diagnostic_summary()

# Posterior
#
post <- fit$draws(format = "matrix") 

#
# Cholesky factorization
#

dat_ls <- list(
    "L" = d$pulled_left,
    "tid" = d$treatment,
    "actor" = d$actor,
    "block_id" = as.integer(d$block_id) 
)

m14.3 <- ulam(
    alist(
        L ~ binomial(1,p),
        logit(p) <- g[tid] + alpha[actor,tid] + beta[block_id,tid],
        # adaptive priors - non-centered
        transpars> matrix[actor,4]:alpha <-
                compose_noncentered( sigma_actor , L_Rho_actor , z_actor ),
        transpars> matrix[block_id,4]:beta <-
                compose_noncentered( sigma_block , L_Rho_block , z_block ),
        matrix[4,actor]:z_actor ~ normal( 0 , 1 ),
        matrix[4,block_id]:z_block ~ normal( 0 , 1 ),
        # fixed priors
        g[tid] ~ normal(0,1),
        vector[4]:sigma_actor ~ dexp(1),
        cholesky_factor_corr[4]:L_Rho_actor ~ lkj_corr_cholesky( 2 ),
        vector[4]:sigma_block ~ dexp(1),
        cholesky_factor_corr[4]:L_Rho_block ~ lkj_corr_cholesky( 2 ),
        # compute ordinary correlation matrixes from Cholesky factors
        gq> matrix[4,4]:Rho_actor <<- Chol_to_Corr(L_Rho_actor),
        gq> matrix[4,4]:Rho_block <<- Chol_to_Corr(L_Rho_block)
    ) , data=dat_ls , chains=4 , cores=4 , log_lik=TRUE )

stancode(m14.3)

path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "14", "3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat)
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Further improvmenets (lecture 2023)
library(rethinking)
data(chimpanzees)
d <- chimpanzees
dat <- list(
P = d$pulled_left,
A = as.integer(d$actor),
B = as.integer(d$block),
T = 1L + d$prosoc_left + 2L*d$condition)

m14.3 <- ulam(
alist(
P ~ bernoulli(p),
logit(p) <- abar[A] + a[A,T] + bbar[B] + b[B,T],
# adaptive priors - non-centered
transpars> matrix[A,4]:a <-
compose_noncentered( sigma_A , L_Rho_A , zA ),
transpars> matrix[B,4]:b <-
compose_noncentered( sigma_B , L_Rho_B , zB ),
matrix[4,A]:zA ~ normal( 0 , 1 ),
matrix[4,B]:zB ~ normal( 0 , 1 ),
zAbar[A] ~ normal(0,1),
zBbar[B] ~ normal(0,1),
transpars> vector[A]:abar <<- zAbar*tau_A,
transpars> vector[B]:bbar <<- zBbar*tau_B,
# fixed priors
c(tau_A,tau_B) ~ exponential(1),
vector[4]:sigma_A ~ exponential(1),
cholesky_factor_corr[4]:L_Rho_A ~ lkj_corr_cholesky(4),
vector[4]:sigma_B ~ exponential(1),
cholesky_factor_corr[4]:L_Rho_B ~ lkj_corr_cholesky(4),
# compute ordinary correlation matrixes
gq> matrix[4,4]:Rho_A <<- Chol_to_Corr(L_Rho_A),
gq> matrix[4,4]:Rho_B <<- Chol_to_Corr(L_Rho_B)
) , data=dat , chains=4 , cores=4 , log_lik=TRUE )

stancode(m14.3)

#
# 14.3 Instruments and Causal Design
#
set.seed(112)
# Generative Simulation
N <- 1e3
U_sim <- rnorm(N, 0, 1)
# Quarters
Q_sim <- sample( 1:4, size=N, replace = TRUE)
E_sim <- rnorm(N, Q_sim + U_sim)
# Set the Tx effect (E -> D) to 0
W_sim <- rnorm(N, 0*E_sim + U_sim)

# Add the (standardized) variables to a list
dat_sim <- list(
    N = N,    
    W = as.numeric(scale(W_sim)),
    E = as.numeric(scale(E_sim)),
    Q = as.numeric(scale(Q_sim))
)

# Statistical model I
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "4.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_sim)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)
# Answer: beta_EW = 0.41. The confound does its job!

# Statistical model II
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "5.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_sim)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)
# Answer: beta_EW = 0.64. U amplifies the bias of the confound! 

# Statistical model III
#

# Ulam
m14.6 <- rethinking::ulam(
    alist(
        c(W,E) ~ multi_normal( c(muW,muE) , Rho , Sigma ),
        muW <- aW + bEW*E,
        muE <- aE + bQE*Q,
        c(aW,aE) ~ normal( 0 , 0.2 ),
        c(bEW,bQE) ~ normal( 0 , 0.5 ),
        Rho ~ lkj_corr( 2 ),
        Sigma ~ exponential( 1 )
    ), data=dat_sim , chains=4 , cores=4)
rethinking::precis( m14.6 , depth=3 )
rethinking::stancode(m14.6)

# Stan
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "6.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_sim)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)
# Answer: beta_EW = 0. The new model deconfounds the estimate

########################################################
# 14.4 Social relations and correlated varying effects #
########################################################

# Load required packages
# Note: dagitty is loaded by rethinking
library(rethinking)

# Get informed
?KosterLeckie

# Load data
data(KosterLeckie)

# Data list
kl_data <- list(
    N = nrow(kl_dyads),
    N_households = max(kl_dyads$hidB),
    did = kl_dyads$did, 
    hidA = kl_dyads$hidA,
    hidB = kl_dyads$hidB,
    giftsAB = kl_dyads$giftsAB,
    giftsBA = kl_dyads$giftsBA
    )

# Statistical model I

# Ulam model
m14.7 <- ulam(
    alist(
        giftsAB ~ poisson( lambdaAB ),
        giftsBA ~ poisson( lambdaBA ),
        log(lambdaAB) <- a + gr[hidA,1] + gr[hidB,2] + d[did,1] ,
        log(lambdaBA) <- a + gr[hidB,1] + gr[hidA,2] + d[did,2] ,
        a ~ normal(0,1),
       ## gr matrix of varying effects
        vector[2]:gr[N_households] ~ multi_normal(0,Rho_gr,sigma_gr),
        Rho_gr ~ lkj_corr(4),
        sigma_gr ~ exponential(1),
       ## dyad effects
        transpars> matrix[N,2]:d <-
                compose_noncentered( rep_vector(sigma_d,2) , L_Rho_d , z ),
        matrix[2,N]:z ~ normal( 0 , 1 ),
        cholesky_factor_corr[2]:L_Rho_d ~ lkj_corr_cholesky( 8 ),
        sigma_d ~ exponential(1),
       ## compute correlation matrix for dyads
        gq> matrix[2,2]:Rho_d <<- Chol_to_Corr( L_Rho_d )
    ), data=kl_data , chains=4 , cores=4 , iter=2000 )

stancode(m14.7)

# Stan model
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "7.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=kl_data)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

#
# Gaussian process
#

# Load packages
library(rethinking)

# Load data
data(islandsDistMatrix)
data(Kline2)
d <- Kline2
# Number of cases
N <- nrow(d)
# Indicator variable
d$society <- seq(N)

# Distance matrix 
Dmat <- islandsDistMatrix
colnames(Dmat) <- abbreviate(rownames(Dmat), 2)
round(Dmat, 1) # Distance between island i and j

# Covariance function
# ...relates correlation to distance
curve( exp(-x), from = 0, to = 4, lwd = 3) # Linear (L1)
curve( exp(-x^2), from = 0, to = 4, lwd = 3, col = "steelblue",
    add = TRUE) # Squared exponential (L2)

# Data list
dat_list <- list(
    T = d$total_tools,
    P = d$population,
    society = d$society,
    Dmat = islandsDistMatrix
)

# Ulam 
m14.8 <- ulam(
    alist(
        T ~ dpois(lambda),
        lambda <- (a*P^b/g)*exp(k[society]),
        vector[10]:k ~ multi_normal( 0 , SIGMA ),
        matrix[10,10]:SIGMA <- cov_GPL2( Dmat , etasq , rhosq , 0.01 ),
        c(a,b,g) ~ dexp( 1 ),
        etasq ~ dexp( 2 ),
        rhosq ~ dexp( 0.5 )
    ), data=dat_list , chains=4 , cores=4 , iter=2000 )

precis(m14.8, depth = 3)
stancode(m14.8)

# Data list
dat_list <- list(
    N = nrow(d),
    T = d$total_tools,
    P = d$population,
    society = d$society,
    Dmat = islandsDistMatrix
)

path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "8a.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$print(max_rows=200)

# 8b does not provide the same pproximation, because values have to be
# pre-set. Solution in 8a is much more elegant
#path <- "~/projects/rethinking2nd"
#file <- file.path(path, "stan", "14", "8b.stan")
#mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
#fit <- mdl$sample(data=dat_list)
#Note that the alpha parameters are on the log-odds scale!
#fit$print(max_rows=200)

fit$sampler_diagnostics()
fit$diagnostic_summary()

# Posterior draws
praws <- fit$draws(format = "data.frame")

# Emptyplot
plot(NULL, xlab = "Distance (thousand km)", ylab = "covariance",
xlim = c(0,10), ylim = c(0,0.5))
# Compute posterior mean covariance
x_seq <- seq(0, 10, length.out = 100 )
# Posterior mean covariances
pmcov <- sapply( x_seq, function(x) praws$etasq * exp(-praws$rhosq*x^2) )
pmcov_mu <- apply( pmcov, 2, mean)
# Mean relationsship between covariance and distance
lines( x_seq, pmcov_mu, lwd = 5, col = "steelblue" )
# Relationsship between covariance and distance
for(i in 1:50) {
    curve(praws$etasq[i] * exp(-praws$rhosq[i]*x^2), add = TRUE
    )
}

# Compute the posterior median covariance among societies 
K <- matrix(0, nrow = 10, ncol = 10)
for (i in 1:10) {
    for(j in 1:10) {
        K[i,j] <- median(praws$etasq) * exp (-median(praws$rhosq) * islandsDistMatrix[i,j]^2)
    }
}
diag(K) <- median(praws$etasq) + 0.01
# Convert to correlation matrix
Rho <- round( cov2cor(K), 2 )
rownames(Rho) <- colnames(Dmat)
colnames(Rho) <- colnames(Dmat)

# scale point size to logpop
psize <- d$logpop / max(d$logpop)
psize <- exp(psize*1.5)-2
# plot raw data and labels
plot( d$lon2 , d$lat , xlab="longitude" , ylab="latitude" ,
    col=rangi2 , cex=psize , pch=16 , xlim=c(-50,30) )
labels <- as.character(d$culture)
text( d$lon2 , d$lat , labels=labels , cex=0.7 , pos=c(2,4,3,3,4,1,3,2,4,2) )
# overlay lines shaded by Rho
for( i in 1:10 )
    for ( j in 1:10 )
        if ( i < j )
            lines( c( d$lon2[i],d$lon2[j] ) , c( d$lat[i],d$lat[j] ) ,
                lwd=2 , col=col.alpha("black",Rho[i,j]^2) )


colnames(praws)
# compute posterior median relationship, ignoring distance
logpop.seq <- seq( from=6 , to=14 , length.out=30 )
lambda <- sapply( logpop.seq , function(lp) exp( praws$a + praws$b*lp ) )
lambda.median <- apply( lambda , 2 , median )
lambda.PI80 <- apply( lambda , 2 , PI , prob=0.8 )
# plot raw data and labels
plot( d$logpop , d$total_tools , col=rangi2 , cex=psize , pch=16 ,
    xlab="log population" , ylab="total tools" )
text( d$logpop , d$total_tools , labels=labels , cex=0.7 ,
    pos=c(4,3,4,2,2,1,4,4,4,2) )
# display posterior predictions
lines( logpop.seq , lambda.median , lty=2 )
lines( logpop.seq , lambda.PI80[1,] , lty=2 )
lines( logpop.seq , lambda.PI80[2,] , lty=2 )
# overlay correlations
for( i in 1:10 )
    for ( j in 1:10 )
        if ( i < j )
            lines( c( d$logpop[i],d$logpop[j] ) ,
       c( d$total_tools[i],d$total_tools[j] ) ,
       lwd=2 , col=col.alpha("black",Rho[i,j]^2) )

# Non-centered Gaussian Process
#
m14.8nc <- ulam(
    alist(
        T ~ dpois(lambda),
        lambda <- (a * P^b / g) * exp(k[society]),
        # non-centered Gaussian Process prior
        transpars > vector[10]:k <<- L_SIGMA * z,
        vector[10]:z ~ normal(0, 1),
        transpars > matrix[10, 10]:L_SIGMA <<- cholesky_decompose(SIGMA),
        transpars > matrix[10, 10]:SIGMA <- cov_GPL2(Dmat, etasq, rhosq, 0.01),
        c(a, b, g) ~ dexp(1),
        etasq ~ dexp(2),
        rhosq ~ dexp(0.5)
    ),
    data = dat_list, chains = 4, cores = 4, iter = 2000
)
stancode(m14.8nc)

path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "8a_nc.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$print(max_rows=200)

# Diagnostics
fit$sampler_diagnostics()
fit$diagnostic_summary()

# Posterior draws
praws <- fit$draws(format = "data.frame")
praws

# Phylogenetic distance
#
library(rethinking)
data(Primates301)
data(Primates301_nex)

# Visualize phylogeny 
library(ape)
plot( ladderize(Primates301_nex), type = "fan", cex = .5)

# Load the data
d <- Primates301
d$name <- as.character(d$name)
dstan <- d[complete.cases( d$group_size, d$body, d$brain),]
spp_obs <- dstan$name

# Create the data list
dat_list <- list(
    N_spp = nrow(dstan),
    M = standardize( log(dstan$body) ),
    B = standardize( log(dstan$brain) ),
    G = standardize( log(dstan$group_size) ),
    Imat = diag( nrow(dstan) )
)

m14.9 <- ulam(
    alist(
        B ~ multi_normal(mu, K),
        mu <- a + bM * M + bG * G,
        matrix[N_spp, N_spp]:K <- Imat * sigma_sq,
        a ~ normal(0, 1),
        c(bM, bG) ~ normal(0, 0.5),
        sigma_sq ~ exponential(1)
    ),
    data = dat_list, chains = 4, cores = 4
)
precis( m14.9 )

# Stancode
stancode(m14.9)

# Stan Version
#
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "9.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$print(max_rows=200)

# Build the distance matrix 
#
library(ape)
tree_trimmed <- keep.tip( Primates301_nex, spp_obs )
Rbm <- corBrownian( phy=tree_trimmed )
V <- vcv(Rbm)
Dmat <- cophenetic( tree_trimmed )
plot( Dmat, V, xlab="pyhlogenetic distance", ylab = "covariance")
image(V)
image(Dmat)

# Put species in right order
dat_list$V <- V[spp_obs, spp_obs]
# Convert to correlation matrix
dat_list$R <- dat_list$V / max(V)

m14.10 <- ulam(
    alist(
        B ~ multi_normal(mu, K),
        mu <- a + bM * M + bG * G,
        matrix[N_spp, N_spp]:K <- R * sigma_sq,
        a ~ normal(0, 1),
        c(bM, bG) ~ normal(0, 0.5),
        sigma_sq ~ exponential(1)
    ),
    data = dat_list, chains = 4, cores = 4
)
precis( m14.10 )

# Extract Stancode
stancode(m14.10)

# Stan version
#
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "10.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$print(max_rows=200)

# Ornstein-Uhlbeck Kernel (L1 norm)

# Scaled and reordered distance matrix
dat_list$Dmat <- Dmat[ spp_obs, spp_obs ] / max(Dmat)
m14.11 <- ulam(
    alist(
        B ~ multi_normal(mu, K),
        mu <- a + bM * M + bG * G,
        matrix[N_spp, N_spp]:K <- cov_GPL1(Dmat, etasq, rhosq, 0.01),
        a ~ normal(0, 1),
        c(bM, bG) ~ normal(0, 0.5),
        etasq ~ half_normal(1, 0.25),
        rhosq ~ half_normal(3, 0.25)
    ),
    data = dat_list, chains = 4, cores = 4
)
precis(m14.11)

# Stan version
#
path <- "~/projects/rethinking2nd"
file <- file.path(path, "stan", "14", "11.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
fit$print(max_rows=200)

# Plot the posteriror covariance function
#
post <- fit$draws(format = "data.frame")
plot(NULL, xlim = c(0, max(dat_list$Dmat)), ylim=c(0,1.5))
with(post,
    for( i in 1:30 ) {
        # Posterior
        curve( etasq[i]*exp(-rhosq[i]*x), add = TRUE, col = "steelblue")
    }
)
# Prior
eta <- abs( rnorm(1e3, 1, 0.25) ) #half-normal
rho <- abs( rnorm(1e3, 3, 0.25) ) #half-normal
d_seq <- seq(0, 5, length.out = 1e3)
K <- sapply(d_seq, function(x) eta*exp(-rho*x))
# Mean
lines( d_seq, colMeans(K), lwd = 2)
shade( apply(K, 2, PI), d_seq)
