#
# Lecture 16: Gaussian processes
# 
library(rethinking)

# Kline data
data(Kline2)
d <- Kline2
data(islandsDistMatrix)
# display (measured in thousands of km)
Dmat <- islandsDistMatrix
colnames(Dmat) <- abbreviate(rownames(Dmat), 2)
round(Dmat,1)

# Prior predictive simulation 
n <- 50 
etasq <- rexp(n,2) ; rhosq <- rexp(n,1)
plot( NULL , xlim=c(0,7) , ylim=c(0,2) , xlab="distance (thousand km)" , ylab="covariance" )
for ( i in 1:n ) {
    curve(etasq[i] * exp(-rhosq[i] * x^2), add = TRUE, lwd = 4, 
    col = "steelblue")
}

# Data list
dat_list <- list(
    N = nrow(d),
    T = d$total_tools,
    P = d$population,
    S = 1:10,
    D = islandsDistMatrix )

mTdist <- ulam(
    alist(
        T ~ dpois(lambda),
        log(lambda) <- abar + a[S],
        vector[10]:a ~ multi_normal( 0 , K ),
        transpars> matrix[10,10]:K <- cov_GPL2(D,etasq,rhosq,0.01),
        abar ~ normal(3,0.5),
        etasq ~ dexp( 2 ),
        rhosq ~ dexp( 0.5 )
    ), data=dat_list , chains=4 , cores=4 , iter=4000 , log_lik=TRUE )

precis(mTdist)
stancode(mTdist)

#
# Stancode
#
# Centered parametrization
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "1_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)
# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences, non-centered not needed!
fit$diagnostic_summary()

# Note test if non-centered works better!
#mTdist_nc <- ulam(
    #alist(
        #T ~ dpois(lambda),
        #log(lambda) <- abar + a[S],
        #transpars> vector[10]: a <<- L_K * z,
        #vector[10]: z ~ normal( 0 , 1 ),
        #transpars> matrix[10,10]: L_K <<- cholesky_decompose( K ),
        #transpars> matrix[10,10]: K <- cov_GPL2(D,etasq,rhosq,0.01),
        #abar ~ normal(3,0.5),
        #etasq ~ dexp( 2 ),
        #rhosq ~ dexp( 0.5 )
    #), data=dat_list , chains=4 , cores=4 , iter=2000 , log_lik=TRUE )

#stancode(mTdist_nc)

# Non-Centered parametrization (worse than centered)
# path <- "~/projects/rethinking22"
# file <- file.path(path, "stan", "16", "1_nct.stan")
# mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
# fit <- mdl$sample(data=dat_list)
# fit$print(max_rows=200)

# Diagnostics
#
# fit$sampler_diagnostics()
# fit$cmdstan_diagnose() # Divergences, centered-parametrization better!
# fit$diagnostic_summary()

post <- fit$draws(format="data.frame")
# For later
post0 <- fit$draws(format="data.frame")

# Prior-Posterior Comparison
#
n <- 50 
etasq <- rexp(n,2) ; rhosq <- rexp(n,1)
plot( NULL , xlim=c(0,7) , ylim=c(0,2) , xlab="distance (thousand km)" , ylab="covariance" )
for ( i in 1:n ) {
    curve(etasq[i] * exp(-rhosq[i] * x^2), add = TRUE, lwd = 4, 
    col = "steelblue")
    curve(post$eta_sq[i] * exp(-post$rho_sq[i] * x^2), add = TRUE, lwd = 4, 
    col = "black")
}

K <- rethinking::extract.samples(mTdist)$K

# scale point size to logpop
psize <- d$logpop / max(d$logpop)
psize <- exp(psize*1.5)-1

# plot raw data and labels
plot( d$lon2 , d$lat , xlab="longitude" , ylab="latitude" ,
    col=2 , cex=psize , pch=16 , xlim=c(-50,30) , ylim=c(-25,25) )
# overlay lines shaded by cov
Kmean <- apply(K,2:3,mean)
Kmean <- Kmean / max(Kmean)
for( i in 1:10 )
    for ( j in 1:10 )
        if ( i < j )
            lines( c( d$lon2[i],d$lon2[j] ) , c( d$lat[i],d$lat[j] ) ,
                lwd=4 , col=col.alpha("black",Kmean[i,j]) )

points( d$lon2 , d$lat , col=2 , cex=psize , pch=16 )
labels <- as.character(d$culture)
text( d$lon2 , d$lat , labels=labels , cex=0.7 , pos=c(2,4,3,3,4,1,3,2,4,2) )

# Full Model
#
dat_list <- list(
    N = nrowd(d),
    T = d$total_tools,
    P = d$population,
    S = 1:10,
    D = islandsDistMatrix )

mTDP <- ulam(
    alist(
        T ~ dpois(lambda),
        lambda <- (abar*P^b/g)*exp(a[S]),
        vector[10]:a ~ multi_normal( 0 , K ),
        transpars> matrix[10,10]:K <- cov_GPL2(D,etasq,rhosq,0.01),
        c(abar,b,g) ~ dexp( 1 ),
        etasq ~ dexp( 2 ),
        rhosq ~ dexp( 0.5 )
    ), data=dat_list , chains=4 , cores=4 , iter=4000 , log_lik=TRUE )

stancode(mTDP)
precis( mTDP , depth=2 )

#
# Stancode
#
# Centered parametrization
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "2_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)
# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() 
fit$diagnostic_summary()

# Non-centered parametrization
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "2_nct.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)
# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # Divergences, non-centered needed!
fit$diagnostic_summary()

# Posterior
#
post <- fit$draws(format = "data.frame")

plot( NULL , xlim=c(0,7) , ylim=c(0,2) , xlab="distance (thousand km)" , ylab="covariance" )
for ( i in 1:n ) {
    curve(post0$eta_sq[i] * exp(-post0$rho_sq[i] * x^2), add = TRUE, lwd = 4, 
    col = "steelblue")
    curve(post$eta_sq[i] * exp(-post$rho_sq[i] * x^2), add = TRUE, lwd = 4, 
    col = "black")
}
# plot relationship with population
post <- extract.samples(mTDP)

plot( log(dat_list$P) , dat_list$T , xlab="log population" , ylab="tools" , col=2 , cex=psize , pch=16 )

xseq <- seq( from=6 , to=14 , len=100 )
l <- sapply( xseq , function(x) with(post, (abar * exp(x)^b/g) ) )
lines_w( xseq , apply(l,2,mean) , lwd=4 , col=4 )
shade( apply(l,2,PI) , xseq , col=col.alpha(4,0.25) )

# overlay lines shaded by cov
K <- post$K
Kmean <- apply(K,2:3,mean)
Kmean <- Kmean / max(Kmean)
x <- log(dat_list$P)
y <- dat_list$T
for( i in 1:10 )
    for ( j in 1:10 )
        if ( i < j )
            lines( c( x[i],x[j] ) , c( y[i],y[j] ) ,
                lwd=4 , col=col.alpha("black",Kmean[i,j]) )
points( x , y , col=2 , cex=psize , pch=16 )

#
# Phylogenetic regression
#
library(rethinking)
data(Primates301)
data(Primates301_nex)
d <- Primates301

# complete case analysis
cc <- complete.cases(d$group_size,d$body,d$brain)
dstan <- d[cc,]
# For the multivariate representation of the model
identity_matrix <- diag(nrow(dstan))

# DAG
#
library(dagitty)
dag <- dagitty( 'dag {
B [outcome,pos="1,-0.5"]
U [latent,pos="0,0.5"]
G [exposure,pos="-1,-0.5"]
M [selected,pos="0,0"]
history [pos="0.5,0.5"]
U -> B
U -> G
U -> M
G -> B
M -> B
M -> G
history -> U }')
plot(dag)

# Data list
#
dat_list <- list(
    N_spp = nrow(dstan),
    M = standardize(log(dstan$body)),
    B = standardize(log(dstan$brain)),
    G = standardize(log(dstan$group_size)),
    Imat = identity_matrix )

# classical regression form
# Note normal_ID GLM (faster)
mBMG0 <- ulam(
    alist(
        B ~ normal( mu , sigma ),
        mu <- a + bM*M + bG*G,
        a ~ normal( 0 , 1 ),
        c(bM,bG) ~ normal( 0 , 0.5 ),
        sigma ~ exponential( 1 )
    ), data=dat_list , chains=4 , cores=4 )

stancode(mBMG0)

#
# Stancode
#
# Centered parametrization
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "3a_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# normal_id_glm() is faster
dat_ls <- list(
    N_spp = nrow(dstan),
    B = standardize(log(dstan$brain)),
    X = cbind( standardize(log(dstan$body)), 
    standardize(log(dstan$group_size))))

path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "3b_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences!
fit$diagnostic_summary()

#
# Stancode
#
# Centered parametrization
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "3a_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# normal_id_glm() is faster
dat_ls <- list(
    N_spp = nrow(dstan),
    B = standardize(log(dstan$brain)),
    X = cbind( standardize(log(dstan$body)), 
    standardize(log(dstan$group_size))))

path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "3b_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences!
fit$diagnostic_summary()

# Multivariate form
# Note: Mega slow...
mBMG <- ulam(
    alist(
        B ~ multi_normal( mu , K ),
        mu <- a + bM*M + bG*G,
        matrix[N_spp,N_spp]: K <- Imat*(sigma^2),
        a ~ normal( 0 , 1 ),
        c(bM,bG) ~ normal( 0 , 0.5 ),
        sigma ~ exponential( 1 )
    ), data=dat_list , chains=4 , cores=4 )

stancode(mBMG)

# Stan version
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "4_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Import: Exactly the same approximation!
#

fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences!
fit$diagnostic_summary()

# GP Version

library(ape)
d <- Primates301
d$name <- as.character(d$name)
dstan <- d[ complete.cases( d$group_size , d$body , d$brain ) , ]
spp_obs <- dstan$name
tree_trimmed <- keep.tip( Primates301_nex, spp_obs )
Rbm <- corBrownian( phy=tree_trimmed )
V <- vcv(Rbm)
Dmat <- cophenetic( tree_trimmed )

# Brownian Motion (linear decline)
plot( Dmat , V , xlab="phylogenetic distance" , ylab="covariance" , lwd=3 , col=2 , type="l" )
curve( max(V)*exp(-(1/28)*x) , add=TRUE , lwd=4 , col=4 )

# put species in right order
dat_list$V <- V[ spp_obs , spp_obs ]
# convert to correlation matrix
dat_list$R <- dat_list$V / max(V)
as.matrix(dat_list$R)
# Brownian motion model
mBMG_brownian <- ulam(
    alist(
        B ~ multi_normal( mu , K ),
        mu <- a + bM*M + bG*G,
        matrix[N_spp,N_spp]: K <- R * sigma_sq,
        a ~ normal( 0 , 1 ),
        c(bM,bG) ~ normal( 0 , 0.5 ),
        sigma_sq ~ exponential( 1 )
    ), data=dat_list , chains=4 , cores=4 )

stancode( mBMG_brownian )

# Stan version
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "5_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences, non-centered not needed!
fit$diagnostic_summary()

# Add scaled distance matrix
dat_list$Dmat <- Dmat[ spp_obs , spp_obs ] / max(Dmat)
dat_list$Dmat

# Ornstein-Uhlenbeck (L1 gaussian process)
mB_OU <- ulam(
    alist(
        B ~ multi_normal( mu , K ),
        mu <- a + 0*M,
        matrix[N_spp,N_spp]:K <- cov_GPL1(Dmat,etasq,rho,0.01),
        a ~ normal(0,1),
        etasq ~ half_normal(1,0.25),
        rho ~ half_normal(3,0.25)
    ), data=dat_list , chains=4 , cores=4 )

stancode(mB_OU)

# Stan version
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "6_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences, non-centered not needed!
fit$diagnostic_summary()

# Extract posterior draws
post0 <- fit$draws(format = "matrix")

mBM_OU <- ulam(
    alist(
        B ~ multi_normal( mu , K ),
        mu <- a + bM*M,
        matrix[N_spp,N_spp]:K <- cov_GPL1(Dmat,etasq,rho,0.01),
        a ~ normal(0,1),
        bM ~ normal(0,0.5),
        etasq ~ half_normal(1,0.25),
        rho ~ half_normal(3,0.25)
    ), data=dat_list , chains=4 , cores=4 )

stancode( mBM_OU )

# Stan version
#
path <- "~/projects/rethinking22"
file <- file.path(path, "stan", "16", "7_cnt.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_list)
# Note that the alpha parameters are on the log scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose() # No divergences, non-centered not needed!
fit$diagnostic_summary()

# Extract posterior draws
post1 <- fit$draws(format = "matrix")
n <- 30
eta <- abs(rnorm(n,1,0.25))
rho <- abs(rnorm(n,3,0.25))

# Visualize results
#
plot( NULL , xlim=c(0,max(dat_list$Dmat)) , ylim=c(0,1.5) ,
    xlab="phylogenetic distance" , ylab="covariance" )


# posterior
for ( i in 1:n ) {
    curve( eta[i]*exp(-rho[i]*x) , add=TRUE , col=grau(0.4) , lwd=2 )
    curve( post0[i, "eta_sq"]*exp(-post0[i, "rho"]*x) , add=TRUE , col=4 , lwd=2 )   
    curve( post1[i, "eta_sq"]*exp(-post1[i, "rho"]*x) , add=TRUE , col=5 , lwd=2 )
}

dens( post0[,"bG"] , lwd=4 , col=1 , xlab="effect of group size" , xlim=c(-2,2) )
dens( post1[,"bG"] , lwd=4 , col=2 , add=TRUE )
abline(v=0,lty=3)
