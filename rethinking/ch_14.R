#
# Adventures in Covatiance
#

library( rethinking )

# Simulate a opl of cafes
# 
a <- 3.5                               # avg: morning wait time
b <- -1                                # avg: difference afternoon 
sigma_a <- 1                           # std: intercepts 
sigma_b <- 0.5                         # std: slopes 
rho <- -0.7                            # cor: intercepts & slopes
Mu <- c( a,b )                         # mean: alpha & beta 
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol=2 )
sigmas <- c(sigma_a, sigma_b)          # vec: std's 
Rho <- matrix(c(1, rho, rho, 1), ncol=2)
# Define Covmat, SRS
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
N_cafes <- 20

# Plot
#
library( MASS )
set.seed( 5 )
vary_effects <- mvrnorm( N_cafes, Mu, Sigma )
a_cafe <- vary_effects[, 1]            # varing intercepts
b_cafe <- vary_effects[, 2]            # varying slopes 
# 20 cafes from one statistical population
plot( a_cafe, b_cafe, col=rangi2, xlab="VI: alpha cafe", 
     ylab="VS: beta cafe" )
library(ellipse)
# Multivariate Gaussian population of intercepts and slopes
for ( l in c( .1, .3, .5, .8, .99 ) )
    lines( ellipse(Sigma, centre=Mu, level=l), col=col.alpha("black", 0.2) )

set.seed(22)
N_visits <- 10
afternoon <- rep(0:1, N_visits * N_cafes/2)
cafe_id <- rep( 1:N_cafes, each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon
sigma <- 0.5                           # std: within cafes 
wait <- rnorm( N_visits*N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id, afternoon=afternoon, wait=wait )

# Simulate data from the LKJcor prior
#
R <- rlkjcorr( 1e4, K=2, eta=2 )
dens( R[,1,2], xlab="correlation" )

# Fit a model to learn about the data generating process
#
set.seed( 876530 )
m14.1 <- ulam(
              alist(
                    wait ~ normal( mu, sigma ),
                    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
                    c(a_cafe, b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho, sigma_cafe ),
                    a ~ normal( 5,2 ),
                    b ~ normal( -1, 0.5 ),
                    sigma_cafe ~ exponential(1),
                    sigma ~ exponential(1),
                    Rho ~ lkj_corr(1)
              ), data=d, chains=4, cores=4
)
stancode(m14.1)

post <- extract.samples( m14.1 )
dens( post$Rho[,1,2], xlim=c(-1,1) )   # Posterior 
R <- rlkjcorr( 1e4, K=2, eta=2 )       # Prior 
dens( R[,1,2], add=TRUE, lty=2 )

# Compute FE's 
a1 <- sapply( 1:N_cafes ,
      function(i) mean(wait[cafe_id==i & afternoon==0]))
b1 <- sapply( 1:N_cafes ,
      function(i) mean(wait[cafe_id==i & afternoon==1])) - a1
# extract posterior means of partially pooled estimates
a2 <- apply( post$a_cafe, 2, mean )
b2 <- apply( post$b_cafe, 2, mean )

# Plot both and connect with lines
plot( a1, b1, xlab="intercept", ylab="slope",
     pch=16, col=rangi2, ylim=c( min(b1)-0.1, max(b1) + 0.1 ),
     xlim = c( min(a1)-0.1, max(a1)+0.1 ))
points( a2, b2, pch=1 )
lapply( 1:N_cafes, function(i)  lines( c(a1[i],a2[i]) , c(b1[i],b2[i])))
# compute posterior mean bivariate Gaussian
Mu_hat <- c( mean(post$a), mean(post$b) )
rho_hat <- mean( post$Rho[,1,2] )
sa_hat <- mean( post$sigma_cafe[,1] )
sb_hat <- mean( post$sigma_cafe[,2] )
cov_ab <- sa_hat*sb_hat*rho_hat
Sigma_hat <- matrix( c( sa_hat^2, cov_ab, cov_ab, sb_hat^2 ), ncol=2 )
# draw contours
library( ellipse )
for ( lvl in c( .1, .3, .5, .8, .99 ) )
      lines( ellipse( Sigma_hat, centre=Mu_hat, level=lvl ),
            col=col.alpha("black", 0.2) )

# Same plot on the outcome scale
#

# convert VE to waiting times
wait_am_1 <- (a1)
wait_pm_1 <- (a1+b1)
wait_am_2 <- (a2)
wait_pm_2 <- (a2+b2)

# plot average waiting times am & pm
plot( wait_am_1, wait_pm_1, xlab="morning wait",
     ylab="afternoon wait", pch=16, col=rangi2,
     ylim=c( min(wait_pm_1)-.1, max(wait_pm_1)+.1 ),
     xlim=c( min(wait_am_1)-.1, max(wait_am_1)+.1 ) )

points( wait_am_2, wait_pm_2, pch=1  )
for ( i in 1:N_cafes ) {
      lines( c(wait_am_1[i], wait_am_2[i]), c(wait_pm_1[i], wait_pm_2[i]) )
}
abline( a=0, b=1, lty=2 )
# shrinkage distribution by simulation  
v <- mvrnorm( 1e4, Mu_hat, Sigma_hat )
v[,2] <- v[,1] + v[,2]                 # calc: afternoon wait
Sigma_hat_2 <- cov(v)
Mu_hat_2 <- Mu_hat
Mu_hat_2[2] <- Mu_hat[1] + Mu_hat[2]

# draw contours
library(ellipse)
for ( lvl in c( .1, .3, .5, .8, .99 ) )
      lines( ellipse( Sigma_hat_2, centre=Mu_hat_2, level=lvl ),
            col=col.alpha("black", 0.2) )


#
# 14.2 Chimpanzees 
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

# Reparametrization
#

set.seed(4387510)
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
                    ) , data=dat , chains=4 , cores=4 , log_lik=TRUE )
stancode(m14.3)

# extract n_eff values for each model
neff_nc <- precis(m14.3,3,pars=c("alpha","beta"))$n_eff
neff_c <- precis(m14.2,3,pars=c("alpha","beta"))$n_eff
plot( neff_c , neff_nc , xlab="centered (default)" ,
     ylab="non-centered (cholesky)" , lwd=1.5 )
abline(a=0,b=1,lty=2)

# Instrumental variables
#

library(rethinking)
library(dagitty)
# Dag 1
dag <- dagitty(
               'dag{ 
               E <- U -> W ; E -> W
               E [e, pos="0,1"] 
               W [o, pos="1,1"] 
               U [u, pos="0.5, 0"]
}')
plot(dag)

dagQ <- dagitty(
               'dag{ 
               E <- U -> W ; E -> W ; Q -> E
               E [e, pos="0,1"] 
               W [o, pos="1,1"] 
               U [u, pos="0.5, 0"]
               Q [e, pos="-0.5, 0"]
}')
plot(dagQ)

set.seed(73)
N <- 500
U_sim <- rnorm( N )
Q_sim <- sample( 1:4 , size=N , replace=TRUE )
E_sim <- rnorm( N , U_sim + Q_sim )
W_sim <- rnorm( N , -U_sim + .6*E_sim )
dat_sim <- list(
                W=standardize(W_sim) ,
                E=standardize(E_sim) ,
                Q=standardize(Q_sim) )

m14.4 <- ulam(
              alist(
                    W ~ dnorm( mu, sigma ),
                    mu <- aW + bEW*E,
                    aW ~ dnorm( 0, 0.2 ),
                    bEW ~ dnorm( 0, 0.5 ),
                    sigma ~ dexp( 1 )
              ), data=dat_sim, chains=4, cores=4
)
precis(m14.4)

m14.5 <- ulam(
              alist(
                    W ~ dnorm( mu, sigma ),
                    mu <- aW + bEW*E + bQE*Q,
                    aW ~ dnorm( 0, 0.2 ),
                    bEW ~ dnorm( 0, 0.5 ),
                    bQE ~ dnorm( 0, 0.5 ),
                    sigma ~ dexp( 1 )
              ), data=dat_sim, chains=4, cores=4
)
precis(m14.5)

m14.6 <- ulam(
              alist(
                    c(W,E) ~ multi_normal( c(muW, muE), Rho, Sigma ),
                    muW <- aW + bEW*E,
                    muE <- aE + bQE*Q,
                    c(aW,aE) ~ normal( 0, 0.2 ),
                    c(bEW, bQE) ~ normal( 0, 0.5 ),
                    Rho ~ lkj_corr(2),
                    Sigma ~ exponential( 1 )
              ), data=dat_sim, chains=4, cores=4
)
precis( m14.6, depth=3 )

m14.4x <- ulam( m14.4, data=dat_sim, chains=4, cores=4 )
precis(m14.4x, depth=3)
m14.6x <- ulam( m14.6, data=dat_sim, chains=4, cores=4 )
precis(m14.6x, depth=3)

library(dagitty)
dagIV <- dagitty( "dag{ Q -> E <- U -> W <- E }" )
instrumentalVariables( dagIV , exposure="E" , outcome="W" )


# 
# Social relations as correlated varying effects
# 

data( KosterLeckie )
d <- kl_dyads 
kl_data <- list(
                N = nrow(kl_dyads),
                N_households = max(kl_dyads$hidB),
                did = kl_dyads$did,
                hidA = kl_dyads$hidA,
                hidB = kl_dyads$hidB,
                giftsAB = kl_dyads$giftsAB,
                giftsBA = kl_dyads$giftsBA
)
m14.7 <- ulam(
              alist(
                    giftsAB ~ poisson( lambdaAB ),
                    giftsBA ~ poisson( lambdaBA ),
                    log(lambdaAB) <- a + gr[hidA,1] + gr[hidB,2] + d[did,1], 
                    log(lambdaBA) <- a + gr[hidB,1] + gr[hidA,2] + d[did,2], 
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

precis(m14.7, depth=3, pars=c("Rho_gr", "sigma_gr"))

# Posterior predictive giving & receiving
post <- extract.samples( m14.7 )
g <- sapply( 1:25 , function(i) post$a + post$gr[,i,1] )
r <- sapply( 1:25 , function(i) post$a + post$gr[,i,2] )
Eg_mu <- apply( exp(g) , 2 , mean )
Er_mu <- apply( exp(r) , 2 , mean )

plot( NULL , xlim=c(0,8.6) , ylim=c(0,8.6) , xlab="generalized giving" ,
     ylab="generalized receiving" , lwd=1.5 )
abline(a=0,b=1,lty=2)
# ellipses
library(ellipse)
for ( i in 1:25 ) {
      Sigma <- cov( cbind( g[,i] , r[,i] ) )
      Mu <- c( mean(g[,i]) , mean(r[,i]) )
      for ( l in c(0.5) ) {
            el <- ellipse( Sigma , centre=Mu , level=l )
            lines( exp(el) , col=col.alpha("black",0.5) )
      }
}
# household means
points( Eg_mu , Er_mu , pch=21 , bg="white" , lwd=1.5 )

precis( m14.7 , depth=3 , pars=c("Rho_d","sigma_d") )

dy1 <- apply( post$d[,,1] , 2 , mean )
dy2 <- apply( post$d[,,2] , 2 , mean )
plot( dy1 , dy2 )

library( rethinking )
data( islandsDistMatrix )
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")
round(Dmat,1)

#linear
curve( exp(-1*x), from=0, to=4, lty=2 )
# squared
curve( exp(-1*x^2), from=0, to=4, lty=2, add=TRUE )

data(Kline2) # load the ordinary data, now with coordinates
d <- Kline2
d$society <- 1:10 # index observations
dat_list <- list(
                 T = d$total_tools,
                 P = d$population,
                 society = d$society,
                 Dmat=islandsDistMatrix )
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

precis(m14.8, depth=3)

post <- extract.samples(m14.8)
# plot the posterior median covariance function
plot( NULL , xlab="distance (thousand km)" , ylab="covariance" ,
     xlim=c(0,10) , ylim=c(0,2) )
# compute posterior mean covariance
x_seq <- seq( from=0 , to=10 , length.out=100 )
pmcov <- sapply( x_seq , function(x) post$etasq*exp(-post$rhosq*x^2) )
pmcov_mu <- apply( pmcov , 2 , mean )
lines( x_seq , pmcov_mu , lwd=2 )
# plot 50 functions sampled from posterior
for ( i in 1:50 )
      curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE ,
            col=col.alpha("black",0.3) )


# compute posterior median covariance among societies
K <- matrix(0,nrow=10,ncol=10)
for ( i in 1:10 )
      for ( j in 1:10 )
            K[i,j] <- median(post$etasq) *
                  exp( -median(post$rhosq) * islandsDistMatrix[i,j]^2 )
diag(K) <- median(post$etasq) + 0.01

# convert to correlation matrix
Rho <- round( cov2cor(K) , 2 )
# add row/col names for convenience
colnames(Rho) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
rownames(Rho) <- colnames(Rho)
Rho

# scale point size to logpop
psize <- d$logpop / max(d$logpop)
psize <- exp(psize*1.5)-2

plot( d$lon2, d$lat, xlab="longitude", ylab="latitude",
     col=rangi2, cex=psize, pch=16, xlim=c(-50,30))
labels <- as.character(d$culture)
text( d$lon2, d$lat, labels=labels, cex=.7, pos=c(2,4,3,3,4,1,3,2,4,2) )

# overlay
for( i in 1:10 ) 
      for( j in 1:10 )
            if( i < j )
                  lines( c( d$lon2[i], d$lon2[j] ), c( d$lat[i], d$lat[j] ) ,
                        lwd=2, col=col.alpha("black", Rho[i,j]^2) )



# compute posterior median relationship, ignoring distance
logpop.seq <- seq( from=6 , to=14 , length.out=30 )
lambda <- sapply( logpop.seq , function(lp) exp( post$a + post$b*lp ) )
lambda.median <- apply( lambda , 2 , median )
lambda.PI80 <- apply( lambda , 2 , PI , prob=0.8 )
# plot raw data and labels
plot( d$logpop , d$total_tools , col=rangi2 , cex=psize , pch=16 ,
     xlab="log population" , ylab="total tools" )
text( d$logpop , d$total_tools , labels=labels , cex=0.7 ,
     pos=c(4,3,4,2,2,1,4,4,4,2) )
# display posterior
lines( logpop.seq ,
      lines( logpop.seq ,
            lines( logpop.seq ,
                  predictions
                  lambda.median , lty=2 )
            lambda.PI80[1,] , lty=2 )
      lambda.PI80[2,] , lty=2 )
# overlay correlations
for( i in 1:10 )
      for ( j in 1:10 )
            if ( i < j )
                  lines( c( d$logpop[i],d$logpop[j] ) ,
                        c( d$total_tools[i],d$total_tools[j] ) ,
                        lwd=2 , col=col.alpha("black",Rho[i,j]^2) )


library( rethinking )
data( Primates301 )
data( Primates301_nex )
d <- Primates301
dstan <- d[ complete.cases( d$group_size, d$body, d$brain ), ]
spp_obs <- dstan$name
dat <- list( 
            N_spp = nrow(dstan),
            M = standardize( log(dstan$body) ),
            B = standardize( log(dstan$brain) ),
            G = standardize( log(dstan$group_size) ),
            Imat = diag( nrow(dstan) )
)
m14.9 <- ulam(
              alist(
                    B ~ multi_normal( mu , SIGMA ),
                    mu <- a + bM*M + bG*G,
                    matrix[N_spp,N_spp]: SIGMA <- Imat * sigma_sq,
                    a ~ normal( 0,1 ),
                    c(bM, bG) ~ normal( 0, 0.5 ),
                    sigma_sq ~ exponential( 1 )
              ), data=dat, chains=4, cores=4
)
precis( m14.9 )
library(ape)
tree_trimmed <- keep.tip( Primates301_nex, spp_obs )
Rbm <- corBrownian( phy=tree_trimmed )
V <- vcv(Rbm)
Dmat <- cophenetic( tree_trimmed )
plot( Dmat , V , xlab="phylogenetic distance" , ylab="covariance" )



