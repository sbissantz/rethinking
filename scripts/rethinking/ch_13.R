######################
# Models With Memory #
######################

library( rethinking )
data( reedfrogs )
d <- reedfrogs 

# Make a taank cluster varibale
d$tank <- 1:nrow( d ) 

# data list
dat <- list(
            S = d$surv,
            N = d$density,
            tank = d$tank
)

# formula 
fml_13.1 <- alist(
    S ~ binomial( N, p ),
    logit( p ) <- a[tank],
    a[tank] ~ normal( 0, 1.5 )
)

# model
m13.1 <- ulam(
                 fml_13.1, data=dat, chains=4, cores=4, log_lik=TRUE
)

precis( mdl_13.1, depth=2 )

#
# MLM
#

f13.2 <- alist(
               S ~ binomial( N, p ),
               logit( p ) <- a[tank],
               a[tank] ~ normal( a_bar, sigma ),
               a_bar ~ normal( 0, 1.5 ),
               sigma ~ exponential( 1 )

)
m13.2 <- ulam( f13.2, data=dat, chains=4, cores=4, log_lik=TRUE )

precis( m13.2, depth=2 )

#
# Model comparison
compare( mdl_13.1, m13.2 )

# extract Stan samples
post <- extract.samples( m13.2 )
# mean probabilty to survive in each tank
(d$propsurv.est <- logistic(  colMeans(post$a) ))

# Raw proportions of surviving in each tank
plot( d$propsurv, ylim=c(0,1) , pch=16, xaxt="n", xlab="tank", 
     ylab="proportion survival", col=rangi2)
axis( 1, at=c(1,16,32,48), labels=c(1,16,32,48) )

# overlay posteiror means
points( d$propsurv.est )

# mark posterior mean probabilty across tanks
abline( h=mean( inv_logit(post$a_bar )), lty=2 )
abline( h=mean( d$propsurv ), lty=1, col=rangi2 )

# vertical dividers bwteen tank debsutues
abline( v=16.5, lwd=0.5 )
abline( v=32.5, lwd=0.5 )
text( 8, 0, "small tanks")
text( 8+16, 0, "medium tanls")
text( 8+32, 0, "large tanks")

# Show first 100 pupoulatoisn in the posterior
plot( NULL , xlim = c(-5,5), ylim=c(0,0.5), xlab="log-odds survive", ylab="Density")
for (i in 1:100) {
    curve( dnorm( x, post$a_bar[i], post$sigma[i] ), add = TRUE,
    col = col.alpha("black", 0.2))
}

# sample 8000 imaginary tanks from the posterior distribution
sim_tanks <- rnorm( 8000, post$a_bar, post$sigma )
# transform to probabilty and visualize
dens( inv_logit( sim_tanks ), lwd=2, adj=0.1, xlab="Pr to survive" )

#
# Simulating fake data
#

# 2. Assign values to the p's
a_bar <- 1.4                           # pond with individual log-odds of  
sigma <- 1.5                           # survival 
nponds <- 60
Ni <- as.integer( rep( c(5,10,25,35) , each=15 ))
set.seed( 5005 )
a_pond <- rnorm( nponds, mean=a_bar, sd=sigma )
dsim <- data.frame( ponds=1:nponds, Ni=Ni, true_a=a_pond )
# Survuvir count
dsim$Si <- rbinom( nponds, prob=logistic(dsim$true_a), size=dsim$Ni )
# empirical proportion of survivor in each pond
dsim$p_nopool <- dsim$Si /dsim$Ni

dat <- list( Si=dsim$Si , Ni=dsim$Ni , pond=dsim$pond )
m13.3 <- ulam(
              alist(
                    Si ~ dbinom( Ni , p ),
                    logit(p) <- a_pond[pond],
                    a_pond[pond] ~ dnorm( a_bar , sigma ),
                    a_bar ~ dnorm( 0 , 1.5 ),
                    sigma ~ dexp( 1 )
                    ), data=dat , chains=4 )

precis( m13.3, depth=2 )

post <- extract.samples( m13.3 )
dsim$p_partpool <- colMeans( inv_logit(post$a_pond) )
dsim$p_true <- inv_logit( dsim$true_a )
nopool_error <- abs( dsim$p_nopool - dsim$p_true )
partpool_error <- abs( dsim$p_partpool - dsim$p_true )

plot( 1:60, nopool_error, xlab="pond", ylab="absolute error", col=rangi2, pch=16 )
points( 1:60, partpool_error )

#
# Prosocial chimpanzees
#
library( rethinking )
data( chimpanzees )
d <- chimpanzees
d$tx <- 1 + d$prosoc_left + 2*d$condition

dat <- list(
            pulled_left = d$pulled_left,
            actor = d$actor,
            block_id = d$block,
            tx = as.integer( d$tx )
)
set.seed( 13 )
m13.4 <- ulam( 
              alist(
                    pulled_left ~ binomial( 1, p ),
                    logit( p ) <- a[ actor ] + g[ block_id ] + b[ tx ],
                    b[ tx ] ~ normal( 0, 0.5 ),
                    # adadptive priors
                    a[ actor ] ~ normal( a_bar, sigma_a ),
                    g[ block_id ] ~ normal( 0, sigma_g ),
                    # hyper-priors
                    a_bar ~ normal( 0, 1.5 ),
                    sigma_a ~ exponential( 1 ),
                    sigma_g ~ exponential( 1 )
              ), data=dat, chains=4, cores=4, log_lik=TRUE
)
precis( m13.4, depth=2 )
plot( precis(m13.4, depth=2) )

f13.5 <- alist(
               pulled_left ~ binomial( 1, p ),
               logit( p ) <- a[ actor ] + b[ tx ],
               # Priors
               b[ tx ] ~ normal( 0, 0.5 ),
               a[ actor ] ~ normal( a_bar, sigma ),
               # Hyperpriors
               a_bar ~ normal( 0, 1.5 ),
               sigma ~ exponential( 1 )
)

m13.5 <- ulam( f13.5, data=dat, chains=4, cores=4, log_lik = TRUE )

compare( m13.4, m13.5 )

f13.6 <- alist(
               # Likelihood
               pulled_left ~ binomial( 1, p ),
               # Conditional expectation 
               logit( p ) <- a[ actor ] + g[ block_id ] + b[ tx ],
               # Adaptive priors
               a[ actor ] ~ normal( a_bar, sigma_a ),
               g[ block_id ] ~ normal( 0, sigma_g ),
               b[ tx ] ~ normal( 0, sigma_b ),
               # Hyperpriors
               a_bar ~ normal( 0, 1.5 ),
               sigma_a ~ exponential( 1 ),
               sigma_g ~ exponential( 1 ),
               sigma_b ~ exponential( 1 )
)

m13.6 <- ulam( f13.6, data=dat, chains=4, cores=4, log_lik=TRUE )

coeftab( m13.4, m13.6 )

#
# Divergent transitions
#
library( rethinking )
# The deveils funnel
m13.7 <- ulam(
              alist(
                    v ~ normal( 0, 3 ),
                    x ~ normal( 0, exp(v) )
              ), data=list(N=1), chains=4,cores=4
)
precis( m13.7 )

m13.7nc <- ulam(
              alist(
                    z ~ normal( 0,1 ),
                    v ~ normal( 0,3 ),
                    gq> real[1]:x <<- z * exp(v)
              ), data=list(N=1), chains=4, cores=4)
precis( m13.7nc )
post <- extract.samples( m13.7nc )
# VIsualize the funnel
plot( post$x, post$v )

set.seed(13)
m13.4b <- ulam( m13.4, chains=4, cores=4, control=list(adapt_delta=.99) )
divergent( m13.4 ) ; divergent( m13.4b )
precis( m13.4 )

# Non-center the model
set.seed(13)
m13.4nc <- ulam(
                alist(
                      pulled_left ~ dbinom( 1 , p ) ,
                      logit(p) <- a_bar + z[actor]*sigma_a + # actor intercepts
                        x[block_id]*sigma_g +
                          # block intercepts
                          b[tx] ,
                        b[tx] ~ dnorm( 0 , 0.5 ),
                        z[actor] ~ dnorm( 0 , 1 ),
                        x[block_id] ~ dnorm( 0 , 1 ),
                        a_bar ~ dnorm( 0 , 1.5 ),
                        sigma_a ~ dexp(1),
                        sigma_g ~ dexp(1),
                        gq> vector[actor]:a <<- a_bar + z*sigma_a,
                        gq> vector[block_id]:g <<- x*sigma_g
                        ) , data=dat , chains=4 , cores=4 )

precis(m13.4)
precis(m13.4nc)

precis_c <- precis(m13.4, depth=2)
precis_nc <- precis(m13.4nc, depth=2)
pars <- c( paste("a[",1:7,"]",sep="") , paste("g[",1:6,"]",sep="") ,
          paste("b[",1:4,"]",sep="") , "a_bar" , "sigma_a" , "sigma_g" )
neff_table <- cbind( precis_c[pars, "n_eff"], precis_nc[pars, "n_eff"] )
plot( neff_table, xlim=range(neff_table), ylim=range(neff_table),
xlab= "n_eff (centered)", ylab="n_eff (non-centered)", lwd=2)
abline( a=0, b=1, lty=2 )

# Posterior predictions
#

# Lefty 
chimp <- 2
d_pred <- list(
               actor = rep(chimp, 4),
               tx = 1:4,
               block_id = rep( 1,4 )
)
p <- link( m13.4, data=d_pred )
p_mu <- apply( p, 2, mean )
p_ci <- apply( p, 2, PI )

post <- extract.samples( m13.4 )
str( post )
# Samples: rows, actors=cols
# density for actor 5
dens( post$a[,5] )


p_link <- function( tx, actor=1, block_id=1 ){
  logodds <- with(post,
                  a[,actor] + g[,block_id] + b[,tx]
  )
                  inv_logit(logodds)
}
p_raw <- sapply( 1:4, function(i) p_link( i, actor=2, block_id=1 ) )
p_mu <- apply( p_raw, 2, mean )
p_mu <- apply( p_raw, 2, PI )

p_link_abar <- function( treatment ) {
  logodds <- with( post, a_bar + b[,treatment] )
  inv_logit(logodds)
}
post <- extract.samples( m13.4 )
p_raw <- sapply( 1:4, function(i) p_link_abar( i ) )
p_mu <- apply( p_raw, 2, mean )
p_ci <- apply( p_raw, 2, PI )
plot( NULL, xlab="TX", ylab="prop pulled left", ylim=c(0,1) , xaxt="n", xlim=c(1,4) )
axis( 1, at=1:4, labels=c("R/N", "L/N", "R/P", "L/P") )
lines( 1:4, p_mu )
shade( p_ci, 1:4 )

a_sim <- with( post, rnorm( length(post$a_bar), a_bar, sigma_a ) )
p_link_asim <- function( treatment ) {
  logodds <- with( post, a_sim + b[,treatment] )
  inv_logit( logodds )
}
p_raw_asim <- sapply( 1:4, function(i) p_link_asim( i ) )
plot( NULL, xlab="TX", ylab="prop pulled left", ylim=c(0,1) , xaxt="n", xlim=c(1,4) )
axis( 1, at=1:4, labels=c("R/N", "L/N", "R/P", "L/P") )
for( i in 1:1000 ) lines( 1:4, p_raw_asim[i,], col=grau(0.25), lwd=2 )

