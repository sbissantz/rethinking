#
#
# The many variables & the spurious waffles
#
#

library(rethinking) ; data(WaffleDivorce) ; d <- WaffleDivorce

#
# 5.1 Spurious associations
#

# Define: two causal models
library( dagitty )
DMA_dag1 <- dagitty('dag{ D <- A -> M -> D }')
DMA_dag2 <- dagitty('dag{ D <- A -> M }')

par(mfrow=c(2,1))
dags <- list( DAG1=DMA_dag1, DAG2=DMA_dag2 )
# Visualize: two causal models
lapply( dags, plot )
# Get the testable implications
lapply( dags, impliedConditionalIndependencies )

# Standardize variables
d$D <- standardize( d$Divorce) 
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage ) 

# Get a sense of a Sd
varls <- with(d, list( D=Divorce, M=Marriage, A=MedianAgeMarriage ))
lapply( varls, sd )
# $D
# [1] 1.820814
# 
# $M
# [1] 3.797905
# 
# $A
# [1] 1.24363
# 1.24363 change in median age at marriage ~ 1sd change in divorce rate.

# Fit a binary regression D <- M
f1 <- alist( 
            D ~ dnorm( mu, sigma ),
            mu <- a + bM * M,
                a ~ dnorm( 0, 0.2 ),
                bM ~ dnorm( 0, 0.5 ),
            sigma ~ dexp( 1 )
)
m1 <- quap( f1, data=d )

# Prior predictive simulation
# prior <- extract.prior( m1 )
# x_seq <- c(-2,2)
# mu <- link( m1, post=prior, data=list(M=x_seq))
# mu.mean <- apply( mu, 2, mean )
# mu.PI <- apply( mu, 2, PI )
# plot( NULL, xlim=x_seq, ylim=x_seq, xlab="M", ylab="D")
# for( i in 1:50 ) lines( x_seq, mu[i,] )
# 

# Fit a binary regression D <- A
f2 <- alist( 
            D ~ dnorm( mu, sigma ),
            mu <- a + bA * A,
                a ~ dnorm( 0, 0.2 ),
                bA ~ dnorm( 0, 0.5 ),
            sigma ~ dexp( 1 )
)
m2 <- quap( f2, data=d )

# Fit a multiple regression D <- M + A
f3 <- alist( 
            D ~ dnorm( mu, sigma ),
            mu <- a + bM * M + bA * A,
                a ~ dnorm( 0, 0.2 ),
                bM ~ dnorm( 0, 0.5 ),
                bA ~ dnorm( 0, 0.5 ),
            sigma ~ dexp( 1 )
)
m3 <- quap( f3, data=d )

# Visudalize the result
plot( coeftab(m1, m2, m3), par=c("bA", "bM") )
# Assuming the DAG: true -- it Seems that the effect of M on D is spurious

#
# Siulatie the divorce example
#

N <- 50 # number of simulated States
age <- rnorm( N )                      # sim A
mar <- rnorm( N , -age )               # sim A -> M
div <- rnorm( N , age )                # sim A -> D


#
# Inferential plots
#

# Predictor residual plots

m5.4 <- quap(
             alist(
                   M ~ dnorm( mu , sigma ) ,
                   mu <- a + bAM * A ,
                   a ~ dnorm( 0 , 0.2 ) ,
                   bAM ~ dnorm( 0 , 0.5 ) ,
                   sigma ~ dexp( 1 )
                   ) , data = d )
mu <- link(m5.4)
mu_mean <- apply( mu , 2 , mean )
mu_resid <- d$M - mu_mean

# Posterior predictive plot

d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )

m <- quap( alist( 
                 D ~ dnorm( mu, sigma ),
                 mu <- a + bM * M + bA * A,
                 a ~ dnorm( 0, 0.2 ),
                 bM ~ dnorm( 0, 0.5 ),
                 bA ~ dnorm( 0, 0.5 ),
                 sigma ~ dexp( 1 )), 
          data=d)

# link without data to use original one
mu <- link( m )
# Summarize samples across cases
mu.mean <- apply( mu, 2, mean )
mu.PI <- apply( mu, 2, PI )

# Simulate observations: use original ones
D_sim <- sim( m, n=1e4)
D_PI <- apply( D_sim, 2, PI )

plot( mu.mean ~ d$D , col=rangi2 , ylim=range(mu.PI) ,
xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) ) lines( rep(d$D[i],2) , mu.PI[,i] , col=rangi2 )

text(x=d$D, y=mu.mean, labels=abbreviate(d$Location, minlength = 5), 
     pos=4, col="black")
# identify( x=d$D, y=mu_mean, labels=d$Loc)


#
# Counterfactual plots
#

# Standardize the variables
d$D <- standardize( d$Divorce) 
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage ) 

# Fit the model: A -> D & M -> D plus additionaly, A -> M (necessary to predict
# the consequences of manimulating A since some of the effect of A acts thrugh
# M)
m5.3_A <- quap(
               alist(
                     ## A -> D <- M
                     D ~ dnorm( mu , sigma ) ,
                     mu <- a + bM*M + bA*A ,
                     a ~ dnorm( 0 , 0.2 ) ,
                     bM ~ dnorm( 0 , 0.5 ) ,
                     bA ~ dnorm( 0 , 0.5 ) ,
                     sigma ~ dexp( 1 ),
                     ## A -> M
                     M ~ dnorm( mu_M , sigma_M ),
                     mu_M <- aM + bAM*A,
                     aM ~ dnorm( 0 , 0.2 ),
                     bAM ~ dnorm( 0 , 0.5 ),
                     sigma_M ~ dexp( 1 )
                     ) , data = d )

# Simmulating the effect of A on D 

# A list of 30 imaginary interventions
A_seq <- seq( from=-2 , to=2 , length.out=30 )
# prep data
sim_dat <- data.frame( A=A_seq )
# simulate M and then D, using A_seq
s <- sim( m5.3_A , data=sim_dat , vars=c("M","D") )

plot( sim_dat$A , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
     xlab="manipulated A" , ylab="counterfactual D" )
shade( apply(s$D,2,PI) , sim_dat$A )
mtext( "Total counterfactual effect of A on D" )

plot( sim_dat$A , colMeans(s$M) , ylim=c(-2,2) , type="l" ,
     xlab="manipulated M" , ylab="counterfactual D" )
shade( apply(s$D,2,PI) , sim_dat$A )
mtext( "Counterfactual effect of A on M" )

# Simulate the counterfactual effect of manipulating M

par(mfrow=c(1,2))

library( dagitty )
dag <- dagitty( 'dag{ D <- A ; M -> D}' ) ; plot( dag )
# The arrow A -> M is deleted, because if we control the values of M, then A no
# longer influ- ences it. It’s like a perfectly controlled experiment. 

sim_dat <- data.frame( M=seq(from=-2,to=2,length.out=30) , A=0 )
s <- sim( m5.3_A , data=sim_dat , vars="D" )
plot( sim_dat$M , colMeans(s) , ylim=c(-2,2) , type="l" ,
     xlab="manipulated M" , ylab="counterfactual D" )
shade( apply(s,2,PI) , sim_dat$M )
mtext( "Total counterfactual effect of M on D" )

#
# Masked relationships
#

library(rethinking) ; data(milk) ; d <- milk

# Think DAGs!
library(dagitty)
dag <- dagitty('dag {
    N [pos="0,0"] ; U [pos="1,0"] ; M [pos="2,0"] ; K [pos="1,1"]
    N <- U -> M
    N -> K
    M -> K
}')
drawdag( dag )

# Standardize!
d$K <- standardize( d$kcal.per.g )
d$N <- standardize( d$neocortex.perc )
# Log of mass -- sclaing MMT ~ magnitude of others
d$M <- standardize( log(d$mass) )
# Complete case analysis
dcc <- d[ complete.cases(d$K,d$N,d$M), ]
#
# Bivariate model K <- N
#
# Formalize & fit
m5.5 <- quap(
             alist(
                   K ~ dnorm( mu , sigma ) ,
                   mu <- a + bN*N ,
                   a ~ dnorm( 0 , 0.2 ) ,
                   bN ~ dnorm( 0 , 0.5 ) ,
                   sigma ~ dexp( 1 )
                   ) , 
             data=dcc )
#
# Prior predictive simulation
# lines stay within the high probability region of the observed data
#
prior <- extract.prior( m5.5)
xseq <- c(-2,2)
mu <- link( m5.5 , post=prior , data=list(N=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )
#
# Summarize
#
precis( m5.5 )
# Visualize: translate the Golems landuage in something more understandable 
xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.5 , data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc )
    lines( xseq , mu_mean , lwd=2 )
    shade( mu_PI , xseq )
#
# Bivariate model K <- B
#
# Formalize & fit
m5.6 <- quap(
             alist(
                   K ~ dnorm( mu , sigma ) ,
                   mu <- a + bM*M ,
                   a ~ dnorm( 0 , 0.2 ) ,
                   bM ~ dnorm( 0 , 0.5 ) ,
                   sigma ~ dexp( 1 )
                   ) , 
             data=dcc )
#
# Prior predictive simulation
# lines stay within the high probability region of the observed data
#
prior <- extract.prior( m5.6 )
xseq <- c( -2, 2 )
mu <- link( m5.6 , post=prior , data=list(M=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )
#
# Summarize
#
precis( m5.6 )
# Visualize: translate the Golems landuage in something more understandable 
xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.6 , data=list(M=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc )
    lines( xseq , mu_mean , lwd=2 )
    shade( mu_PI , xseq )

#
# Multivariate model K <- N + M
#
pairs( ~K + M + N, dcc )
# Formalize & fit
m5.7 <- quap(
             alist(
                   K ~ dnorm( mu , sigma ) ,
                   mu <- a + bN*N + bM*M ,
                   a ~ dnorm( 0 , 0.2 ) ,
                   bN ~ dnorm( 0 , 0.5 ) ,
                   bM ~ dnorm( 0 , 0.5 ) ,
                   sigma ~ dexp( 1 )
                   ) , 
             data=dcc )
#
# Prior predictive simulation
# lines stay within the high probability region of the observed data
#
prior <- extract.prior( m5.7 )
xseq <- c( -2, 2 )
mu <- link( m5.7 , post=prior , data=list(N=xseq, M=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )
#
# Summarize
#
plot( coeftab( m5.5, m5.6, m5.7 ), pars=c("bM", "bN") )

#
# Counterfactuals (imagined worlds)
#
# Imagine manipulating M and N, breaking the influence of U on each. 
# In the real world, such experiments are impossible. If we change an animal’s
# body size, natural selection would then change the other features to match it.
# But these counterfactual plots do help us see how the model views the
# association between each predictor and the outcome. 
dag <- dagitty('dag {
    N [pos="0,0"]
    M [pos="2,0"]
    K [pos="1,1"]
    N -> K
    M -> K
}')
drawdag( dag )

# Imagine: changine MASS
xseq <- seq( from=min(dcc$M)-.2 , to=max(dcc$M)+.2 , length.out=30 )
mu <- link( m5.7 , data=list( M=xseq , N=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$M) , ylim=range(dcc$K), 
     xlab="Body Mass (std)", ylab="Kcal Milk (std)")
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

# Imagine: changing Neocortex
xseq <- seq( from=min(dcc$N)-0.5 , to=max(dcc$N)+0.5 , length.out=30 )
mu <- link( m5.7 , data=data.frame( M=xseq , N=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$N) , ylim=range(dcc$K),
     xlab="Body Mass (std)", ylab="Kcal Milk (std)")
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

#
# Index variables
#

#
# 2 cstegories
#

data(Howell1) ; d <- Howell1

d$sex <- ifelse( d$male==1, 2, 1 )

# formalize the model
f5.8 <- alist( 
              height ~ dnorm( mu, sigma ),
              mu <- a[sex],
              a[sex] <- dnorm( 178, 20 ),
              sigma ~ dunif( 0, 50 )
)
m5.8 <- quap( f5.8, data=d )

precis( m5.8, depth=2 )


post <- extract.samples( m5.8 )
post$diff <- post$a[,1] - post$a[,2]
precis( post, depth = 2 )

#
# Many categrical variables 
#

data( milk ) ; d <- milk
levels( d$clade )

d$clade_id <- as.integer( d$clade )
d$K <- standardize( d$kcal.per.g )
set.seed( 63 )
d$house <- sample( rep(1:4, each=8), size=nrow(d) )

m5.10 <- quap(
              alist(
                    K ~ dnorm( mu , sigma ),
                    mu <- a[clade_id] + h[house],
                    a[clade_id] ~ dnorm( 0 , 0.5 ),
                    h[house] ~ dnorm( 0 , 0.5 ),
                    sigma ~ dexp( 1 )
                    ) , 
              data=d )

precis( m5.10, depth=2 )

labels <- paste( "a[" , 1:4 , "]:" , levels(d$clade) , sep="" )
plot( precis( m5.9 , depth=2 , pars="a" ) , labels=labels ,
     xlab="expected kcal (std)" )

labels <- paste( "h[" , 1:4 , "]:" , levels(d$house) , sep="" )

