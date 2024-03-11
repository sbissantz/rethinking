#
#
# Counterfactual plots (inferential plot)
#
#

#
# Waffles example
#

library(rethinking) ; data(WaffleDivorce) ; d <- WaffleDivorce

par( mfrow=c(1,3) )

# Draw a DAG
library( dagitty )
dag <- dagitty( 'dag{ D <- A -> M -> D}' ) ; plot( dag )

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

#
# Simmulating the effect of A on D 
#

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

#
# Simulate the counterfactual effect of manipulating M
#

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
