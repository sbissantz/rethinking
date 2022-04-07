#
#
# The Haunted DAG & the causal terror
#
#

#
# Multicolli legs
#

# DAG
library( dagitty )
dag <- dagitty( "dag { leg_left -> height <- right_leg }" )
plot( dag )

# Fake data
N <- 100                               # number of individuals
set.seed(909) 
height <- rnorm(N, mean=10, sd=2)    # sim total height of each
leg_prop <- runif(N, min=0.4, max=0.5) # leg as proportion of height
leg_left <- leg_prop*height +          # sim left leg as proportion + error
    rnorm( N , mean=0 , sd=0.02 )                  
leg_right <- leg_prop*height +         # sim right leg as proportion + error 
    rnorm( N , mean=0 , sd=0.02 )   
d <- data.frame(height,leg_left,leg_right) # combine into data frame

# height <- left_leg + right_leg

library( rethinking )
m6.1 <- quap( alist( 
                    height~ dnorm( mu, sigma ),
                    mu <- a + bl*leg_left + br*leg_right,
                    a ~ dnorm( 10, 100 ),
                    br ~ dnorm( 2, 10  ),
                    bl ~ dnorm( 2, 10 ),
                    sigma ~ dexp( 1 )
                    ), data=d)
# Table output
precis( m6.1 )
# Plot the actual output
plot( precis( m6.1 ) )

# Extract samples from the posterior
post <- extract.samples( m6.1 )
# Plot the association between bl & br
plot( bl ~ br, data=post, col=col.alpha(rangi2,0.1), pch=16 )
# Estimate the sum of both 
sum_blbr <- post$bl + post$br
# Plot the density for the sum
dens( sum_blbr, col=rangi2, lwd=2, xlab="sum of bl and br" )

#
# Multicollinear milk
#

# Get the data 
data( milk )
# Save typing 
d <- milk
# Look in the data
pairs( ~ kcal.per.g + perc.fat + perc.lactose , data=d , col=rangi2 )

# Standardize the variables
d$K <- standardize( d$kcal.per.g )
d$F <- standardize( d$perc.fat )
d$L <- standardize( d$perc.lactose )

# Think DAG's before you regress!
library( dagitty )
dag <- dagitty( "dag{ perc.fat -> kcal.per.g }" )
plot( dag )
# Formalize & fit
m6.3 <- quap( alist( 
                    K ~ dnorm( mu, sigma ),
                    mu <- a + bF*F,
                    a ~ dnorm( 0, 0.2 ),
                    bF ~ dnorm( 0, 0.5 ),
                    sigma ~ dexp( 1 )
                    ), data=d )
# Plot to fully understand
plot(precis( m6.3 ))

# Think DAG's before you regress!
dag <- dagitty( "dag{ perc.lactose -> kcal.per.g }" )
plot( dag )
# Formalize & fit
m6.4 <- quap( alist( 
                    K ~ dnorm( mu, sigma ),
                    mu <- a + bL*L,
                    a ~ dnorm( 0, 0.2 ),
                    bL ~ dnorm( 0, 0.5 ),
                    sigma ~ dexp( 1 )
                    ), data=d )
# Plot to fully understand
plot(precis( m6.4 ))

# Think DAGs 
dag <- dagitty( "dag{ perc.fat -> kcal.per.g <- per.lactose }" )
plot( dag )

m6.5 <- quap( alist( 
                    K ~ dnorm( mu, sigma ),
                    mu <- a + bF*F + bL*L,
                    a ~ dnorm( 0, 0.2 ),
                    bF ~ dnorm( 0, 0.5 ),
                    bL ~ dnorm( 0, 0.5 ),
                    sigma <- dexp( 1 )

                    ), data=d )
# Table the output
precis( m6.5 )
# Plot to understand
plot( precis( m6.5 ) )

#  What likely goes on here...
dag <- dagitty( "dag{
               L <- D -> F
               L -> K <- F
}" )
plot( dag )

#
# Simulating colli
#

library(rethinking)
data(milk)
d <- milk
sim.coll <- function( r=0.9 ) {
    d$x <- rnorm( nrow(d) , mean=r*d$perc.fat ,
                 sd=sqrt( (1-r^2)*var(d$perc.fat) ) )
    m <- lm( kcal.per.g ~ perc.fat + x , data=d )
    sqrt( diag( vcov(m) ) )[2] # stddev of parameter
}
rep.sim.coll <- function( r=0.9 , n=100 ) {
    stddev <- replicate( n , sim.coll(r) )
    mean(stddev)
}
r.seq <- seq(from=0,to=0.99,by=0.01)
stddev <- sapply( r.seq , function(z) rep.sim.coll(r=z,n=100) )
plot( stddev ~ r.seq , type="l" , col=rangi2, lwd=2 , xlab="correlation" )

#
# Post-treatment bias
#

# Fake data 
set.seed(71)

# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N, mean=10, sd=2)

# assign treatments and simulate fungus and growth
# First 50 get Tx
# Prob to get fungus: (a) wihout tx 50/50, (b) with tx 1/10
# Prev height = init height - 3 * 1 given fungus  else 0*initial height
treatment <- rep( 0:1 , each=N/2 ) # the first 50 get tx
fungus_prob <-  0.5 - treatment*0.4 # Without tx: 50/50 with 0.1
fungus <- rbinom( N , size=1 , prob=fungus_prob ) # Prob to get fungus
h1 <- h0 + rnorm(N, mean=5-3*fungus) 

# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )
precis(d)

# Formalize the model
# h_t0 < h_t1
# p > 0

sim_p <- rlnorm( 1e4, meanlog=0, sdlog=0.25 )
precis( data.frame(sim_p) )

# Think DAGs
library(dagitty)
dag <- dagitty( "dag {
                     H_0 -> H_1 <- F <- T
}")
coordinates( dag ) <- list( x=c(H_0=0, H_1=1, F=1.5, T=2) ,
                                 y=c(H_0=0,T=0,F=0,H_1=0) )
plot( dag )

impliedConditionalIndependencies( dag )

# Formalize and fit the model
m6.6 <- quap(
             alist(
                   h1 ~ dnorm( mu , sigma ),
                   mu <- h0*p,
                   p ~ dlnorm( 0 , 0.25 ),
                   sigma ~ dexp( 1 )
                   ), data=d )
precis(m6.6)

m6.7 <- quap( 
             alist( 
                   h1 ~ dnorm( mu, sigma ),
                    mu <- h0*p,
                        p <- a + bt*treatment + bf*fungus,
                            a ~ dlnorm( 0, 0.2 ),
                            bt ~ dnorm( 0, 0.5 ),
                            bf ~ dnorm( 0, 0.5 ),
                   sigma <- dexp( 1 )
             ), data=d
)
precis( m6.7 )

m6.8 <- quap( 
             alist( 
                   h1 ~ dnorm( mu, sigma ),
                    mu <- h0*p,
                        p <- a + bt*treatment,
                            a ~ dlnorm( 0, 0.2 ),
                            bt ~ dnorm( 0, 0.5 ),
                   sigma <- dexp( 1 )
             ), data=d
)
precis( m6.8 )

#
# Post-treatment Bias 
#

library( dagitty )
dag <- dagitty( 'dag{ 
               H0 -> H1 <- M -> F <- T
}' )
coordinates( dag ) <- list( x = c( H0=0, H1=1, M=2, F=3, T=4 ),
                            y = c( H0=1, H1=1, M=0, F=1, T=1 ))
plot( dag )

# Simulate fake data
set.seed(71)
N <- 1000

h0 <- rnorm(N,10,2)
treatment <- rep( 0:1 , each=N/2 )
M <- runif(N, min=0, max=1)
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 + 0.4*M )
h1 <- h0 + rnorm( N , 5 + 3*M )

d2 <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )

m6.9 <- quap( 
             alist( 
                   h1 ~ dnorm( mu, sigma ),
                    mu <- h0*p,
                        p <- a + bt*treatment + bf*fungus,
                            a ~ dlnorm( 0, 0.2 ),
                            bt ~ dnorm( 0, 0.5 ),
                            bf ~ dnorm( 0, 0.5 ),
                   sigma <- dexp( 1 )
             ), data=d2
)
m6.10 <- quap( 
             alist( 
                   h1 ~ dnorm( mu, sigma ),
                    mu <- h0*p,
                        p <- a + bt*treatment,
                            a ~ dlnorm( 0, 0.2 ),
                            bt ~ dnorm( 0, 0.5 ),
                   sigma <- dexp( 1 )
             ), data=d
)

par( mfrow=c(1,2) )
plot( precis(m6.9), pars=c("bt", "bf") )
plot( precis(m6.10), pars=c("bt"))

#
# 6.3 Collider Bias
#

# Simulate the agent base data
library( rethinking )
d <- sim_happiness( seed=1997, N_years=1000 )
precis( d )

# Think dags
library( dagitty )
dag <- dagitty("dag{ H -> M <- A }") 
coordinates( dag ) <- list( y=c(H=0,M=0,A=0), x=c(H=0, M=1, A=2) )
plot( dag )

# Seperarte units
d2 <- d[ d$age>17 , ] # only adults
# rescale age: 0=18, 1=65
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )
# mid index: since 0:1 -+1-> 1:2
d2$mid <- d2$married + 1

# Remeber: M ~ A = 0
m6.9 <- quap(
             alist(
                   happiness ~ dnorm( mu , sigma ),
                   mu <- a[mid] + bA*A,
                   a[mid] ~ dnorm( 0, 1 ),
                   bA ~ dnorm( 0, 2 ),
                   sigma ~ dexp( 1 )
             ), data=d2)

precis( m6.9, depth=2 )
plot(precis( m6.9, depth=2 ))

# Haunted DAG

# fake data
N <- 200 # Number of granparent-parent-child triads
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect og G on C (!)
b_PC <- 1 # direct effect of P on C
b_U <- 2 # direct effect of U on P and C

set.seed(1)
U <- 2*rbern( N , 0.5 ) - 1
G <- rnorm( N )
P <- rnorm( N , mean=b_GP*G + b_U*U )
C <- rnorm( N , mean=b_PC*P + b_GC*G + b_U*U )
d <- data.frame( C=C , P=P , G=G , U=U )

# Think DAG 

dag <- dagitty( "dag{ 
               G -> P <- U -> C
               G -> C
               P -> C
}" )
coordinates( dag ) <- list( x=c(G=0, P=0.5, C=0.5, U=0.75), y=c(G=0, P=0, C=1, U = .5) )
plot(dag)

# Formalize & Fit

m6.12 <- quap(
              alist(
                    C ~ dnorm( mu , sigma ),
                    mu <- a + b_PC*P + b_GC*G + b_U*U,
                    a ~ dnorm( 0 , 1 ),
                    c(b_PC,b_GC,b_U) ~ dnorm( 0 , 1 ),
                    sigma ~ dexp( 1 )
                    ), data=d )
precis(m6.12)
plot(precis(m6.12))

#
# Backdoor criterion 
#

# Model 1

library(dagitty)
dag_6.1 <- dagitty( "dag {
                   U [unobserved]
                   X -> Y
                   X <- U <- A -> C -> Y
                   U -> B <- C
}")
# Textable implications
impliedConditionalIndependencies( dag_6.1 )
# Conditional independencies
adjustmentSets( dag_6.1 , exposure="X" , outcome="Y" )


library(dagitty)
dag_6.2 <- dagitty( "dag {
                   A -> D
                   A -> M -> D
                   A <- S -> M
                   S -> W -> D
}")
# Textable implications
impliedConditionalIndependencies( dag_6.2 )
# Conditional independencies
adjustmentSets( dag_6.2 , exposure="W" , outcome="D" )










