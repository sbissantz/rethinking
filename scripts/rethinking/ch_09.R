#
#
# Markov Chain Monte Carlo
#
#

num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
for ( i in 1:num_weeks ) {
## record current position
positions[i] <- current
## flip coin to generate proposal
proposal <- current + sample( c(-1,1) , size=1 )
## now make sure he loops aroundthe archipelago
if ( proposal < 1 ) proposal <- 10
if ( proposal > 10 ) proposal <- 1
## move?
prob_move <- proposal/current
current <- ifelse( runif(1) < prob_move , proposal , current )
print( paste(i, "->", current) )
}

plot(1:100, positions[1:100])
plot( table(positions) )

library(rethinking)

# Show the radial distance from the mean
plot( NULL, xlim=c(0,50), ylim=c(0,1) )
fun <- function(dim) {
D <- dim 
T <- 1e3
Y <- rmvnorm(T,rep(0,D),diag(D))
rad_dist <- function( Y ) sqrt( sum(Y^2) )
Rd <- sapply( 1:T , function(i) rad_dist( Y[i,] ) )
dens( Rd, add = TRUE )
}
lapply( c(1, 10, 100, 500), fun )

#
# 9.4 
#

library( rethinking )
data( rugged ) 
d <- rugged

# Preprocess the variables
d$log_gdp <- log( d$rgdppc_2000)
dd <- d[ complete.cases( d$rgdppc_2000 ), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1, 1, 2)

# Slim the data frame
dat_slim <- list(
                 log_gdp_std = dd$log_gdp_std,
                 rugged_std = dd$rugged_std,
                 cid = as.integer( dd$cid )
)
str(dat_slim)

m9.1 <- ulam(
             alist(
                   log_gdp_std ~ dnorm( mu, sigma ),
                   mu <- a[cid] + b[cid] * (rugged_std - 0.215),
                   a[cid] ~ dnorm( 1, 0.1 ),
                   b[cid] ~ dnorm( 0, 0.3 ),
                   sigma ~ dexp( 1 )
             ), data=dat_slim, chains=4, cores=4, iter=400 )

# Get model formula and duration
show( m9.1 )

# Summarize the model output
precis( m9.1, depth=2 )

# Plot the samples
pairs( m9.1 )

# Diagnose the chains
traceplot( m9.1, chain=2 )
trankplot( m9.1 )

# See stancode
stancode(m9.1)

#
# 1 - 4 rule: 1: debug 4_ i 
#

m9.1_dbg <- ulam(
                   alist(
                         log_gdp_std ~ dnorm( mu, sigma ),
                         mu <- a[cid] + b[cid] * (rugged_std - 0.215),
                         a[cid] ~ dnorm( 1, 0.1 ),
                         b[cid] ~ dnorm( 0, 0.3 ),
                         sigma ~ dexp( 1 )
                         ), data=dat_slim, chains=1 )


traceplot( m9.1_dbg )

m9.1 <- ulam(
             alist(
                   log_gdp_std ~ dnorm( mu, sigma ),
                   mu <- a[cid] + b[cid] * (rugged_std - 0.215),
                   a[cid] ~ dnorm( 1, 0.1 ),
                   b[cid] ~ dnorm( 0, 0.3 ),
                   sigma ~ dexp( 1 )
                   ), data=dat_slim, chains=4 )

# 9.3 -- Taming a wild chain
#
library( rethinking )
y <- c(-1, 1)
set.seed(11)
m9.2 <- ulam( 
             alist( 
                   y ~ dnorm( mu, sigma ),
                   mu <- alpha,
                   alpha ~ dnorm( 0, 1000 ),
                   sigma ~ dexp( 0.0001 )
             ), data=list(y=y), chains=3)

show( m9.2 )
precis( m9.2 )

# STAN - Hamilton MC diagnositcs 
pairs( m9.2@stanfit )

# Disgnose
traceplot( m9.2 )
trankplot( m9.2 )

# Informative priors
#

m9.3 <- ulam( 
             alist(
                    y ~ dnorm( mu, sigma ),
                    mu <- alpha,
                        alpha ~ dnorm( 1, 10 ),
                    sigma ~ dexp( 1 )
                    ), data=list(y=y), chains=3 )


precis( m9.3 )
# NULL
pairs( m9.3@stanfit )
# NULL

#
# 9.5.4 Nin identifiable paramters
#

# Siml 100 obs from a Gaussian
#

y <- rnorm( 100, mean=0, sd=1 )

m9.4 <- ulam( 
             alist( 
                   y ~ dnorm( mu, sigma ),
                   mu <- a1 + a2,
                        a1 ~ dnorm( 0, 1000 ),
                        a2 ~ dnorm( 0, 1000 ),
                   sigma ~ dexp( 1 )
             ), data=list(y=y), chains =3
)

precis( m9.4 )
traceplot( m9.4 )

m9.5 <- ulam(
             alist( 
                   y ~ dnorm( mu, sigma ),
                   mu <- a1 + a2,
                        a1 ~ dnorm( 0, 10 ),
                        a2 ~ dnorm( 0, 10 ),
                  sigma ~ dexp( 1 )
             ), data=list(y=y), chains=3
)
precis( m9.5 )






