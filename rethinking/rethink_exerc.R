
library(rethinking)

#
# Homework week 1
#

# 1. 

# 8 Water in 15 tosses, same flat prior
p_grid <- seq( 0,1,length.out=1000  )
prior <- rep( 1,1000 )
likelihood <- dbinom( 8, size=15, prob=p_grid )
ust_posterior <- prior * likelihood
posterior <- ust_posterior / sum(ust_posterior)
set.seed(100)
samples <- sample( p_grid, prob=posterior, size=1e4, replace=TRUE)

# 2.

# Prior: 0 if p < 0.5, & c if p > 0.5
p_grid <- seq( 0,1,length.out=1000  )
prior <- c( rep( 0, 500 ), rep( 1, 500) ) 
likelihood <- dbinom( 8, size=15, prob=p_grid )
ust_posterior <- prior * likelihood
posterior <- ust_posterior / sum(ust_posterior)
samples2 <- sample( p_grid, prob=posterior, size=1e4, replace=TRUE)

dens(samples, xlab="Probability of Water", xlim=c(0,1), ylim=c(0,6)) 
dens(samples2, xlab="Probability of Water", add=TRUE)
abline(v=.7) 

# 3.

s <- 1
d <- vector(length = 100000)
repeat{
    p_true <- .7
    W <-rbinom(1, size=s, prob=p_true)
    p_grid <- seq( 0,1,length.out=1000  )
    prior <- c( rep( 0, 500 ), rep( 1, 500) ) 
    likelihood <- dbinom( W, size=s, prob=p_grid )
    ust_posterior <- prior * likelihood
    posterior <- ust_posterior / sum(ust_posterior)
    samples <- sample( p_grid, prob=posterior, size=1e4, replace=TRUE)
    d[s] <- diff(PI(samples, prob=.99))
    cat("Number of tosses:", s, "Difference:", d[s], "\n")
    if( d[s] < 0.05 ){
       break
    }else{
        s <- s+1
    }
}
dif <- d[seq(1,2124)]
dif_len <- length(dif)
plot(dif ~ seq(dif_len), ylab="d(CI_up, CI_low)", 
     xlab="Number of tosses")
mtext("As the n° tosses increases, the CI width decreases")

#
# Homework week 2
#

data(Howell1) ; d <- Howell1 ; d2 <- d[ d$age >= 18, ]

# 1.

# Formalize & fit the model
xbar <- mean(d2$weight)
m1 <- quap(
           alist(
                 height ~ dnorm( mu , sigma ) ,
                    mu <- a + b*( weight - xbar ) ,
                        a ~ dnorm( 178 , 20 ) ,
                        b ~ dlnorm( 0 , 1 ) ,
                    sigma ~ dunif( 0 , 50 )
                 ) , 
           data=d2 )
# Produce a sequence of weights to predict
pred_dta <- data.frame(weight=c( 45, 40, 65, 31, 53 ))
h_sim <- sim( m1, data=pred_dta )
h_E <- apply( h_sim, 2, mean )
h_CI <- apply( h_sim, 2, PI, prob=.89 )
cbind(pred_dta, h_E, h_CI[1,], h_CI[2,])

# 2.

# Plot the model
plot(height ~ weight, data=d)
plot(log(height) ~ log(weight), data=d)

# Prepare the variables
d$log_weight <- log( d$weight )
xbar <- mean( d$log_weight )
# Formalize the model
f <- alist(
          height ~ dnorm( mu, sigma ),
            mu <- a + b * ( log_weight - xbar ),
                a ~ dnorm( 178, 20 ),
                b ~ dlnorm( 0, 1 ),
            sigma ~ dunif( 0, 50 )
)
m <- quap( f, data=d )
x_seq <- log( 1:60 )
h_sim <- sim( m, data=list( log_weight=x_seq ) )
h_E <- apply( h_sim, 2, mean )
h_PI <- apply( h_sim, 2, PI, prob=.99 )
plot( height ~ weight, data=d, col=col.alpha(rangi2,.5) )
lines( exp(x_seq), h_E )
shade( h_PI, exp(x_seq) )

# 3.

d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
alist(
      height ~ dnorm( mu , sigma ) ,
        mu <- a + b1*weight_s + b2*weight_s2 ,
            a ~ dnorm( 178 , 20 ) ,
            b1 ~ dlnorm( 2 , 1 ) ,
            b2 ~ dlnorm( 1 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
) , data=d )

set.seed( 45 )
prior <- extract.prior( m4.5 )

w_seq <- seq( from=min( d$weight_s ) , to=max( d$weight_s ) ,
                  length.out=50 )
w2_seq <- w_seq^2
pred_dta <- list( weight_s=w_seq, weight_s2=w2_seq )
mu <- link( m4.5, post=prior, data=pred_dta)

plot( NULL, xlim=range(w_seq), ylim=c(55,272), 
     xlab="weight (sd)", ylab="height")
for( i in 1:50 ) lines( w_seq, mu[i,], col=col.alpha("black", .5) )


#
#
# Homework week 4
#
# 

#
# 1
#

# Think Dags
library( dagitty )
dag <- dagitty("dag{
               A -> F
               F -> W <- G
               F -> G

} ")
coordinates( dag ) <- list( x=c(A=0.5, F=0, G=1, W=0.5), 
                           y=c(A=0, F=0.5, G=0.5, W=1) )
plot( dag )
#  {}

# Data
library( rethinking )
data(foxes) ; d <- foxes

# Standardize
d$W <- standardize( d$weight )
d$A <- standardize( d$area )

# Adjustments 
adjustmentSets( dag, exposure = "A", outcome = "W" )


# formalize and fit
m <- quap( alist(
                 W ~ dnorm( mu, sigma ),
                 mu <- a + bA * A,
                    a ~ dnorm( 0, 0.1 ),
                    bA ~ dnorm( 0, 0.5 ),
                 sigma ~ dexp( 1 )
                 ), data=d )

# Prior predictive simulation

prior <- extract.prior( m )
lim <- c(-2,2)
mu <- link( m, post=prior, data=list(A=lim ) )
plot( NULL, xlim=lim, ylim=lim, xlab="Area", ylab="Weight" )
for( i in 1:50 ) lines( lim, mu[i,], col=col.alpha("black", .4) )

# Summarize the output
precis( m )
plot( precis(m) )

#
# 2
#

# Standardize

d$F <- standardize( d$avgfood )

# Adjustments
adjustmentSets( dag, exposure = "F", outcome = "W" )

# Formalize & fit
m <- quap( 
          alist(
                W ~ dnorm( mu, sigma ),
                mu <- a + bF * F,
                a ~ dnorm( 0, 0.1 ),
                bF ~ dnorm( 0, 0.5 ),
                sigma ~ dexp( 1 )
                ), data=d )

# Prior predictive simulatoin
prior <- extract.prior( m )
lim <- c( -2,2 )
mu <- link( m, post=prior, data=list(F=lim) )
plot( NULL, xlim=lim, ylim=lim, xlab="avgfood", ylab="weight" ) 
for( i in 1:50 ) lines( lim, mu[i,], col=col.alpha("black", 0.4) )

# Summarize 

precis( m )
plot( precis(m) )

#
# 3
#

# Prepare the variablesj
d$G <- standardize( d$groupsize )

# Adjustments
adjustmentSets( dag, exposure = "G", outcome = "W" )

# Formalize & fit

m <- quap( alist(
                 W ~ dnorm( mu, sigma ),
                 mu <- bF*F + bG*G ,
                    a ~ dnorm( 0, 0.2 ),
                    c(bF, bG) ~ dnorm( 0, 0.4 ),
                 sigma ~ dexp( 1 )
                 ), data=d )

prior <- extract.prior( m )
lim <- c(-2, 2)
mu <- link( m, post=prior, data=list(F=lim, G=lim))
plot( NULL, xlim=lim, ylim=lim, xlab="groupsize", ylab="weight" )
for( i in 1:50 ) lines( c(-2,2), mu[i,], col=col.alpha("black", .4) )

precis( m )
plot(precis( m ))


#
#
# Homework week 5
#
#

#
# 1 
#

entropy <- function(p) {
    -sum(p * log(p))
}

i1 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
i2 <- c(0.8, 0.1, 0.05, 0.025, 0.025)
i3 <- c(0.05, 0.15, 0.7, 0.05,0.05)

IB <- list( i1, i2, i3 )
sapply( IB, entropy )
# [1] 1.6094379 0.7430039 0.9836003

kl_divergence <- function( p, q ) {
    sum( p * (log(p) - log(q) ))
}

(Dm <- matrix( NA, ncol=3, nrow=3 ))
#for( i in 1:3 ) for( j in 1:3 )

for (i in seq(3)) for (j in seq(3)) Dm[i,j] <- kl_divergence( IB[[j]], IB[[i]] )

dimnames( Dm ) <- list( c("i1", "i2", "i3"), c("true_d1", "true_d2", "true_d3") ) 
Dm
#      true_d1  true_d2   true_d3
# i1 0.0000000 0.866434 0.6258376
# i2 0.9704061 0.000000 1.8388452
# i3 0.6387604 2.010914 0.0000000

#
# 2 
#

library( rethinking )
d <- sim_happiness( seed=1997, N_years=1000 )

# Seperarte units
d2 <- d[ d$age>17 , ] # only adults
# rescale age: 0=18, 1=65
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )
# mid index: since 0:1 -+1-> 1:2
d2$mid <- d2$married + 1

m6.9 <- quap(
             alist(
                   happiness ~ dnorm( mu , sigma ),
                   mu <- a[mid] + bA*A,
                   a[mid] ~ dnorm( 0, 1 ),
                   bA ~ dnorm( 0, 2 ),
                   sigma ~ dexp( 1 )
             ), data=d2)

m6.10 <- quap(
             alist(
                   happiness ~ dnorm( mu , sigma ),
                   mu <- bA*A,
                   bA ~ dnorm( 0, 2 ),
                   sigma ~ dexp( 1 )
             ), data=d2)

compare( m6.9, m6.10 )
#           WAIC       SE    dWAIC      dSE    pWAIC     weight
# m6.9  2717.470 37.41938   0.0000       NA 3.716116 1.0000e+00
# m6.10 3100.112 27.70419 382.6413 34.71661 1.431047 8.1378e-84


#
#
#

# Standardize

d$F <- standardize( d$avgfood )

# Adjustments
dagitty::adjustmentSets( dag, exposure = "F", outcome = "W" )

# Formalize & fit
m1 <- quap( 
          alist(
                W ~ dnorm( mu, sigma ),
                mu <- a + bF * F,
                a ~ dnorm( 0, 0.1 ),
                bF ~ dnorm( 0, 0.5 ),
                sigma ~ dexp( 1 )
                ), data=d )


m1 <- quap( 
          alist(
                W ~ dnorm( mu, sigma ),
                mu <- a + bF * F,
                a ~ dnorm( 0, 0.1 ),
                bF ~ dnorm( 0, 0.5 ),
                sigma ~ dexp( 1 )
                ), data=d )


# Prior predictive simulatoin
prior <- extract.prior( m )
lim <- c( -2,2 )
mu <- link( m, post=prior, data=list(F=lim) )
plot( NULL, xlim=lim, ylim=lim, xlab="avgfood", ylab="weight" ) 
for( i in 1:50 ) lines( lim, mu[i,], col=col.alpha("black", 0.4) )

# Summarize 

precis( m )
plot( precis(m) )

#
# 3
#

dag <- dagitty::dagitty("dag{
               A -> F
               F -> W <- G
               F -> G

} ")
dagitty::coordinates( dag ) <- list( x=c(A=0.5, F=0, G=1, W=0.5), 
                           y=c(A=0, F=0.5, G=0.5, W=1) )
plot( dag )
#  {}

# Data
data(foxes) ; d <- foxes


# Prepare the variablesj
d$G <- standardize( d$groupsize )
d$A <- standardize( d$area )
d$W <- standardize( d$weight )

# formalize and fit

m_FGA <- quap( alist(
                 W ~ dnorm( mu, sigma ),
                 mu <- a + bF*F + bG*G + bA*A,
                    a ~ dnorm( 0, 0.2 ),
                    c(bF, bG, bA) ~ dnorm( 0, 0.5 ),
                 sigma ~ dexp( 1 )
                 ), data=d )

m_FG <- quap( alist(
                 W ~ dnorm( mu, sigma ),
                 mu <- a +  bF*F + bG*G,
                    a ~ dnorm( 0, 0.2 ),
                    c(bF, bG) ~ dnorm( 0, 0.5 ),
                 sigma ~ dexp( 1 )
                 ), data=d )

m_F <- quap( alist(
                 W ~ dnorm( mu, sigma ),
                 mu <- a + bF*F,
                    a ~ dnorm( 0, 0.2 ),
                    bF ~ dnorm( 0, 0.5 ),
                 sigma ~ dexp( 1 )
                 ), data=d )

m_A <- quap( alist(
                 W ~ dnorm( mu, sigma ),
                 mu <- a + + bA*A,
                    a ~ dnorm( 0, 0.2 ),
                    bA ~ dnorm( 0, 0.5 ),
                 sigma ~ dexp( 1 )
                 ), data=d )

#
#
# Homework week 5
#
#

# Exercise 1
#
library( rethinking ) ; library( dagitty )
data( Wines2012 ) ; d <- Wines2012

# Draw a dag
#
dag <- dagitty("dag{ J -> S <- W} ") 
plot( dag )

# Prepare variables
#

# Prepare: data set & variables 
d_slim <- list( S = standardize( d$score ),
                wid = as.integer( d$wine ),
                jid = as.integer(d$judge)
)

# Fit the model 
m1 <- ulam( 
            alist( 
                  S ~ dnorm( mu, sigma ),
                    mu <- j[jid] + w[wid],
                        j[jid] ~ dnorm( 0, 0.5 ),
                        w[wid] ~ dnorm( 0, 0.5 ),
                    sigma ~ dexp( 1 )
            ), data=d_slim, chains=4, cores=4)

traceplot( m1 ) ; trankplot( m1 )

# Plot to interpret
plot( precis(m1, depth=2) )

# Exercise 2
#

ls2 <- list( 
            S = standardize( d$score ),
            W = d$wine.amer,
            J = d$judge.amer,
            R = ifelse( d$flight=="red", 1L, 0L )
)

precis( ls2 )

m2 <- ulam( 
           alist(
                 S ~ dnorm( mu, sigma ),
                 mu <- a + bW*W + bJ*J + bR*R,
                    a ~ dnorm( 0, 0.1 ),
                    c(bW, bJ, bR) ~ dnorm( 0, 0.5 ),
                sigma ~ dexp( 1 )
           ), data=ls2, cores = 4, chains=4)

precis( m2 )
plot( precis( m2 ) )

dag <- dagitty("dag{ 
               J -> S
               W -> S
               F -> S
               } ") 
plot( dag )

ls3 <- list(
            S = standardize( d$score ),
            # since the min(wine.amer) = 0 + 1
            wid = as.integer(d$wine.amer) + 1L,
            # since the min(judge) = 0 + 1
            jid = as.integer(d$judge.amer) + 1L,
            fid = ifelse(d$flight=="red", 1L, 2L)
)

m3 <- ulam( 
           alist(
                 S ~ dnorm( mu, sigma ),
                 mu <- w[wid] + j[jid] + f[fid],
                 w[wid] ~ dnorm( 0, 0.5 ),
                 j[jid] ~ dnorm( 0, 0.5 ),
                 f[fid] ~ dnorm( 0, 0.5 ),
                 sigma ~ dexp( 1 ) 
                 ), data = ls3, chains=4, cores=4)

plot( precis( m3, depth=2 ) )

traceplot( m3 )
pairs(m3)

# Contrasts
#
post <- extract.samples( m3 )

diff_w <- post$w[,2] - post$w[,1]
precis( diff_w )
diff_j <- post$j[,2] - post$j[,1]
precis( diff_j )
diff_f <- post$f[,2] - post$f[,1]
precis( diff_f )

#
# 3 
#

# indicator approach
#

ls2 <- list( 
            S = standardize( d$score ),
            W = d$wine.amer,
            J = d$judge.amer,
            R = ifelse( d$flight=="red", 1L, 0L )
)

m4 <- ulam( 
           alist( 
                 S ~ dnorm( mu, sigma ),
                    mu <- a + bW*W + bJ*J + bR*R +
                        bWJ*W*J + bJR*J*R + bWR*W*R,
                    a ~ dnorm( 0, 0.2 ),
                    c(bW, bJ, bR) ~ dnorm( 0, 0.5 ),
                    c(bWJ, bJR, bWR) ~ dnorm( 0, 0.25 ),
                    sigma ~ dexp( 1 )
           ), data=ls2, chains=4, cores=4)

precis( m4 )

pred_dat <- data.frame( 
                       W = rep( 0:1, times=4 ),
                       J = rep( 0:1, each=4 ),
                       R = rep( c(0,0,1,1), times=2 )
)

mu <- link( m4, data=pred_dat )
row_labels <- paste( ifelse(pred_dat$W==1, "Aw", "Fw"),
                    ifelse(pred_dat$J==1, "Aj", "Fj"),
                    ifelse(pred_dat$R==1, "R", "W"), 
                    sep="|") 

plot( precis( list(mu=mu), 2 ), labels=row_labels )

# Index version
#

ls3 <- list(
            S = standardize( d$score ),
            # since the min(wine.amer) = 0 + 1
            wid = as.integer(d$wine.amer) + 1L,
            # since the min(judge) = 0 + 1
            jid = as.integer(d$judge.amer) + 1L,
            fid = ifelse(d$flight=="red", 1L, 2L)
)

mcode <- "
data{
    vector[180] S; 
    int fid[180];
    int jid[180];
    int wid[180];
    }
parameters{
    real w[2,2,2];
    real<lower=0> sigma;
    }
model{
    vector[180] mu;
    sigma ~ exponential( 1 );
    for ( i in 1:2 )
        for ( j in 1:2 )
            for ( k in 1:2)
                w[i,j,k] ~ normal( 0 , 0.5 );
    for ( i in 1:180 ) {mu[i] = w[wid[i], jid[i], fid[i]];}
    S ~ normal( mu , sigma );
}
"
m3b <- stan( model_code=mcode , data=ls3 , chains=4 , cores=4 )

row_labels = c("FFR","FFW","FAR","FAW","AFR","AFW","AAR","AAW" )
plot( precis( m3b, depth = 3, pars="w" ), labels=row_labels )

#
#
# Homework 6
#
#

library(rethinking)
data(NWOGrants)
d <- NWOGrants

library(dagitty)
dag <- dagitty("dag{
               G -> A G->D->A;
               G [exposure]
               D [exposure]
               A [outcome]
}")
coordinates( dag ) <- list( 
                           x= c(G=0, A=0.5, D=1),
                           y= c(G=0, A=1, D=0)
)
plot(dag)

# Total xcausal effect of Gender
#

# Reduced data list
dat_list <- list(
                 awards = d$awards,
                 apps = as.integer( d$applications ),
                 gid = ifelse( d$gender=="m", 1L, 2L )
)

mdl_1 <- alist(
               awards ~ dbinom( apps, p ),
               logit( p ) <- a[ gid ],
               a[ gid ] ~ normal( -1,1 )
)

fit_1 <- ulam( mdl_1, data=dat_list, chains=4 )
precis( fit_1, depth = 2 )

# Contrast
post <- extract.samples( fit_1 )
# male - female
diff <- inv_logit(post$a[ ,1 ]) - inv_logit(post$a[ ,2 ])
precis( diff ) 

# Direct xcausal effect of Gender
#

dat_list$did <- as.integer(d$discipline)

mdl_2 <- alist(
               awards ~ dbinom( apps, p ),
               logit( p ) <- a[ gid ] + d[ did ],
               a[ gid ] ~ normal( -1,1 ),
               d[ did ] ~ normal( 0,1 )
)

fit_2 <- ulam( mdl_2, data=dat_list, chains=4 )
precis( fit_2, depth = 2 )

# Contrast
post <- extract.samples( fit_2 )
# male - female
diff <- inv_logit(post$a[ ,1 ]) - inv_logit(post$a[ ,2 ])
precis( diff ) 
postcheck( fit_2 )

# Exercise 2 
#

library(dagitty)
dag <- dagitty("dag{
               G -> A G->D->A A <-S->D; 
               G [exposure]
               D [exposure]
               A [outcome]
               S [unobserved]
}")
coordinates(dag) <-  list( 
                          x = c(G=0, A=0.5, D=0.5, S=1),
                          y = c(G=0, A=1, D=0, S=0)
)
plot(dag)

# Note: not possivle to get an unconfounded estimate of gender on awards

#
# Exercise 3
#

dag <- dagitty( "dag{
               brain -> soclearn;
}" )
coordinates( dag ) <- list( 
                           x = c( soclearn=1, brain=0 ),
                           y = c( soclearn=0, brain=0 )
                            
)
plot( dag )

library( rethinking )
data( Primates301 )
d <- Primates301
d <- d[complete.cases(d$species, d$brain, d$research_effort),]

dat_list <- list( 
                 soclearn = as.integer(d$social_learning),
                 log_brain = standardize( log(d$brain) ),
                 log_effort = standardize( log(d$research_effort) )
)

mdl <- alist( 
             soclearn ~ poisson( lambda ),
             log(lambda) <- a + bb*log_brain,
             a ~ normal(0, 1),
             bb ~ normal( 0, 0.5 )
)

fit <- ulam( mdl, data=dat_list, chains= 4, cores=4, log_lik = TRUE )

precis( fit, depth=2 )

postcheck( fit, window = 50 )

# Include research effort
#

mdl_2 <- alist( 
             soclearn ~ poisson( lambda ),
             log(lambda) <- a + bb*log_brain + be*log_effort,
             a ~ normal(0, 1),
             bb ~ normal( 0, 0.5 ),
             be ~ normal( 0, 0.5 )
)

fit_2 <- ulam( mdl_2, data=dat_list, chains= 4, cores=4, log_lik = TRUE)
precis( fit, depth=2 )


waic_1 <- WAIC( fit, pointwise=TRUE )
waic_2 <- WAIC( fit_2, pointwise=TRUE )

plot( waic_1$WAIC - waic_2$WAIC, dat_list$log_effort, col=rangi2, pch=16 )
identify( waic_1$WAIC, waic_2$WAIC, d$genus, cex=0.5)
abline( v=0, lty=2, lwd=.5 )

library( dagitty )
dag <- dagitty( "dag{
               brain -> effort;
               brain -> soclearn;
               effort -> soclearn
}" )
coordinates( dag ) <- list( 
                           x=c(brain = 0, effort= 1, soclearn= 0.5 ),
                           y=c(brain =0, effort=0, soclearn=1 )
                           )
plot(dag)


################
# Trolley Data #
################
pkg <- c( "rethinking", "dagitty" )
lapply( pkg, library, character.only = TRUE )

data( Trolley )
d <- Trolley

dag <- dagitty( "dag{
               A -> E -> R <- A
}" )
coordinates( dag ) <- list( x=c(A=0 , E=1 , R=0.5 ), y=c(A=0 , E=0 , R=0.5) )
plot( dag )

edu_ord <- c( 6, 1, 8, 4, 7, 2, 5, 3) 
d$edu_new <- edu_ord[ d$edu ] 

idx <- 1:nrow(d)

dat <- list( 
            R = d$response[idx],
            A = d$action[idx],
            I = d$intention[idx],
            C = d$contact[idx],
            E = as.integer( d$edu_new[idx] ),
            edu_norm = normalize( d$edu_new[idx] ),
            age = standardize( d$age ),
            alpha = rep(2,7) #gprior
)

m1 <- ulam( 
           alist(
                 R ~ ordered_logistic( phi, cutpoints ),
                    phi <- bE*sum( delta_shell[1:E] ) + bA*A + bC*C + BI*I + bAge*age,
                        BI <- bI + bIA*A + bIC*C,
                    c( bE, bA, bC, bI, bAge, bIA, bIC ) ~ normal( 0, 0.5 ),
                    cutpoints ~ normal( 0, 1.5 ),
                    vector[8]: delta_shell <<- append_row( 0, delta ),
                    simplex[7]: delta ~ dirichlet( alpha )
           ), data=dat, chains=4, cores=4)

precis( m1, omit="cutpoints")




dag <- dagitty( "dag{
               A -> E -> R <- A ; 
               G -> E ; G -> R
}" )
coordinates( dag ) <- list( x=c(A=0 , E=1 , R=0.5, G=1.5 ), y=c(A=0 , E=0 , R=1, G=0.5) )
plot(dag)

adjustmentSets( dag, exposure = "E", outcome = "R", effect="total" )

dat$female <- ifelse( d$male=="m", 0L, 1L )


m2 <- ulam( alist(
                  R ~ ordered_logistic( phi, cutpoints ),
                  phi <- bE*sum( delta_shell[1:E] ) + bA*A + bC*C + BI*I + bAge*age + bF*female,
                    BI <- bI + bIC*C + bIA*A,
                  c(bE, bA, bC, bAge, bF, bI, bIC, bIA) ~ normal( 0, 0.5 ),
                  cutpoints ~ normal( 0, 0.5 ),
                  vector[8]: delta_shell <<- append_row( 0, delta ),
                  simplex[7]: delta ~ dirichlet( alpha )
                  ), data=dat, chains=4, cores=4 )

precis( m2 )
plot( precis( m2 ) )


#
# Week 8
#
library( rethinking )
data( reedfrogs )
d <- reedfrogs

dat <- list(
            S = d$surv,
            n = d$density,
            tank = 1:nrow(d),
            pred = ifelse( d$pred=="no", 1L, 2L ),
            size_ = ifelse( d$size=="small", 1L, 2L )
)

m1.1 <- ulam( 
             alist( 
                   # outcome
                   S ~ binomial( n, p ),
                   # likelihood
                   logit( p ) <- a[tank],
                   # Prior
                   a[tank] ~ normal( a_bar, sigma ),
                   # Hyperpriors
                   a_bar ~ normal( 0, 1.5 ),
                   sigma ~ exponential( 1 )
                   ), data=dat, chains=4, cores=4, log_lik=TRUE)

precis( m1.1, depth=2 )

m1.2 <- ulam( 
             alist( 
                   # outcome
                   S ~ binomial( n, p ),
                   # likelihood
                   logit( p ) <- a[tank] + bp*pred,
                   # Prior
                   bp ~ normal( -0.5, 1 ),
                   a[tank] ~ normal( a_bar, sigma ),
                   # Hyperpriors
                   a_bar ~ normal( 0, 1.5 ),
                   sigma ~ exponential( 1 )
                   ), data=dat, chains=4, cores=4, log_lik=TRUE)

precis( m1.2, depth=2 )

m1.3 <- ulam( 
             alist( 
                   # outcome
                   S ~ binomial( n, p ),
                   # likelihood
                   logit( p ) <- a[tank] + s[size_],
                   # Prior
                   a[tank] ~ normal( a_bar, sigma ),
                   s[size_] ~ normal( 0, 0.5 ),
                   # Hyperpriors
                   a_bar ~ normal( 0, 1.5 ),
                   sigma ~ exponential( 1 )
                   ), data=dat, chains=4, cores=4, log_lik=TRUE)


precis( m1.3, depth=2 )

m1.4 <- ulam( 
             alist( 
                   # outcome
                   S ~ binomial( n, p ),
                   # likelihood
                   logit( p ) <- a[tank] + bp*pred + s[size_],
                   # Prior
                   s[size_] ~ normal( 0, 0.5 ),
                   bp ~ normal( -0.5, 1 ),
                   a[tank] ~ normal( a_bar, sigma ),
                   # Hyperpriors
                   a_bar ~ normal( 0, 1.5 ),
                   sigma ~ exponential( 1 )
                   ), data=dat, chains=4, cores=4, log_lik=TRUE)

precis( m1.4, depth=2 )

m1.5 <- ulam( 
             alist( 
                   # outcome
                   S ~ binomial( n, p ),
                   # likelihood
                   logit( p ) <- a_bar + z[tank]*sigma + s[size_] + bp[size_]*pred,
                   # Prior
                   s[size_] ~ normal( 0, 1 ),
                   bp[size_] ~ normal( -0.5, 1 ),
                   # a[tank] ~ normal( a_bar, sigma ),
                   z[tank] ~ normal( 0, 1 ),
                   # Hyperpriors
                   a_bar ~ normal( 0, 1.5 ),
                   sigma ~ exponential( 1 )
                   ), data=dat, chains=4, cores=4, log_lik=TRUE)

compare(m1.1, m1.2, m1.3, m1.4, m1.5)

plot( coeftab( m1.1, m1.2, m1.3, m1.4, m1.5 ), pars="sigma" )


# bangladesh fertility data
library( rethinking )
data( bangladesh )
d <- bangladesh
unique(d$district)

d$d_id <- as.integer(as.factor(d$district))

dat <- list(
            C=d$use.contraception,
            did=d$d_id
)

m2.1 <- ulam( 
             alist(
                    C ~ bernoulli(p),
                    logit(p) <- a[did],
                    a[did] ~ normal( 0, 1.5 )
                    ), data=dat, chains=4, cores=4, log_lik = TRUE)

m2.2 <- ulam( 
             alist(
                    C ~ bernoulli(p),
                    logit(p) <- a[did],
                    a[did] ~ normal( a_bar, sigma ),
                    a_bar ~ normal( 0,1.5 ),
                    sigma ~ exponential(1)
                    ), data=dat, chains=4, cores=4, log_lik = TRUE)

post1 <- extract.samples( m2.1 )
post2 <- extract.samples( m2.2 )

p1 <- apply( X=inv_logit(post1$a), 2, mean )
p2 <- apply( X=inv_logit(post2$a), 2, mean )

nd <- max( dat$d )
plot( NULL, xlim=c(1,nd), ylim=c(0,1), ylab="prob use contraception", xlab="district")
points( 1:nd, p1, pch=16, col=rangi2 )
points( 1:nd, p2, pch=16)
abline( h=mean(inv_logit(post2$a_bar)), lty=2 )

# Trolley data
#
library( rethinking )
data( Trolley )
d <- Trolley

dat <- list(
            R = d$response,
            A = d$action,
            I = d$intention,
            C = d$contact
)
m1.1 <- ulam( 
             alist(
                   R ~ dordlogit( phi, cutpoints ),
                   phi <- bA*A + bC*C + BI*I,
                   BI <- bI + bIA*A + bIC*C,
                   c(bA, bI, bC, bIA, bIC) ~ dnorm( 0, 0.5 ),
                   cutpoints ~ dnorm( 0, 1.5 )
             ), data=dat, chains=4, cores=4, log_lik=TRUE
)
# Create an ID variable

dat$id <- coerce_index( d$id )

m1.2 <- ulam(
             alist(
                   # outcome
                   R ~ dordlogit( phi, cutpoints ),
                   # Likelihood
                   phi <- a[id] + bA*A + bC*C + BI*I,
                   BI <- bI + bIA*A + bIC*C,
                   c(bA, bI, bC, bIA, bIC) ~ dnorm( 0, 0.5 ),
                   cutpoints  ~ dnorm( 0, 1.5 ),
                   # adaptive prior
                   a[id] ~ dnorm( 0, sigma ),
                   sigma ~ dexp( 1 )
             ), data=dat, chains=4, cores=4, log_lik=TRUE
)

precis(m1.1) ; precis(m1.2)
# Control awa some Vartn bc. repeated sampling
compare(m1.1, m1.2)









