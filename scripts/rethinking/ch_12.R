#########################
# Monsters and Mixtures #
#########################

# 12.1. Over-dispersed counts
library( rethinking )

pbar <- 0.5
par( mfrow = c(2,5) )
for ( i in 1:10 ) {
    curve( dbeta2(x,pbar, i) , from=0 , to=1 ,
      xlab="probability" , ylab="Density" ) 
    mtext( paste0( "theta= ", i ) )
}

theta <- 5
par( mfrow = c(2,5) )
for ( i in seq(0.1, 1, by = 0.1) ) {
    curve( dbeta2(x, i, theta) , from=0 , to=1 ,
      xlab="probability" , ylab="Density" ) 
    mtext( paste0( "p= ", i ) )
}

library( rethinking )
data( UCBadmit )
d <- UCBadmit

# GID: gender index
library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$gid <- ifelse( d$applicant.gender=="male" , 1L , 2L )
dat <- list( A=d$admit , N=d$applications , gid=d$gid )
m12.1 <- ulam(
              alist(
                    A ~ dbetabinom( N , pbar , theta ),
                    logit(pbar) <- a[gid],
                    a[gid] ~ dnorm( 0 , 1.5 ),
                    transpars> theta <<- phi + 2.0,
                    phi ~ dexp(1)
                    ), data=dat , chains=4 )
post <- extract.samples( m12.1 )
post$da <- post$a[,1] - post$a[,2]
precis( post , depth=2 )

gid <- 2
# draw posterior mean beta distribution
curve( dbeta2(x,mean(logistic(post$a[,gid])),mean(post$theta)) , from=0 , to=1 ,
ylab="Density" , xlab="probability admit", ylim=c(0,3) , lwd=2 )
# draw 50 betadistributions sampled from posterior
for ( i in 1:50 ) {
p <- logistic( post$a[i,gid] )
theta <- post$theta[i]
curve( dbeta2(x,p,theta) , add=TRUE , col=col.alpha("black",0.2) )
}
mtext( "distribution of female admission rates" )

postcheck( m12.1 )

# Comparison model -- uncertainty is much smaller
dat$did <- as.integer( d$dept )
dat$applications <- d$applications
dat$admit <- d$admit

m.c <- ulam( 
            alist(
                  admit ~ binomial(applications, p),
                  logit( p ) <- a[gid] + d[did],
                  a[gid] ~ dnorm( 0, 1.5 ),
                  d[did] ~ dnorm( 0, 1.5 )
                  ), data = dat, cores=4, chains=4 )

postcheck(m.c)

# 12.2 Gamma Poison

library( rethinking )
data( Kline )
d <- Kline
d$P <- standardize( log(d$population) )
d$cid <- ifelse( d$contact == "low", 1L, 2L )

dat <- list(
            T = d$total_tools,
            P = d$population,
            cid = d$cid
)

m12.2 <- ulam( 
              alist(
                    T ~ dgampois( lambda, phi ) ,
                    log(lambda) <- exp( a[cid] ) * P^b[cid] / g ,
                    a[cid] ~ dnorm( 1,1 ) ,
                    b[cid] ~ dexp( 1 ) ,
                    g ~ dexp( 1 ) ,
                    phi ~ dexp( 1 )

              ), data = dat, chains = 4, cores = 4, log_lik=TRUE
)
precis( m12.2, depth=2 )

lbl <- levels(  d$culture )
postcheck( m12.2 )
mtext( lbl, at = 1:length(lbl), side = 1 )

# Zero-inflated outcomes
library( rethinking )
# Simulate monk data

# define parameters
prob_drink <- 0.2 # 20% of days
rate_work <- 1
# average 1 manuscript per day

# sample one year of production
N <- 365
# simulate days monks drink
set.seed(365)
drink <- rbinom( N , 1 , prob_drink )
# simulate manuscripts completed
y <- (1-drink)*rpois( N , rate_work )

simplehist( y , xlab="manuscripts completed" , lwd=4 )
zeros_drink <- sum(drink)
zeros_work <- sum(y==0 & drink==0)
zeros_total <- sum(y==0)
lines( c(0,0) , c(zeros_work,zeros_total) , lwd=4 , col=rangi2 )

m12.3 <- ulam(
              alist(
                    y ~ dzipois( p, lambda ),
                    logit( p ) <- ap,
                    log( lambda ) <- al,
                    ap ~ dnorm( -1.5, 1 ),
                    al ~ dnorm( 1, 0.5 )
              ), data = list( y=y ), chains = 4, cores = 4
)
precis( m12.3 )

post <- extract.samples( m12.3 )
d_mean <- mean( inv_logit( post$ap ) ) # probability drin
d_sd <- sd( inv_logit( post$ap ) ) # probability drin

mean( exp( post$al ) ) # average rate finish manuscript
sd( exp( post$al ) ) # average rate finish manuscript


m12.3_alt <- ulam(
                  alist(
                        y|y>0 ~ custom(log1m(p) + poisson_lpfm(y|lambda)),
                        y|y==0 ~ custom(log_mix(p, 0, poisson_lpfm(0|lambda))),
                        logit(p) <- ap,
                        log(lambda) <- al,
                        ap ~ dnorm(-1.5, 1),
                        al ~ dnorm(1, 0.5),

                  ), data=list(y=as.integer(y)), chains=4
)

stancode( m12.3 )

## 12.3: Ordered categorical outcomes

library( rethinking )
data( Trolley )
d <- Trolley 

simplehist( d$response, xlim=c(1,7), xlab="response" )

# probabilites 
pr_k <- table( d$response ) / nrow(d)

#cumulative probabilites 
cum_pr_k <- cumsum( pr_k )

# Plot
plot( 1:7, cum_pr_k, type="b", xlab="response", ylab ="cumulatice proportion",
     ylim=c(0,1) )

# log cumulative odds: alpha k
logit <- function( x ) log(x / (1-x))
(log_cum_pr_k <- round( lco <- logit( cum_pr_k ), 2 ))
plot( 1:7,  log_cum_pr_k, type="b", xlab="response", ylab="log cumulative
     proportions", ylim=c(-4,4))

dat <- list( R = d$response )

m12.4 <- ulam( 
               alist( 
                      R ~ dordlogit( 0, kappa ),
                      kappa ~ dnorm( 0, 1.5 )
                   ), data = dat, chains=4, cores=4
)

# output on the log cumulative odds scale
precis( m12.4, depth=2 )
cutpoints <- round( inv_logit( coef(m12.4) ), 3 )
plot( 1:7, c(cutpoints,1), type="b" )

round( pk <- dordlogit( 1:7, 0, coef( m12.4 ) ) ,2 )
sum( pk*( 1:7 ) )

round( pk <- dordlogit( 1:7, 0, coef( m12.4 ) -0.5 ) ,2 )
# If we subtract smth, the mean increases 
sum( pk*( 1:7 ) )                      

round( pk <- dordlogit( 1:7, 0, coef( m12.4 ) +0.5 ) ,2 )
# If we add smth, the mean decreases 
sum( pk*( 1:7 ) )                      

dat <- list( 
            R = d$response,
            A = d$action,
            I = d$intention,
            C = d$contact
)

m12.5 <- ulam( 
              alist(
                    R ~ dordlogit( phi, cutpoints ),
                        cutpoints ~ dnorm( 0, 1.5 ),
                        phi <- bA*A + bC*C + BI*I,
                              BI <- bI + bIA*A + bIC*C,
                              c(bA, bC, bI) ~ dnorm(0, 0.5),
                              c(bIA, bIC) ~ dnorm(0, 0.5)


              ), data = dat, chains=4, cores=4
)
precis( m12.5 )
plot( precis(m12.5), xlim=c(-1.4,0) )


fun <- function( kA, kC ) {
      plot( NULL, type="n", xlab="intention", ylab="probability",
           xlim=c(0,1), ylim=c(0,1), xaxp=c(0,1,1), yaxp=c(0,1,2)
      )
      mtext(paste( "action=", kA, "contact=", kC))
      kI <- 0:1 #value intention to calculate over 
      pdat <- data.frame( A=kA, C=kC, I=kI )
      phi <- link( m12.5, data=pdat )$phi
      post <- extract.samples( m12.5 )
      for( s in 1:50 ) {
            pk <- pordlogit( 1:6, phi[s, ], post$cutpoints[s, ] )
            for( i in 1:6 ) lines( kI, pk[,i], col=grau(0.1) )
      }

}
par( mfrow=c(1,3) )
fun( kA=0, kC=0 ) ; fun( kA=1, kC=0 ) ; fun( kA=0, kC=1 )


iphist <- function( kA, kC ) {
      # Implied histogram
      kI <- 0:1 #value intention to calculate over 
      pdat <- data.frame( A=kA, C=kC, I=kI )
      s <- sim( m12.5, data=pdat )
      simplehist( s, xlab="response" )
      mtext(paste( "action=", kA, "contact=", kC))
}
par( mfrow=c(1,3) )
iphist( kA=0, kC=0 ) ; iphist( kA=1, kC=0 ) ; iphist( kA=0, kC=1 )


################################################################################


library( rethinking )
data( Trolley ) 
d <- Trolley
# integrating edu
levels( d$edu )
# sort edu
edu_levels <- c( 6, 1, 8, 4, 7, 2, 5, 3 )
d$edu_new <- edu_levels[ d$edu ]

# Ordered logit model
library( gtools )
set.seed( 1805 )
delta <- rdirichlet( 10, alpha=rep(2,7) )
str( delta )
h <- 3
plot( NULL, xlim=c(1,7), ylim=c(0,0.4), xlab="index", ylab="probability" )
for ( i in 1:nrow(delta) ) lines( 1:7, delta[i,], type="b",
pch=ifelse(i==h, 16, 1), lwd=ifelse(i==h,4,1.5),
col=ifelse(i==h, "black", col.alpha("black", 0.7)))

dat <- list(
            R = d$response,
            action = d$action,
            intention = d$intention,
            contact = d$contact,
            E = as.integer( d$edu_new ), # index
            alpha = rep( 2, 7 ) # delta prior
)

m12.6 <- ulam(
              alist(
                    R ~ ordered_logistic( phi , kappa ),
                    phi <- bE*sum( delta_j[1:E] ) + bA*action + bI*intention + bC*contact,
                    kappa ~ normal( 0 , 1.5 ),
                    c(bA,bI,bC,bE) ~ normal( 0 , 1 ),
                    vector[8]: delta_j <<- append_row( 0 , delta ),
                    simplex[7]: delta ~ dirichlet( alpha )
                    ), data=dat , chains=4 , cores=4 )


delta_labels <- c("Elem", "MidSch", "SHS", "HSG", "SCol", "Bach", "Mast", "Grad")
pairs( m12.6, pars="delta", labels=delta_labels )

# Repaarameterized model: ordinary 
dat$edu_norm <- normalize( d$edu_new )
m12.7 <- ulam(
              alist(
                    R ~ ordered_logistic( mu , cutpoints ),
                    mu <- bE*edu_norm + bA*action + bI*intention + bC*contact,
                    c(bA,bI,bC,bE) ~ normal( 0 , 1 ),
                    cutpoints ~ normal( 0 , 1.5 )
                    ), data=dat , chains=4 , cores=4 )
precis( m12.7 )





















































