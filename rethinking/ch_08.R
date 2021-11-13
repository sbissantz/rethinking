#
#
# Conditional manatees 
#
#

library( rethinking )
library( dagitty )

dag <- dagitty( "dag{
               R -> G <- C ; U -> G ; U -> R;
}" )

coordinates( dag ) <- list( x = c(R=0, G=0.5, C=1, U=0.5), 
                           y = c(R=0, G=0, C=0, U=1) )
plot( dag )

#
# 8.1 Building an interaction
#

data( rugged )
d <- rugged

# log outcome 
d$log_gdp <- log( d$rgdppc_2000 )

# extract the samples with GDP data
dd <- d[ complete.cases( d$rgdppc_2000 ), ]

# Rescale log gdp 
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

# Mdl 
m8.1 <- quap( 
             alist(
                   log_gdp_std ~ dnorm( mu, sigma ),
                   # mean( dd$rugged_std )
                   # [1] 0.2149601
                   mu <- a + b *( rugged_std - 0.215 ),
                   a ~ dnorm( 1, 0.1 ),
                   # a ~ dnorm( 1, 1 ),
                   b ~ dnorm( 0, 0.3 ),
                   # b ~ dnorm( 0, 1 ),
                   sigma ~ dexp( 1 )
             ), data=dd
)

# PPD
set.seed( 7 )
prior <- extract.prior( m8.1 )
# set up te plot dimensions
plot( NULL, xlim=c(0,1), ylim=c(0.5,1.5), xlab="ruggedness", ylab="log GDP" )
# extrem values for GDP in the data 
abline( h=min(dd$log_gdp_std), lty=2 )
abline( h=max(dd$log_gdp_std), lty=2 )
# 50 lines from the prior
rugged_seq <- seq( from=-0.1, to=1.1, length.out=30 )
mu <- link( m8.1, post=prior, data=data.frame(rugged_std=rugged_seq) )
for( i in 1:50 ) lines( rugged_seq, mu[i,], col=col.alpha("black", 0.3) )

# Analyzing the range of the effect of beta ~ N(9,0.3)
sum( abs(prior$b) > 0.6  ) / length(prior$b)    # 5% of the slops > 0.6 

# Evaluate the posterior
precis( m8.1 )

# IA Model (varying intercept)
#

dd$cid <- ifelse( dd$cont_africa, 1, 2 )

m8.2 <- quap( 
             alist(
                   log_gdp_std ~ dnorm( mu, sigma ),
                   mu <- a[cid] + b*( rugged_std - 0.215 ),
                   a[cid] ~ dnorm( 1, 0.1 ),
                   b ~ dnorm( 0, 0.3 ),
                   sigma ~ dexp( 1 )
                   ), data=dd )

compare( m8.1, m8.2 )
precis( m8.2, depth=2 )

# Posterior contrast -- diff between African/Non-African GDP
post <- extract.samples( m8.2 )
diff_a1_a2 <- post$a[,1] - post$a[,2]
PI( diff_a1_a2 )

# Sample from the Posterior and compute the predicted means
rugged.seq <- seq( from=-0.1, to=1.1, length.out=30 )
# compute mu over samples, fixing cid=2; cid=1
mu.NotAfrica <- link( m8.2, data=data.frame( cid=2, rugged_std=rugged_seq ) )
mu.Africa <- link( m8.2, data=data.frame( cid=1, rugged_std=rugged_seq ) )
#summarize to means and intervals
mu.NotAfrica_mu <- apply( mu.NotAfrica, 2, mean )
mu.NotAfrica_ci <- apply( mu.NotAfrica, 2, PI, prob=.89 )
mu.Africa_mu <- apply( mu.Africa, 2, mean )
mu.Africa_ci <- apply( mu.Africa, 2, PI, prob=.89 )

m8.3 <- quap( 
             alist(
                   log_gdp_std ~ dnorm( mu, sigma ),
                   mu <- a[cid] + b[cid] * (rugged_std - 0.215),
                   a[cid] ~ dnorm( 1, 0.1 ),
                   b[cid] ~ dnorm( 0, 0.3 ),
                   sigma ~ dexp( 1 )
             ), data=dd)

precis( m8.5, depth=2 )
compare( m8.1, m8.2, m8.3, func=PSIS )

# Plot the high leverage points
plot( PSIS( m8.3, pointwise=TRUE )$k )

# Plot the IA mdl
#
par( mfrow=c(1,2) )
d.A1 <- dd[ dd$cid==1, ] 
plot( d.A1$rugged_std, d.A1$log_gdp_std, pch=16, col=rangi2,
xlab="ruggedness (standardized)", ylab="log GDP (as proportion of the mean)",
             xlim=c(0,1) )
mu <- link( m8.3, data=data.frame( cid=1, rugged_std=rugged_seq ) )
mu_mean <- apply( mu, 2, mean )
mu_ci <- apply( mu, 2, PI, prob=0.89 )
lines( rugged_seq, mu_mean, lwd=2 )
shade( mu_ci, rugged_seq, col=col.alpha(rangi2,0.3) )
mtext("African nations")

d.A2 <- dd[ dd$cid==2, ] 
plot( d.A2$rugged_std, d.A2$log_gdp_std, pch=1, 
xlab="ruggedness (standardized)", ylab="log GDP (as proportion of the mean)",
             xlim=c(0,1) )
mu <- link( m8.3, data=data.frame( cid=2, rugged_std=rugged_seq ) )
mu_mean <- apply( mu, 2, mean )
mu_ci <- apply( mu, 2, PI, prob=0.89 )
lines( rugged_seq, mu_mean, lwd=2 )
shade( mu_ci, rugged_seq, col=col.alpha("black",0.3) )
mtext("Non-African nations")

#
# Symmetrie of IA'S
# 

# Counterfactual plot
# Africa ~ log GDP |  ruggedness
#

rugged_seq <- seq( from=-0.2, to=1.2, length.out=30 )
muA <- link( m8.3, data=data.frame(cid=1, rugged_std=rugged_seq) )
muN <- link( m8.3, data=data.frame(cid=2, rugged_std=rugged_seq) )
# exp. difference in log GDP: A vs NA countries
delta <- muA - muN


#
# Continous IA's
#

library( rethinking )
data( tulips )
d <- tulips
str( d )


library( dagitty )
dag <- dagitty( 'dag { 
               W -> B ; S -> B
}' )
plot( dag )

# Prepare the variables
#

d$blooms_std <- d$blooms / max(d$blooms) # [0,1] 
d$water_cent <- d$water - mean(d$water) # [-1, 1]
d$shade_cent <- d$shade - mean(d$shade) # [-1, 1]

# Prior thinking 
#

a <- rnorm( 1e4, mean=0.5, sd=1 )
sum ( a < 0 | a > 1 ) / length( a )
# [1] 0.6204 
# over 60% of the probability mass is outside the range
hist( a )
# Improve:
a <- rnorm( 1e4, mean=0.5, sd=0.25 )
sum ( a < 0 | a > 1 ) / length( a )
# [1] 0.0445
hist( a )

range( d$water_cent ) ; range( d$shade_cent )
# [1] -1  1

# Max: d(0:blooms to max:bloms) = 1
# 2 variables -> 2 x 0.5 = 1 
# Std of each = 2(r/l) x 0.25
b <- rnorm( 1e4, mean=0, sd=0.25)
hist( b )

# Main effects 
#

m8.4 <- quap( 
             alist(
                   blooms_std ~ dnorm( mu, sigma ),
                   mu <- a + bw*water_cent + bs*shade_cent,
                   a ~ dnorm( 0.5, 0.25 ),
                   bw ~ dnorm( 0, 0.25 ),
                   bs ~ dnorm( 0, 0.25 ),
                   sigma ~ dexp( 1 )
             ), data=d
)

# IA 
#

# bw ü bws*Si = 0  | Si = max = 1 -> bw = -bws (= largest conceivable IA)
m8.5 <- quap( 
             alist(
                   blooms_std ~ dnorm( mu, sigma ),
                   mu <- a + bw*water_cent + bs*shade_cent + 
                         bws*water_cent*shade_cent,
                   a ~ dnorm( 0.5, 0.25 ),
                   bw ~ dnorm( 0, 0.25 ),
                   bs ~ dnorm( 0, 0.25 ),
                   bws ~ dnorm( 0, 0.25 ),
                   sigma ~ dexp( 1 )
             ), data=d
)

# PPD
#

# Main effects 
par(mfrow=c(1,3)) # 3 plots in 1 row
for ( s in -1:1 ) { 
      idx <- which( d$shade_cent==s ) 
      plot( d$water_cent[idx] , d$blooms_std[idx] , 
           xlim=c(-1,1) , ylim=c(-1,2) ,
           xlab="water" , ylab="blooms" , 
           pch=16 , col=rangi2 )
      mtext(paste0("S (prior)= ", s))
      prior <- extract.prior(mdl)
      mu <- link( m8.4 , data=data.frame( shade_cent=s , water_cent=-1:1 ), 
                 post=prior)
      abline(  h=0 ) ; abline( h=1 )
      for ( i in 1:35 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
      # Make 1st line red
      lines( -1:1, mu[1,], lwd=2, col="red" )
}

# IA
par(mfrow=c(1,3)) # 3 plots in 1 row
for ( s in -1:1 ) { 
      idx <- which( d$shade_cent==s ) 
      plot( d$water_cent[idx] , d$blooms_std[idx] , 
           xlim=c(-1,1) , ylim=c(-1,2) ,
           xlab="water" , ylab="blooms" , 
           pch=16 , col=rangi2 )
      mtext(paste0("S (prior)= ", s))
      prior <- extract.prior(mdl)
      mu <- link( m8.5 , data=data.frame( shade_cent=s , water_cent=-1:1 ), 
                 post=prior)
      abline(  h=0 ) ; abline( h=1 )
      for ( i in 1:35 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
      # Make 1st line red
      lines( -1:1, mu[1,], lwd=2, col="red" )
}

# Posterior predixtions (all together)
#

par(mfrow=c(2,3)) # 3 plots in 1 row
for( mdl in c(m8.4,m8.5) ) {
      for ( s in -1:1 ) {
            idx <- which( d$shade_cent==s )
            plot( d$water_cent[idx] , d$blooms_std[idx] , 
                 xlim=c(-1,1) , ylim=c(0,1) ,
                 xlab="water" , ylab="blooms" , 
                 pch=16 , col=rangi2 )
      mtext(paste0("S= ", s))
      mu <- link( mdl , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
      for ( i in 1:35 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
      # Make 1st line red
      lines( -1:1, mu[1,], lwd=2, col="red" )
      }
}










