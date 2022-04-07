
#
# Geocentric models
#
#

library(rethinking) ; data(Howell1) ; d <- Howell1 ; d2 <- d[ d$age >= 18, ]

#
# Dataset and variable inspections
#

# dataset
precis( d2 )

# Y variable
dens( d2$height )

#
# Prior predictive simulation (single variable)
# hi ~ Normal(mu, sigma) 
#

set.seed(2971)
# Inspect single prior: mu ~ Normal(178, 20) 
curve(dnorm(x, 178, 20), from=100, to=250)
# Inspect: single prior: sigma ~ Uniform(0, 50)
curve(dunif(x, 0, 50), from=10, to=60)

# Joint prior: What does the model think before seeing the data?
N <- 1e4
# Simulate single prior: mu ~ Normal(178, 20) 
mu_smpl <- rnorm( N, 178, 20 ) 
# Simulate single prior: sigma ~ Uniform(0, 50)
sigma_smpl <- runif(N, 0, 50)
# Simulate joint prior: hi  ~ Normal( mu, sigma )
h_prior <- rnorm(N, mu_smpl, sigma_smpl)

# Plot: the joint prior distribution 
dens(h_prior)
    abline(v = mean(mu_smpl) + 2 * mean(sigma_smpl))
    abline(v = mean(mu_smpl) - 2 * mean(sigma_smpl))

# Summarize the result: two point summary -- PI
PI(h_prior, prob = .5)
#      25%      75% 
# 156.5737 198.3794 

#
# Posterior construction (quadratic approx)
#

# Set start values to be more efficiently
start_val <- list(
             mu=mean(d2$height),
             sigma=sd(d2$height)
)


# Formalize the model: hi ~ Normal(mu, sigma)
f4.1 <- alist(
             height ~ dnorm( mu, sigma ),
             mu ~ dnorm( 178, 20 ),
             sigma ~ dunif( 0,50 )
) 
# Fit the model to the data & summarize
m4.1 <- quap( f4.1, data=d2, start=start_val ) ; precis( m4.1 )
#             mean        sd       5.5%     94.5%
# mu    154.654599 0.4172459 153.987760 155.32144
# sigma   7.762898 0.2951341   7.291217   8.23458

#
# Posterior sampling 
#

# Extract samples from the joint posterior of mu & sigma
post <- extract.samples(m4.1) ; precis( post )
# post <- MASS::mvrnorm( 1e4, mu=coef(m4.1), Sigma=(vcov(4.1)) )

#
# Communication
#

# Plot marginal posterior & 89% CI: mu
dens( post$mu ) ; abline( v=PI( post$mu ))
# Plot marginal posterior & 89% CI: sigma
dens( sigma_smpl ) ; abline( v=PI( sigma_smpl ))
# Look @ correlation between mu & sigma
cov2cor( vcov( m4.1 ) )
#                mu       sigma
# mu    1.000000000 0.001946345
# sigma 0.001946345 1.000000000

#
# Expand the model: Add a predictor 
#

# Plot height = f(weight)
plot( d2$height ~ d2$weight )

#
# Prior predictive simulation
#

set.seed(2971)
N <- 100                               # 100 Prior lines 
a <- rnorm( N, 178, 20 )               # a ~ Normal(178,20)
b <- rnorm( N, 0, 10 )                 # b ~ Normal(0,10) 

# Plot the joint prior 
plot( NULL, xlim=range(d2$weight) , ylim=c(-100,400))
# Mark the possible outcome space
abline( h=0, lty=2 ) ; abline( h=272, lty=1, lwd=.5 )
mtext("b ~ dnorm(0,10)")
# Attach the lines
xbar <- mean(d2$weight)
for ( i in 1:N ) {
    curve(a[i] + b[i]*(x-xbar), from=min(d2$weight), to=max(d2$height), 
          add=TRUE, col=col.alpha("black", .2))
}                                      # Bad model (outcome space) 

# Consider lognormal for b
# Prior Knowlege: b>0 
b <- rlnorm( 1e4, 0, 1 )               # b ~ Lognormal(0,1) 
dens( b, xlim=c(0,5), adj = .1 )

# Plot the joint prior 
plot( NULL, xlim=range(d2$weight) , ylim=c(-100,400), 
     xlab="weight", ylab="height")
# Marke the possible outcome space
abline( h=0, lty=2 ) ; abline( h=272, lty=1, lwd=.5 )
mtext("b ~ dlnorm(0,1)")
# Attach the lines
xbar <- mean(d2$weight)
for ( i in 1:N ) {
    curve( a[i] + b[i]*(x-xbar), from=min(d2$weight), to=max(d2$height), 
          add=TRUE, col=col.alpha("black", .2))
}

#
# Posterior construction
#

# Constuct the mean: center alpha
xbar <- mean(d2$weight)
# Formalize the linear model: mu = a+ b*x
f4.3 <- alist(
              height ~ dnorm( mu, sigma ),
              mu <- a + b * ( weight - xbar ),
              a ~ dnorm( 178, 20 ),
              b ~ dlnorm( 0, 1 ),
              sigma <- dunif( 0, 50 )
)
# Fit the liear model to the data
m4.3 <- quap( f4.3, data=d2 )
precis(m4.3)

#
# Posterior sampling
#

post <- extract.samples( m4.3 )
# post <- MASS::mvrnorm( 20, mu=coef(mN), Sigma=vcov(mN) )

#
# Communication
#

# Generate a seq.of weights to make predictions for
weight_seq <- seq( from=25, to=70, by=1 )

# 2. Predict the mean fpr the 46 unique weights
mu <- link( m4.3, data=list(weight=weight_seq ) )
# mu_link <- function( x ){ post[ ,"a" ] + post[ ,"b" ] * ( x - xbar ) }
# mu <- sapply(weight_seq, mu_link)

# Posterior line prediction

# Calculate the MAP for every sample
mu.mean <- apply( mu, 2, mean )
# Calcuate the compatibility interval
mu_CI <- apply(mu, 2, rethinking::PI, prob=.89)  # := Combatibility interval
# mu_HPDI <- apply(mu, 2, rethinking::HPDI, prob=.89) ???

# Posterior predictions

# Sample real values of heights for predictions 
# ...smooth the boundaries with a large n 
sim_height <- sim( m4.3, data=list(weight=weight_seq), n=1e4 )
# siml <- function( x ) { rnorm( n=nrow(post), 
#                               mean=post[,"a"] + post[,"b"] * (x - xbar), 
#                               sd=post[,"sigma"]) }
# sim_height <- apply(post, 2, siml)

# Instead of mean values sample real values of height 
height_PI <- apply(sim_height, 2, rethinking::PI, prob=.89)

# Plot weights against heights
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
# MAP line
lines( weight_seq, mu_mean )
# mu's distribution ~ Posterior line predictive untertainty
# CI/HPDI region for line 
shade( mu_CI, weight_seq )             # shade( mu_HPDI, weight_seq )
# predicted y's  distribution -- Posterior predictive uncertainty
# PI region for heights (Sampling variation)
shade( height_PI, weight_seq ) 

#
#
# Polynomial regression
#
#

library(rethinking) ; data(Howell1) ; d <- Howell1

# Plot the expected relation
plot( height ~ weight, data=d )

# Cubic polynomial (same with parabolic polynomials)

# Standardize (to make estimation more efficient ~ outcomespace^2,3...)
weight_s <- (d$weight - mean(d$weight)) / sd(d$weight)
weight_s2 <- d$weight_s^2 ; weight_s3 <- d$weight_s^3

# formalize the model
f4.6 <- alist(
              height ~ dnorm( mu, sigma ),  # heights ~ Normal(mu,sigma) 
                mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3,
                    a ~ dnorm( 178, 20 ), # alpha ~ Normal(170,20) 
                    b1 ~ dlnorm( 0, 1 ),  # beta1 ~ Log-Normal(0,1) 
                    b2 ~ dnorm( 0, 1 ), # beta2 ~ Normal(9,1) 
                    b3 ~ dnorm( 0, 1 ), # beta3 ~ Normal(9,1) 
                sigma ~ dunif( 0, 50 ) # sigma ~ Uniform(9,50)
)
m4.6 <- quap(f4.6, data=d)

weight.seq <- seq( from=-2.2, to=2, length.out=30 )
# Make a list of inputs for the prediction
pred_dat <- list( weight_s=weight.seq, weight_s2=weight.seq^2, weight_s3 =
                 weight.seq^3 )
# Make predictions from the parabolic model
mu <- link( m4.6, data = pred_dat )
# Generate the mu mean
mu.mean <- apply( mu, 2, mean )
# Generate a predictive interval for mu
mu.PI <- apply( mu, 2, PI, prob=.89 )
# simulate height data
sim.height <- sim( m4.6, data=pred_dat )
# Generate the Predictive intervals for height
height.PI <- apply( sim.height, 2, PI )

# Plot the model on the ustandardized scale
plot(height ~ weight_s, data=d, col=col.alpha(rangi2, .5), xaxt="n")  
at <- seq(-2, 2, 1 )
labels <- (at * sd( d$weight ) + mean( d$weight ))
    axis( side=1, at=at, labels=round( labels,1 ) )
    # MAP line
    lines( weight.seq, mu.mean )
    # Distribution of mu
    shade( mu.PI, weight.seq )
    # Sampling variation
    shade( height.PI, weight.seq)


#
#
# Splines
#
#

library(rethinking) ; data(cherry_blossoms) ; d <- cherry_blossoms
precis(d) ; plot(doy ~ year, data=d)
d2 <- d[ complete.cases(d$doy), ]
num_knots <- 15
# Probs: evenly spaced quaniles
knot_list <- quantile( d2$year, probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)], 
        degree=3, intercept=TRUE)

# plot( NULL, xlim=range(d2$year), ylim=c(0,1), xlav="year", ylab="basis" )
# for( i in 1:ncol(B) ) lines( d2$year, B[,i] )

f4.7 <- alist(
              D ~ dnorm(mu, sigma),    # Data prior: Normal
                mu <- a + B %*% w,       # mu as a linear function 
                    a ~ dnorm(100,10),
                    w ~ dnorm(0,10),
                sigma ~ dexp(1)
)
m4.7 <- quap( f4.7, data=list( D=d2$doy, B=B ),
start=list( w=rep( 0, ncol(B) ) ))

post <- extract.samples( m4.7 )
w <- apply( post$w, 2, mean )
# plot( NULL, xlim=range(d2$year), ylim=c(-6,6), xlab="year", ylab="basis*weight")
# for( i in 1:ncol(B) ) lines( d2$year, w[i]*B[,i] )

mu <- link( m4.7 )
mu_PI <- apply( mu, 2, PI, 0.97 )
plot( d2$year, d2$doy, col=col.alpha(rangi2, .3), pch=16 )
shade( mu_PI, d2$year, col=col.alpha("black", .5) )

mu <- dnorm(0,10)
sig <- dexp(1)
rnorm(mu, sig)

