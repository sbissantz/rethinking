
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# Generative process ------------------------------------------------------

# Generative process
N <- 100
y <- rbinom(N, 1, prob=.73)

# Model fitting -----------------------------------------------------------

# data list
dat_ls <- list(
               y = y,
               N = N
               )

model.stan <- "
data{
    int<lower=0> N;
    int<lower=0, upper=1> y[N];
}
parameters{
    real<lower=0, upper=1> p;
}
model{
    y ~ bernoulli(p);
    p ~ normal(.6, 0.1);
}"
fit <- rstan::stan(model_code = model.stan, data = dat_ls)

# Inspection & Extraction -------------------------------------------------

print(fit)
pairs(fit)
rstan::traceplot(fit)

samples <- rstan::extract(fit)$p
# Sampling to summarize ---------------------------------------------------
# 3.2

#
# Intervals of defined boundaries
#

N_samples <- length(samples)
# one boundary
sum(samples < 0.722)/N_samples
# two boundaries
sum(samples > 0.71 & samples < 0.73 )/N_samples

# Graph: Interval of defined boundaries
#
# Visualize the Posterior
dsamples <- density(samples)
plot(dsamples, main=NA)
# Add boundary
boundary <- 0.722
abline(v = boundary, lty=2)
# Add boundaries
boundaries <- c(0.71, 0.73)
abline(v = boundaries, lty=6)

#
# CI
# Intervals of defined mass
#
mass <- 0.5
quantile( samples, mass )

# Percentile Interval
#
masses <- c(0.7, 0.8)
quantile( samples, masses )

# Graph: Interval of defined mass 
#
# Visualize the Posterior
dsamples <- density(samples)
plot(dsamples, main=NA)
# Add mass 
mass <- 0.5
qval <- quantile( samples, mass )
abline(v = qval, lty=2)
# Add masses 
masses <- c(0.7, 0.8) #PI
qvals <- quantile( samples, masses )
abline(v = qvals, lty=6)

#
#
# PI
# Percentile interval
#
mass <- .9
rethinking::PI(samples, prob=mass)

#
# HPDI
# Highest posterior density interval
#

# For S3 class objects: corece
rethinking::HPDI( as.vector(samples), prob=.9 )

# Graph: HPDI 
#
dsamples <- density(samples)
plot(dsamples, main=NA)
qvals <- rethinking::HPDI( as.vector(samples), prob=.9 )
abline(v=qvals, lty=2)

# Point estimate ----------------------------------------------------------

# Posterior Mode (MAP)
#
rethinking::chainmode(samples)
# 
# barefoot
# Idea: generate the density and find the max value
dsamples <- density(samples)      
# x: data from which the estimate is to be computed
# y: the estimated desity values (>=0)
dsamples$x[which.max((dsamples$y))]

# Posterior median
#
mean(samples)

# Posterior mean
#
median(samples)

# Visualize 
#
dsamples <- density(samples)
plot(dsamples, main=NA)
  map <- rethinking::chainmode(samples)
abline(v=map, lty=2)
  pmean <- mean(samples)
abline(v=pmean, lty=4)
  pmedian <- median(samples)
abline(v=pmedian, lty=6)

# Loss function 
#

# Absolute Loss 
# ..with STAN samples

d <- seq(0,1, length.out=length(samples))
(loss <- sapply(d, function(d) sum(abs(d - samples)/sum(samples))))
plot(d, loss, type="l")
y <- min(loss) ; x <- d[which.min(loss)] 
points(x,y, pch=20)
text(x, y+0.1, paste0("(",round(x, digits = 2),",",round(y, digits = 2),")"))

# Quadratic loss 
# ..with STAN samples

d <- seq(0,1, length.out=length(samples))
(loss <- sapply(d, function(d) sum((d - samples)^2/sum(samples))))
plot(d, loss, type="l")
y <- min(loss) ; x <- d[which.min(loss)] 
points(x,y, pch=20)
text(x, y+0.1, paste0("(",round(x, digits = 2),",",round(y, digits = 2),")"))
abline(v=median(samples), lty=2)
abline(v=mean(samples), lty=3)

# Sampling to simulate predictions ----------------------------------------
# 3.3

# Analytical solution
#
dbinom( 0:2, size=2, prob=0.7)
# 9% chance to observe w = 0
# 42% chance to observe w = 1
# 49% chance to observe w = 2

# Simulated data 
#
N <- 1e4
tosses <- 9 # globe tosses
dummy_w <- rbinom(N, tosses, prob = 70/100)
# Equal to `dbinom( 0:2, size=2, prob=0.7)`?
table(dummy_w) / N

# Visualize
#
# Percentage
table(dummy_w) / N

# Frequency table 
ftbl_w <- table(dummy_w) 
plot(ftbl_w, xlab="Dummy water Count (x/10)", ylab = "Frequency")
# 7/10 guess for 70/100... pretty awesome!

#
# Understanding sampling distributions 
#

seq <- seq(-5, 5, length.out=100)
# Density function
plot(dnorm(seq, mean = 0, sd = 1))
# Cumulative distribution function
plot(pnorm(seq, mean = 0, sd = 1))

plot(dbinom(0:12, size=10, prob=0.7),
     xlab = "Number of water samples: x/10 tosses",
     ylab = "Probability to observe n/10", xaxt = 'n')
axis(side = 1, at=1:13, labels = 0:12)
abline(v=11.5, lty=2)
text(12.5, 0.1, "Impossible")
  N <- 1e6 ; tosses <- 10
  dummy_w <- rbinom(N, tosses, prob = 0.7)
  x <- 1:11 ; y <- table(dummy) / N
points(x, y, pch=3)
# Awesome...

#
# Understanding the Posterior Predictive Distribution
#

# Posterior distribution
#
dsamples <- density(samples)
plot(dsamples, main=NA)

# A sampling distribution 
p <- 0.6
w <- rbinom(1e4, size=9, prob = p)

# Sampling distributions
#
p_range <- seq(0.1,0.9,by=0.1)
N <- 1e4
gen_smpl_dstrb <- function(p, N) rbinom(N, size=9, prob=p )
dummy_w <- vapply(p_range, gen_smpl_dstrb, N=N, FUN.VALUE = numeric(N))

# Visualize sampling distributions
#
simpl_hist <- function(p) {
   plot(table(dummy_w[,p]), main=paste0(p_range[p]))
  
}
op <- par(no.readonly = TRUE)
par(mfrow = c(3,3))
  lapply(1:length(p_range), simpl_hist)
par(op)

# Sampling distribution 
# ...for the posterior mean
# predictive distribution -- posterior mean
# 
w_pm <- rbinom(1e4, size=9, prob = mean(samples))
hist(w_pm)

# TODO: Do it with a CI!

#
# PPD
# Posterior predictive distribution
# ...all samples from posterior

# Note: If the 'prob' argument is a vector it gets recycled Why does it work?
# because every sample is a conjecture about p. So if we sample sequences of
# globe tosses where the probability to succeed in each try is a conjecture

w_ppd <- rbinom(1e4, size=9, prob = samples)
hist(w_ppd, main="Posterior Predictive Distribution", 
     xlab="number of water samples") 

# Overconfidence tran
# Comparing: PPD vs. PD -- Posterior mean
#
hist(w_pm)
hist(w_ppd, add = TRUE, col="black", xlab="number of water samples")
# Illusion: model seems more consistent with the data then it really it!




