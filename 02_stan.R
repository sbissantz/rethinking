
# Generative process
#
N <- 5
y <- rbinom(N, 1, .73)

# Data
#
dat_list <- list(
  y = y,
  N = length(y)
)

model.stan <- "
data{
  int<lower=0> N;
  int<lower=0, upper=1> y[N];
}
parameters{
  real<lower=0,upper=1> p;
}
model{
  y ~ bernoulli(p); // likelihood
  p ~ normal(0.6, 0.1); // prior
}
"
fit <- rstan::stan(model_code = model.stan, data = dat_list)
samples <- rstan::extract(fit)$p

# Point estimate ----------------------------------------------------------

# Posterior Mode (MAP)
#
rethinking::chainmode(samples)
# 
# barefoot
# Idea: generate the density and find the max value
dd <- density(samples)      
# x: data from which the estimate is to be computed
# y: the estimated desity values (>=0)
dd$x[which.max((dd$y))]

# Posterior median
#
mean(samples)

# Posterior mean
#
median(samples)

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

# Helper Functions

abs_loss <- function(p){
  d <- seq(0,1, length.out=length(p))
  sapply(d, function(d) sum(abs(d - p)/sum(p)))
}
plot_abs_loss <- function(loss){
  d <- seq(0,1, length.out=length(loss))
  y <- min(loss) ; x <- d[which.min(loss)] 
  plot(d, loss, type="l") ; points(x,y, pch=20)
  text(x, y+0.1, paste0("(",round(x, digits = 2),",",round(y, digits = 2),")"))
  abline(v=median(samples), lty=2)
  abline(v=mean(samples), lty=3)
  legend("bottomleft", legend = c("median", "mean"), lty = c(2,3))
}

loss <- abs_loss(samples)
# Check: vale = median
plot_abs_loss(loss)

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

# Helper Functions

quad_loss <- function(p){
  d <- seq(0,1, length.out=length(p))
  sapply(d, function(d) sum((d - p)^2/sum(p)))
}
plot_quad_loss <- function(loss, samples){
  d <- seq(0,1, length.out=length(loss))
  y <- min(loss) ; x <- d[which.min(loss)] 
  plot(d, loss, type="l") ; points(x,y, pch=20)
  text(x, y+0.1, paste0("(",round(x, digits = 2),",",round(y, digits = 2),")"))
  abline(v=median(samples), lty=2)
  abline(v=mean(samples), lty=3)
  legend("bottomleft", legend = c("median", "mean"), lty = c(2,3))
}

# Analytical solution
dbinom( 0:2, size=2, prob=0.7)
# 9% chance to observe w = 0
# 42% chance to observe w = 1
# 49% chance to observe w = 2

# Simulated data
N <- 1e4
trials <- 2 # globe tosses
(dummy_w <- rbinom(N, trials, prob = 70/100))
table(dummy_w) / N
# Compare to analytical solution
dbinom( 0:2, size=2, prob=0.7)

# Simulated data
N <- 1e4
tosses <- 10 # globe tosses
(dummy_w <- rbinom(N, tosses, prob = 70/100))

# Sums)
table(dummy_w)
# Percentage
table(dummy_w) / N
# Simple histogram
plot(table(dummy_w), xlab="Frequencies (x/10)", ylab = "Dummy water count")
# 7/10 guess for 70/100... pretty awesome!

# Sampling distributions
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
x <- 1:11
N <- 1e6
(y <- table(rbinom(N, size = 10, prob = 0.7)) / N)
points(x, y, pch=3)

#
# Understanding the Posterior Predictive Distribution
# https://stackoverflow.com/questions/42926683/how-does-prob-argument-in-rbinom-work-when-prob-is-a-vector
#

# Posterior distribution
#
plot(density(samples))

# A sampling distribution
# p = 0.6
w <- rbinom(1e4, size=9, prob = .6)

# Sampling distributions
#
p_range <- seq(0.1,0.9,by=0.1)
N <- 1e4
dummy_w <- vapply(p_range, function(p) rbinom(N, size=9, prob=p ), 
                  FUN.VALUE = numeric(N))

op <- par(no.readonly = TRUE)
par(mfrow = c(3,3))
  lapply(1:length(p_range), function(p) plot(table(dummy_w[,p]), 
                                             main=paste0(p_range[p])))
par(op)

# Sampling distribution 
# ... posterior mean
# predictive distribution -- posterior mean
# 
w_pm <- rbinom(1e4, size=9, prob = mean(samples))
hist(w_pm)

# TODO: Do it with a CI!
#
#

# Posterior predictive distribution
# ... all samples from posterior
# Note: If the 'prob' argument is a vector it gets recyceled
# Why does it work? because every sample is a conjecture about p. So if we
# sample sequences of globe tosses where the probability to suceed in each try
# is a conjecture
#
w_ppd <- rbinom(1e4, size=9, prob = samples)
hist(w_ppd, main="Posterior Predictive Distribution", 
     xlab="number of water samples") 

# Overconfidence tran
# Comparing: PPD vs. PD -- Posterior mean
#
hist(w_pm)
hist(w_ppd, add = TRUE, col="black", xlab="number of water samples")
# Illusion: model seems more consistent with the data then it really it!
