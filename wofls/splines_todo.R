#
# Basis Splines 
#

# Note: This is really just an adapted snippet from Richard McElreath's Book. I
# customized it to use it with cmdstanr (i.e., Stan). The tutorial really got
# me into splines. But if you wanto to dive deeper into generalized additive
# models (GAM), have a look at Simon N. Wood's "Generalized Additive Models --
# An Introduction with R."

library(rethinking)
data("cherry_blossoms")
d <- cherry_blossoms
# Complete case analysis
dcc <- d[complete.cases(d$doy),]

# In a nutshell
#
N_knots <- 15
probs <- seq(0,1, length.out=N_knots)
knots <- quantile(dcc$year, probs)
B <- splines::bs(dcc$year, knots = knots[-c(1,N_knots)], degree = 3,
                 intercept = TRUE)

dat_ls <- list(N = nrow(dcc), K = ncol(B), B = B, D = dcc$doy)

file <- file.path("spline.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls, seed = 123)

fit$cmdstan_diagnose()
samples <- fit$draws(format = "df")
fit$print(max_rows = 100)
# bayesplot::mcmc_trace(samples)

w <- fit$draws("w", format = "matrix")
w_means <- colMeans(w)
mu <- fit$draws("mu", format = "matrix")

mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)

par(mfrow=c(3,1))

plot(NULL, xlim = range(dcc$year), ylim = c(0,1),
     xlab="year", ylab="basis value" )
for(i in 1:ncol(B)) lines(dcc$year, B[,i], lwd=2)
text("+", x=knots, y=1)

plot(NULL, xlim=range(dcc$year), ylim=c(-8,8), xlab="year", 
     ylab="basis*weight")
for ( i in 1:ncol(B) ) lines( dcc$year, w_means[i] * B[,i], lwd=3, col="black")

plot(dcc$year, dcc$doy, xlab="year", ylab="Day in year",
      col=scales::alpha("steelblue", 0.3), pch=16)
lines(dcc$year, mu_mean, lwd=1.5)
rethinking::shade(mu_HPDI, dcc$year, col=scales::alpha("black", 0.5))

#
# Documented version
#

# Numer of knots
N_knots <- 15
# Note: determines the wigglyness of the spline

# Evenly space quantile values 
probs <- seq(0,1, length.out=N_knots)
knots <- quantile(dcc$year, probs)

# B-Spline basis matrix (B) 
# Note: cubic spline
B <- splines::bs(dcc$year, knots = knots[-c(1,N_knots)], degree=3, 
                 intercept=TRUE)
round(B, digits=2)

# Visualize the basis functions
# I.e.: Visiualization of the basis matrix (B)
plot(NULL, xlim = range(dcc$year), ylim = c(0,1),
     xlab="year", ylab="basis value" )
for(i in 1:ncol(B)) lines(dcc$year, B[,i], lwd=2)
# Note: Actualy we determining points, so actually we use ... in the plot:
# for(i in 1:ncol(B)) points(dcc$year, B[,i], type="l", lwd=2)
# Show the knots "+"
text("+", x=knots, y=1)

# Reduced data list
#
dat_ls <- list(
  N = nrow(dcc),
  K = ncol(B),
  B = B,
  D = dcc$doy
)

# Fit
#
file <- file.path("spline.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls, seed = 123)

# Diagnostics & samples
#
fit$cmdstan_diagnose()
samples <- fit$draws(format="data.frame")
# bayesplot::mcmc_trace(samples)
fit$print(max_rows=100)
bayesplot::mcmc_trace(samples)
# Note: Seems approx. equal to...
# m4.7 <- quap(
#   alist(
#     D ~ dnorm(mu, sigma),
#     mu <- a + B %*% w,
#     a ~ dnorm(100,10),
#     w ~ dnorm(0,10),
#     sigma ~ dexp(1)
#   ), data=list( D=dcc$doy, B = B),
#   start=list(w=rep(0, ncol(B) ) ) )
# precis(m4.7, depth = 2)

# Weight matrix
#
w <- fit$draws("w", format="matrix")
w_means <- colMeans(w)

# Linear predictor: mu = a + B %*% w,
mu <- fit$draws("mu", format="matrix")
# Note: I generated the linear predictor "mu" inside of Stan. You could also
# use mu <- B %*% w_means to continue.

# Visualize the linear predictor (B%*%w)
# i.e: the basis function weighted by its parameter
#
plot(NULL, xlim=range(dcc$year), ylim=c(-8,8), xlab="year", 
     ylab="basis*weight")
for (i in 1:ncol(B)) lines(dcc$year, w_means[i]*B[,i], lwd=3, col="black")

# Posterior line predictions
# a + B %*% w,
# Posterior mean 
mu_mean <- apply(mu, 2, mean)
# Highest posterior density interval 
mu_HPDI <- apply(mu, 2, rethinking::HPDI)

# Visualize the posterior line predictions from the fitted model
# I.e.: the sum of the weights basis functions
# 
plot(dcc$year, dcc$doy, xlab="year", ylab="Day in year",
      col=scales::alpha("steelblue", 0.3), pch=16 )
lines(dcc$year, mu_mean, lwd=1.5)
rethinking::shade(mu_HPDI, dcc$year, col=scales::alpha("black", 0.5))


