# Spline ----------------------------------------------------------------------

library(rethinking)
data("cherry_blossoms")
d <- cherry_blossoms
rethinking::precis(d)
# Complete case analysis
dcc <- d[complete.cases(d$doy),]
N_knots <- 15
probs <- seq(0,1, length.out=N_knots)
knots <- quantile(dcc$year, probs)

# B-Spline basis matrix (B)
#
B <- splines::bs(dcc$year, knots = knots[-c(1,N_knots)], degree = 3,
                 intercept = TRUE)
round(B, digits=2)
# Visualize (B)
#
plot(NULL, xlim = range(dcc$year), ylim = c(0,1),
     xlab="year", ylab="basis value" )
for(i in 1:ncol(B)) lines(dcc$year, B[,i], lwd=2)
# for(i in 1:ncol(B)) points(dcc$year, B[,i], type="l", lwd=2)
# Show the knots
text("+", x=knots, y=1)

dat_ls <- list(
  N = nrow(dcc),
  K = ncol(B),
  B = B,
  D = dcc$doy
)

# model -------------------------------------------------------------------

file <- file.path(getwd(), "stan", "mdl46.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls, seed = 123)

fit$cmdstan_diagnose()
samples <- fit$draws(format = "df")
# bayesplot::mcmc_trace(samples)
fit$print(max_rows = 100)
bayesplot::mcmc_trace(samples)
# Seems to be approx. equal to
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

w <- fit$draws("w", format = "matrix")
w_means <- colMeans(w)
mu <- fit$draws("mu", format = "matrix")

# Visualize (B%*%w)
#
plot(NULL, xlim=range(dcc$year), ylim=c(-6,6), xlab="year", ylab="basis*weight")
for ( i in 1:ncol(B) ) lines( dcc$year, w_means[i] * B[,i], lwd=3, col="black")

# Posterior line predictions
#
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)

# Visualize
#
plot( dcc$year, dcc$doy, xlab="year", ylab="Day in year",
      col=col.alpha(rangi2, 0.3), pch=16 )
lines(dcc$year, mu_mean, lwd=1.5)
rethinking::shade(mu_HPDI, dcc$year, col=col.alpha("black", 0.5))

