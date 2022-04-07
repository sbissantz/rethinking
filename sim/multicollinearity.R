# Sim multicolli
#

# Cholesky method
#
# Random variable 
z <- rnorm(N)
# Correlation: x & z 
rho <- .9
# Cholesky
x <- rnorm(N, mean=z*rho, sd=sqrt(1-rho^2) * var(z))

# MVrnorm
#
M <- 3
mu <- c(1,1,1)
# Kovarianzmatrix
Sigma <- matrix(c(1.0, 0.8, 0.9, 
                  0.8, 1.0, 0.8,
                  0.9, 0.8, 1.0), 
                M,M)
N <- 1e4
X <- data.frame(abs(MASS::mvrnorm(N, mu, Sigma)))
fit <- lm(X$X1 ~ X$X2 + X$X3)


library(rethinking)
data(milk)
d <- milk

sim.coll <- function(r=.9) {
    d$x <- rnorm( nrow(d), mean=r*d$perc.fat,
    sd=sqrt( (1-r^2) * var(d$perc.fat) ))
    m <- lm( d$kcal.per.g ~ d$perc.fat + d$x)
    sqrt( diag(vcov(m))[2] )
}
rep.sim.coll <- function(r=0.9, n=100) {
    stddev <- replicate(n, sim.coll(r))
    mean(stddev)
}
r.seq <- seq(from = 0, to = 0.99, by=0.01)
stddev <- sapply( r.seq, function(z) rep.sim.coll(r=z, n=100) )
plot(stddev ~ r.seq, type="l", col=rangi2, lwd=2, xlab="correlation")
