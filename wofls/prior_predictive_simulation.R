#
# Prior predictive simulation
#

# Note: I found it most useful to do PPS in R. But you can also do it in Stan.
#
N <- 1e2 ; weight <- rnorm(N) ; height <- rnorm(N, weight)
d <- data.frame(weight, height)

#
# Posterior line predictive simulation
#

# Standardized
#
N <- 1e2
# alpha prior
a <- rnorm(N, 0, 0.2)
# beta prior
b <- rnorm(N, 0, 0.5)
# value ranges
x <- d$weight ; y <- d$height

# Visualize
#
plot(NULL, xlim=c(-3,3), ylim=c(-3, 3), 
     xlab="Weight (std)", ylab="Height (std)")
# High probability region
abline(h=c(-2,2), lty=2)
# 50 prior lines for mu
for(i in seq(50)) abline(a[i], b[i], col=scales::alpha("steelblue", .8))



for(i in 1:N) {
  curve(a[i] + b[i]*x, from=min(d2$weight)[1], to=max(d2$weight), 
        add=TRUE, col=col.alpha("black", .2)) 
}




# Joint prior model
#
N <- 1e2
# alpha prior
a <- rnorm(N, 178, 20)
# beta prior
b <- rnorm(N, 0, 1)
# value range
x <- d2$weight ; y <- d2$height
xbar <- mean(d2$weight) 
plot(NULL, xlim=range(x), ylim=c(-100, 400),
     xlab="Weight", ylab="Height")
abline(h=c(0,272), lty = 2)
for(i in 1:N) {
  curve(a[i] + b[i]*(x-xbar), from=min(d2$weight)[1], to=max(d2$weight), 
        add=TRUE, col=col.alpha("black", .2)) 
}

N <- 1e3
b <- rnorm(N,0,.5)
plot(c(-4,4), c(-4,4), type="n")
for(i in seq(100)) abline(a=0, b[i])
