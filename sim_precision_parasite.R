#
# Precision parasite
#

# Functional relationships
#
# Z: indep.
# X: f(Z)
# Y: f(X)

f <- function(N=100, b_ZX=1, b_XY=1) { 
    Z <- rnorm(N) 
    X <- rnorm(N, b_ZX*Z)
    Y <- rnorm(N, b_XY*X)
    b_X <- coef( lm(Y ~ X) )['X']
    b_XZ <- coef( lm(Y ~ X+Z) )['X']
    c(b_X, b_XZ)
}
sim <- mcreplicate::mc_replicate(1e4, f(), mc.cores=4)

plot(density(sim[1,]), lwd=3, xlab="Posterior mean", xlim=c(0.5,1.5))
lines(density( sim[2,]), lwd=3, col="red")
text(x=1.25, y=4, "correct", col="black") 
text(x=0.75, y=4, "wrong", col="red")

