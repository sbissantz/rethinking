#
# Big Rntroy and the Generalized Linear Model
#
library(rethinking)

#
# 10.1 Maximum entropy
#

p <- list()
p$A <- c(0,0,10,0,0)
p$B <- c(0,1,8,1,0)
p$C <- c(0,2,6,2,0)
p$D <- c(1,2,4,2,1)
p$E <- c(2,2,2,2,2)

# Normalize
p_norm <- lapply( p, function(x) x/sum(x) )
# Information entropy
( H <- sapply( p_norm , function(q) -sum(ifelse(q==0,0,q*log(q))) ) )

ways <- c(1,90,1260,37800,113400)
logwayspp <- log(ways)/10

plot(logwayspp ~ H, pch = 21)

# build list of the candidate distributions
p <- list()
p[[1]] <- c(1/4,1/4,1/4,1/4)
p[[2]] <- c(2/6,1/6,1/6,2/6)
p[[3]] <- c(1/6,2/6,2/6,1/6)
p[[4]] <- c(1/8,4/8,2/8,1/8)
# compute expected value of each
sapply( p , function(p) sum(p*c(0,1,1,2)) )
# compute entropy of each distribution
sapply( p , function(p) -sum( p*log(p) ) )

p <- 0.7
( A <- c( (1-p)^2 , p*(1-p) , (1-p)*p , p^2 ) )
-sum( A*log(A) )

sim.p <- function(G=1.4) {
x123 <- runif(3)
x4 <- ( (G)*sum(x123)-x123[2]-x123[3] )/(2-G)
z <- sum( c(x123,x4) )
p <- c( x123 , x4 )/z
list( H=-sum( p*log(p) ) , p=p )
}

H <- replicate( 1e5 , sim.p(1.4) )
dens( as.numeric(H[1,]) , adj=0.1 )

entropies <- as.numeric(H[1,])
distributions <- H[2,]

max(entropies)

distributions[ which.max(entropies) ]


