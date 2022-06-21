#
# Chapter 10    
# 

# Maximum entropy
#

#List of pebbles
p <- list(A=c(0,0,10,0,0),
          B=c(0,1,8,1,0),
          C=c(0,2,6,2,0),
          D=c(1,2,4,2,1),
          E=c(2,2,2,2,2))

# Normalize the pebbles
p_norm <- lapply(p, function(q) q/sum(q)) 

# Compute entropy (using L'Hopital's rule)
calc_H <- function(q)  -sum(ifelse(q==0, 0, q*log(q)))
H <- sapply(p_norm, calc_H)

ways <- c(1,90,1260, 37800, 113400)
logwayspp <- log(ways)/10

# Binomial maxent
#
p <- list()
p[[1]] <- c(1/4,1/4,1/4,1/4)
p[[2]] <- c(2/6,1/6,1/6,2/6)
p[[3]] <- c(1/6,2/6,2/6,1/6)
p[[4]] <- c(1/8,4/8,2/8,1/8)

# Compute E
#
sapply(p, function(p) sum(p*c(0,1,1,2)))

# Compute the entropy
#
sapply(p, calc_H)

# Constraint E must be 1.4
#
p <- 0.7
(A <- c((1-p)^2, p*(1-p), (1-p)*p, p^2))
# Entropy
calc_H(A)

# Simulate different function with E=1.4
#
sim.p <- function(G=1.4) {
    x123 <- runif(3)
    x4 <- ((G)*sum(x123)-x123[2]-x123[3])/(2-G)
    z <- sum(c(x123,x4))
    p <- c(x123, x4)/z
    list(H=-sum(p*log(p)),p=p)
}
H <- replicate(1e5, sim.p(1.4))
rethinking::dens(as.numeric(H[1,]),adj=0.1)

entropies <- as.numeric(H[1,])
distributions <- H[2,]
max(entropies)
distributions[which.max(entropies)]
# [[1]]
# [1] 0.09011521 0.21020938 0.20956020 0.49011521
# 

