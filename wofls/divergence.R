#
# Entropy, Cross Entropy & KL divergence
#

# Information entropy
#
H <- function(p) {
    -sum(p * log(p))
}
p <- c(0.5, 0.5)
# Shannon entropy
H(p)

# KL-divergence
#
D_KL <- function(p, q) {
    sum(p * log(p/q))
}
p <- c(0.5, 0.5) ; q <- c(0.4, 0.6)
D_KL(p,q)

# Cross entropy
#
H2 <- function(p, q) {
  -sum(p*log(q))
}
# Cross entropy
H2(p,q) 

# Test
#
# cross entropy - shanonen entropy = KL divergence
all.equal(H2(p,q) - H(p), D_KL(p,q))
# [1] TRUE

# Log-probability score
# 
S <- function(q) sum(log(q))
# Log-probability score
S(q)


