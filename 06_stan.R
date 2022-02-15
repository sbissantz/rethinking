# Simulating selection distortion
# Pre-collider Bias
#
set.seed(1914)

# Number of proposals (e.g., each year in NATURE)
N <- 1e3

# Dimensions
# Trustwortyness & Newsworthiness
# (Note: uncorrelated)
T <- rnorm(N) ; N <- rnorm(N)
# Combined Study Score 
# (i.e., performance on both dimensions)
S <- T + N

# Selection process
#
# Acceptance rate 
# (..high performers only: 10%)
p <- 0.1 
q <- quantile(S, 1-p)
# Selection criterion & selection process
selected <- ifelse(S >= q, TRUE, FALSE)

# Associations
#
# Pre-selection correlation
rho <- cor(N, T)  
# Post-selection (selection distortion)
rho_selected <- cor(N[selected], T[selected])  
# Conditional on being in the sample, there is a negative association between
# T and N!

# Visuzalization
# 
plot(N, T, pch=20, col=ifelse(S >= q, "steelblue", "black"))
abline(a=0, b = rho, lwd=2, lty=3)
abline(a=2.5, b = rho_selected, lwd=2, lty=3)

