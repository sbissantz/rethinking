#
# Adventure in Covariance
#

# 14.1 Varying slopes by construction
#

# Simulate the population
# (Data Generating Process)
#
# Average morging wait time (min)
a <- 3.5
# Average diverence afternoon wait time (min)
b <- (-1) 
# Standard deviation in intercepts 
sigma_a <- 1
# Standard deviation in slopes
sigma_b <- 0.5
# Correlation between intercepts and slopes
rho <- (-0.7)
# Population: 2D-Gaussian (means, variances, covariances)
#
# Mean Vector 
Mu <- c(a,b)
# Covariance Matrix
cov_ab <- sigma_a * sigma_b * rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), nrow = 2, ncol = 2)
# Alternative
sigmas <- c(sigma_a, sigma_b)
Rho <- matrix(c(1, rho, rho, 1), nrow = 2)
# Matrix multiply
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
# Number of cafes
N <- 20
# Sample from MVN
set.seed(5)
vary_effects <- MASS::mvrnorm(N, Mu, Sigma)
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
#
# Visualize
plot(a_cafe, b_cafe, xlab = "Intercept", ylab = "Slope", pch = 16, col =
     "steelblue")
library(ellipse)
sequence <- c(0.1, 0.3, 0.5, 0.8, 0.99)
for (l in sequence) {
    lines(ellipse(Sigma, centre = Mu, level = l), col = "black")
}



