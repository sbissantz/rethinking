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

# Multicollinearity -------------------------------------------------------

# Data simulation
#
# Number of individuals
N <- 1e2
set.seed(909)
# Simulate heights
height <- rnorm(N, 150, 20)
leg_prop <- runif(N, 0.4, 0.5)
MME <- replicate(2, rnorm(N,0,0.2))
leg_left <- height * leg_prop + MME[1] 
leg_right <- height * leg_prop + MME[2] 
d <- data.frame(height, leg_left, leg_right)
# Expectation
mean(height) / (mean(leg_prop)*mean(height))

# Wrangling
#
d$H <- scale(d$height)
d$R <- scale(d$leg_right)
d$L <- scale(d$leg_left)

# Model sketch
#
# H_i ~ nromal(mu_i, sigma)
# mu_i = alpha + beta_L*L + beta_R*R
# alpha ~ normal(0, 0.2)
# beta_L ~ normal(0, 0.5)
# beta_R ~ normal(0, 0.5)

# Reduction
#
dat_ls <- list(N=nrow(d), H=as.numeric(d$H), R=as.numeric(d$R), L=as.numeric(d$L))

# Fitting 
#
file <- file.path(getwd(), "stan", "mdl_61.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format="data.frame")

# Posterior correlation
#
round(cor(samples[,3:4]), digits=2)
# Note: rho=.99 ; parameters do not act independently on the mean!
plot(samples$beta_L, samples$beta_R, pch=20)

# Visualize inference
#
plot(density(samples$beta_L))
lines(density(samples$beta_R), lty=2)

# Calculate thei sum
# since L=R=X, H=alpha + (beta_L + beta_R)*X
samples$beta_LR <- samples$beta_L + samples$beta_R
plot(density(samples$beta_LR), main="beta_L + beta_R")

#
# Adaption (Multicolli)
#

# Reduction
#
dat_ls <- list(N=nrow(d), H=as.numeric(d$H), L=as.numeric(d$L))

# Fitting 
#
file <- file.path(getwd(), "stan", "mdl_62.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format="data.frame")
plot(density(samples$beta_L), main="beta_L + beta_R")
