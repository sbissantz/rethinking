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

# 6.1 Multicollinearity ---------------------------------------------------

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

# Multicollinear milk -----------------------------------------------------

library(rethinking)
data(milk)
d <- milk
str(d)
d$K <- scale(d$kcal.per.g)
d$F <- scale(d$perc.fat)
d$L <- scale(d$perc.lactose)

pairs(data.frame(d$kcal.per.g, d$perc.lactose, d$perc.fat))

# Model sketch
#
# K_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_F*F + beta_L*L
# alpha ~ normal(0, 0.2)
# beta_F ~ normal(0, 0.5)
# beta_L ~ normal(0, 0.5)
# sigma ~ exponential(1)

# Reduction
#
dat_ls <- list(N=nrow(d), K=as.numeric(d$K), F=as.numeric(d$F), 
               L=as.numeric(d$L))

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_63.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format = "data.frame")
# Hairy catterpillar occular inspection test
bayesplot::mcmc_trace(samples)

# Posterior correlations
#
rho_beta <- cor(samples$beta_F, samples$beta_L)
# High conditional associations
# Violation: ...parameters do not act independently on the mean!
plot(samples$beta_F, samples$beta_L, pch=20, col=scales::alpha("steelblue", .3))

# Visualization
#
beta_F_d <- density(samples$beta_F)
beta_L_d <- density(samples$beta_L)
plot(NULL, xlim=c(-2,2), ylim=c(0,2)) 
lines(beta_F_d, lty=2)
lines(beta_L_d)

# Excluding one predictor
#
library(rethinking)
data(milk)
d <- milk
str(d)
d$K <- scale(d$kcal.per.g)
d$F <- scale(d$perc.fat)

# Reduction
#
dat_ls <- list(N=nrow(d), K=as.numeric(d$K), F=as.numeric(d$F))  

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_64.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format = "data.frame")
# Hairy catterpillar occular inspection test
bayesplot::mcmc_trace(samples)

# Visualization
#
beta_F_d <- density(samples$beta_F)
plot(beta_F_d) 
lines(beta_F_d, lty=2)

# Note: since beta_F and beta_L jointly influence the mean, their sum(1) es 
# approximately equal to the estima of beta_L in the second model.




