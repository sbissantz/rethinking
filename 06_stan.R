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

# M2 ----------------------------------------------------------------------

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
file <- file.path(getwd(), "stan", "6", "mdl_61.stan")
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

# Visualization
# 
pairs(data.frame(d$kcal.per.g, d$perc.lactose, d$perc.fat))

# Data wrangling
#
d$K <- scale(d$kcal.per.g)
d$F <- scale(d$perc.fat)
d$L <- scale(d$perc.lactose)

# M2 ----------------------------------------------------------------------

# Model sketch
#
# K_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_L*L
# alpha ~ normal(0, 0.2)
# beta_L ~ normal(0, 0.5)
# sigma ~ exponential(1)

# Reduction
#
dat_ls <- list(N=nrow(d), K=as.numeric(d$K), L=as.numeric(d$L))

# Fitting
#
file <- file.path(getwd(), "stan", "6", "mdl_63.stan")
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
beta_L_d <- density(samples$beta_L)
plot(beta_L_d) ; lines(beta_L_d, lty=2)

# M4 ----------------------------------------------------------------------

# Model sketch
#
# K_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_F*F
# alpha ~ normal(0, 0.2)
# beta_F ~ normal(0, 0.5)
# sigma ~ exponential(1)

# Reduction
#
dat_ls <- list(N=nrow(d), K=as.numeric(d$K), F=as.numeric(d$F))  

# Fitting
#
file <- file.path(getwd(), "stan", "6", "mdl_64.stan")
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
plot(beta_F_d) ; lines(beta_F_d, lty=2)

# M5 ----------------------------------------------------------------------

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
dat_ls <- list(N=nrow(d), K=as.numeric(d$K), L=as.numeric(d$L), 
               F=as.numeric(d$F))

# Fitting
#
file <- file.path(getwd(), "stan", "6", "mdl_65.stan")
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
plot(samples$beta_F, samples$beta_L, pch=20, 
     col=scales::alpha("steelblue", .3))

# Visualization
#
beta_F_d <- density(samples$beta_F)
beta_L_d <- density(samples$beta_L)
plot(NULL, xlim=c(-2,2), ylim=c(0,2.5)) 
lines(beta_F_d, lty=2) ; lines(beta_L_d)

# Note: since beta_F and beta_L jointly influence the mean, their sum(1) es 
# approximately equal to the estima of beta_L in the second model.

# Information extraction
#
N <- 1e4
alpha_prior <- rnorm(N, 0, 0.2)
beta_prior <- rnorm(N, 0, 0.5) 

plot(beta_L_d, xlim=c(-2,2)) 
lines(beta_F_d, lty=2) ; lines(density(beta_prior), lty=2)

--------------------------------------------------------------------------------

# Sim multicolli
#

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

# 6.2 Post-treatment Bias -------------------------------------------------

# Simulating Post-treatment Bias
#

# Number of plants
N <- 100

# Initial heights
h0 <- rnorm(N,10,2)

# Treatments
tx <- rep( 0:1, each=N/2 )

# Fungus & growth
p_fungus <- .5-tx*0.4
fungus <- rbinom(N,1, prob=p_fungus)

# Final heights
h1 <- h0 + rnorm(N,5-3*fungus)

d <- data.frame(h0=h0, tx=tx, fungus=fungus, h1=h1)

# M66 ---------------------------------------------------------------------

# Model sketch 
#..for average growth in the experiment
#
# h1_i ~ normal(mu_i, sigma)
# mu_i = h0_i * p  # p=h1_i/h0_i 
# p ~ lognormal(0, 0.25)
# sigma ~ exponential(1)

# PPS
#
p_pps <- rlnorm(1e4, 0,0.25) ; plot(density(p_pps))
abline(v=1, lty=2) ; text(1,1,"0% growth")
abline(v=1.5, lty=2) ; text(1.5,1,"50% growth")
abline(v=0.5, lty=2) ; text(0.5,1,"50% shinkage")

# M6 ---------------------------------------------------------------------

# Reduction
#
dat_ls <- list(N=nrow(d), h0=d$h0, h1=d$h1)

# Fitting
#
file <- file.path(getwd(), "stan", "6", "mdl_67.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# M7 ---------------------------------------------------------------------

# Reduction
#
dat_ls <- list(N=nrow(d), h0=d$h0, h1=d$h1, F=d$fungus, T=d$tx)
file <- file.path(getwd(), "stan", "6", "mdl_68.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

dag <- dagitty::dagitty('dag {
H0 [pos="0,0"]
H1 [pos="1,0"]
F [pos="2,0"]
T [pos="3,0"]
H0 -> H1 <- F <- T
}')
plot(dag)

dagitty::impliedConditionalIndependencies(dag)

# M8 ---------------------------------------------------------------------

dag <- dagitty::dagitty('dag {
H0 [pos="0,0"]
H1 [pos="1,0"]
M [latent, pos="1.5,1"]
F [pos="2,0"]
T [pos="3,0"]
H0 -> H1 <- M -> F <- T
}')
plot(dag)

set.seed(71)
# H0 indep
# M  indep
# T  indep
# H1 = f(M,H0)
# F = f(M,T)
N <- 1e3
H0 <- rnorm(N) 
T <- sample(0:1,N,replace=TRUE) 
M <- rbinom(N, 1, .5)
F <- rbinom(N, 1, prob=0.5-T*0.4 + 0.4*M)
H1 <- H0 * rnorm(N, 5 + 3*M)
d2 <- data.frame(H0, H1, T, M, F)

# Reduction
#
dat_ls <- list(N = nrow(d2), h0=d2$H0, h1=d2$H1, T=d2$T, F=d2$F)

# 6.3 Collider bias -------------------------------------------------------

dag <- dagitty::dagitty('dag {
H [pos="0,0"]
M [pos="1,0"]
A [pos="2,0"]
H -> M <- A
}')
plot(dag)

# Data
#
library(rethinking)
d <- rethinking::sim_happiness( seed=1977, N_years=1e3 )
str(d)

# Visualizaition I
#
plot(d$happiness~d$age, pch=20, col=ifelse(d$married, "blue", "black"))

# Reduction 
#
d2 <- d[d$age>17,] # adults only!

# Rescaling
#
min_A <- 18 ; max_A <- 65
range_A <- max_A-min_A
d2$A <- (d2$age - min_A)/(range_A)

summary(d2$A)

# M9 ----------------------------------------------------------------------

# PPS
#
N <- 1e3
b <- rnorm(N,0,.5)
plot(c(-4,4), c(-4,4), type="n")
for(i in seq(100)) abline(a=0, b[i])

# Model sketch 
#
# H ~ normal(mu, sigma)
# mu = alpha[mid[i]] + beta_A * A
# alpha ~ normal(0,0)
# beta_A ~ normal(0,1)
# beta_M ~ normal(0,2)











