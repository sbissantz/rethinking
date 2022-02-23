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
