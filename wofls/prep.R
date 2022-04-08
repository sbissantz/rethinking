#
# Data
#

# Suppose we have the following data...
# Note: Statistical Rethinking (2020) (p.194)

sppnames <- c("afarensis", "africanus", "habilits", "boisei", "rudolfensis",
              "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species=sppnames, brain=brainvolcc, mass=masskg)

# Visualize
#
plot(d$mass, d$brain, ylab="brain volume (cc)", xlab="body mass (kg)", pch=20)
text(x=masskg, y=brainvolcc+50, sppnames)

# Rescale variables
#
# Standardize
d$M <- with(d, (mass - mean(mass))/sd(mass))
# Normalize
d$B <- with(d, brain/max(brain))

# Modifications 
#
# Standardize
d$M <- with(d, (mass - mean(mass))/sd(mass))
# Normalize
d$B <- with(d, brain/max(brain))

# Reduction
#
dat_ls <- list(N=nrow(d), B=d$B, M=d$M)

# Model sketch
#
# B_i ~ Normal(mu_i, sigma)
# mu_i = alpha + beta_M * M_i
# alpha ~ normal(0,0.1)
# beta_M ~ normal(0,5)
# sigma ~ exponential(1)
path <- "~/projects/stanmisc/wofls/"
file <- file.path(path, "model.stan")

# Fitting
#
# Compile the Stan program
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
# Fit the model
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

