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
