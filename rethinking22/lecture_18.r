#
# Lecture 18 (Missing data)
#

# Draw DAGs on paper before simulating!

# Dog eats random homework (MCAR)
set.seed(112)
N <- 1e2 # 100 students
S <- rnorm(N) # Study time (standardized)
D <- rbinom(N, 1, 0.5) # Missingness indicator
H <- rnorm(N, 0.6 * S) # Effect: S -> H (more studying; more quality)
# Dog eats 80% of the homework at random
Hstar <- H
Hstar[D == 1] <- NA
Hstar
# Visualize
plot(S, H, col = "steelblue", lwd = 2)
points(S, Hstar, col = 2 , lwd = 3)
# True effect: S -> H (unobserved)
abline(lm(H ~ S), lwd = 3 , col = "steelblue")
# Estimated effect: S -> Hstar (partially observed)
abline(lm(Hstar ~ S), lwd = 3, col = 2)

# Dog eats homework of students how study too much (MAR)
# Conditional on student
set.seed(112)
N <- 1e2
S <- rnorm(N) # Study effort in time
H <- rnorm(N, 0.8*S) # Asm: Linear effect!
D <- ifelse(S > 0, 1, 0) #...if student studies more than average – eat!
Hstar <- H
Hstar[D == 1] <- NA
# Visualize
plot(S, H, col = "steelblue", lwd = 2)
points(S, Hstar, col = 2 , lwd = 3)
# True effect: S -> H (unobserved)
abline(lm(H ~ S), lwd = 3 , col = "steelblue")
# Estimated effect: S -> Hstar (partially observed)
abline(lm(Hstar ~ S), lwd = 3, col = 2)

# Dog eats homework of students how study too much (MAR)
# NONLINEAR-CEILING EFFECT
set.seed(112)
N <- 1e2
S <- rnorm(N) # Study effort in time
H <- rnorm(N, 1-exp(-0.7*S)) # Asm: Noninear effect!
D <- ifelse(S > 0, 1, 0) #...if student studies more than average – eat!
Hstar <- H
Hstar[D == 1] <- NA
# Visualize
plot(S, H, col = "steelblue", lwd = 2)
points(S, Hstar, col = 2 , lwd = 3)
# True effect: S -> H (unobserved)
abline(lm(H ~ S), lwd = 3 , col = "steelblue")
# Estimated effect: S -> Hstar (partially observed)
abline(lm(Hstar ~ S), lwd = 3, col = 2) # Boooom!

# Dog eats bad homework (MNAR)
# Conditional on homework
N <- 1e2
S <- rnorm(N) # Study effort in time
H <- rnorm(N, 0.7*S) # Asm: Linear effect!
D <- ifelse(H < 0, 1, 0) #...if homework are worse than average – feed to dog
Hstar <- H
Hstar[D == 1] <- NA
# Visualize
plot(S, H, col = "steelblue", lwd = 2)
points(S, Hstar, col = 2 , lwd = 3)
# True effect: S -> H (unobserved)
abline(lm(H ~ S), lwd = 3 , col = "steelblue")
# Estimated effect: S -> Hstar (partially observed)
abline(lm(Hstar ~ S), lwd = 3, col = 2) # Boooom!

#
# Primated Phylogeny
# Moving from complete cases to imputation
#
library(rethinking)
library(ape)
data(Primates301) ; data(Primates301_nex)
d <- Primates301
d$name <- as.character(d$name)

# Complete case analysis (1a.stan)

# Data
dcc <- d[complete.cases(d$group_size, d$body, d$brain), ]
spp <- dcc$name
# Phylogeny
spp <- as.character(dcc$name)
tree_trimmed <- keep.tip(Primates301_nex, spp)
Rbm <- corBrownian(phy = tree_trimmed)
V <- vcv(Rbm)
Dmat <- cophenetic(tree_trimmed)

# Stan list
stan_ls <- list(
    N = nrow(dcc),
    M = standardize(log(dcc$body)),
    B = standardize(log(dcc$brain)),
    G = standardize(log(dcc$group_size)),
    Dmat = Dmat[spp, spp] / max(Dmat)
)

# Stan model
path <- "~/projects/rethinking/rethinking22"
file <- file.path(path, "stan", "18", "1a.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic = TRUE)
fit <- mdl$sample(data=stan_ls, parallel_chains = 4)
fit$cmdstan_diagnose()

# Posterior
draws1a <- fit$draws(format = "data.frame")

# Visualize
plot(density(draws1a$bG), lwd = 3, col = "steelblue", xlab = "effect of G on B",
ylim = c(0, 25)) 
abline(v = 0, lty = 3)

# Impute group size (1b.stan)

# Data
dd <- d[complete.cases(d$brain, d$body), ]
spp <- dd$name
# Phylogeny
spp <- as.character(dd$name)
tree_trimmed <- keep.tip(Primates301_nex, spp)
Rbm <- corBrownian(phy = tree_trimmed)
V <- vcv(Rbm)
Dmat <- cophenetic(tree_trimmed)

# Impute only group size
stan_ls <- list(
    N = nrow(dd),
    N_G_obs = sum(complete.cases(dd$group_size)),
    ii_G_obs = which(complete.cases(dd$group_size)),
    N_G_mis = sum(is.na(dd$group_size)),
    ii_G_mis = which(is.na(dd$group_size)),
    M = standardize(log(dd$body)),
    B = standardize(log(dd$brain)),
    Dmat = Dmat[spp, spp] / max(Dmat)
)

# Stan model
path <- "~/projects/rethinking/rethinking22"
file <- file.path(path, "stan", "18", "1b.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic = TRUE)
fit <- mdl$sample(data=stan_ls, parallel_chains = 4)
fit$cmdstan_diagnose()

# Posterior
draws1b <- fit$draws(format = "data.frame")

colnames(draws1b)


# Continue with two missing variables (1c.stan)