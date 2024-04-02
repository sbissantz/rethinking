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

# Overall DAG

dag <- dagitty::dagitty( 'dag {
Brainsize [outcome,pos="0,0"]
Bstar [outcome,pos="-0.5,0"]
mB [outcome,pos="-0.5,0.1"]

Groupsize [exposure,pos="1,0"]
Gstar [exposure,pos="1.5,0"]
mG [exposure,pos="1.5,0.1"]

Bodymass [selected,pos="0.5,0.25"]
Mstar [selected,pos="0.15,0.25"]
mM [selected,pos="0.15,0.35"]

u [pos="0.5,0.5"]
history [pos="1,0.5"] 

Groupsize -> Brainsize
Bodymass -> Brainsize
Bodymass -> Groupsize

Groupsize -> Gstar
Bodymass -> Mstar 
Brainsize -> Bstar 

mG -> Gstar
mB -> Bstar
mM -> Mstar

u -> Groupsize
u -> Bodymass
u -> Brainsize
history -> u
}')
plot(dag)

# Complete case analysis (1a.stan)

# DAG
dag <- dagitty::dagitty( 'dag {
Brainsize [outcome,pos="0,0"]
Groupsize [exposure,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Groupsize -> Brainsize
Bodymass -> Brainsize
Bodymass -> Groupsize
u -> Groupsize
u -> Bodymass
u -> Brainsize
history -> u
}')
plot(dag)

# Submodel 1 
dag <- dagitty::dagitty( 'dag {
Brainsize [outcome,pos="0,0"]
Groupsize [exposure,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Groupsize -> Brainsize
Bodymass -> Brainsize
u -> Brainsize
history -> u
}')
plot(dag)

# Submodel 2
dag <- dagitty::dagitty( 'dag {
Groupsize [outcome,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Bodymass -> Groupsize
u -> Groupsize
history -> u
}')
plot(dag)

# Submodel 3
dag <- dagitty::dagitty( 'dag {
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
u -> Bodymass 
history -> u
}')
plot(dag)


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

# Impute group size and body mass (1b.stan)
# ...ignore models for each

op <- par()
par(mfrow = c(3,1))

# Submodel 1 
dag <- dagitty::dagitty( 'dag {
Brainsize [outcome,pos="0,0"]
Groupsize [exposure,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Groupsize -> Brainsize
Bodymass -> Brainsize
u -> Brainsize
history -> u
}')
plot(dag)

# Submodel 2 
dag <- dagitty::dagitty( 'dag {
Groupsize [outcome,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
history -> u
}')
plot(dag)

# Submodel 3
dag <- dagitty::dagitty( 'dag {
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
history -> u
}')
plot(dag)
par(op)

# Ulam version (Why is it so much faster?)
# Ulam version was faster, because I did forget to set the prior for the mixed 
# parameter/data vectors!

# Data
dd <- d[complete.cases(d$brain),]
spp <- dd$name
# Phylogeny
spp <- as.character(dd$name)
tree_trimmed <- keep.tip(Primates301_nex, spp)
Rbm <- corBrownian(phy = tree_trimmed)
V <- vcv(Rbm)
Dmat <- cophenetic(tree_trimmed)

stan_ls <- list(
    N = nrow(dd),
    # Impute G
    N_G_obs = sum(complete.cases(dd$group_size)),
    N_G_mis = sum(is.na(dd$group_size)),
    G_obs = standardize(log(dd$group_size[complete.cases(dd$group_size)])),
    ii_G_obs = which(complete.cases(dd$group_size)),
    ii_G_mis = which(is.na(dd$group_size)),
    # Impute B
    N_M_obs = sum(complete.cases(dd$body)),
    N_M_mis = sum(is.na(dd$body)),
    M_obs = standardize(log(dd$body[complete.cases(dd$body)])),
    ii_M_obs = which(complete.cases(dd$body)),
    ii_M_mis = which(is.na(dd$body)),
    # Oldje stuff 
    B = standardize(log(dd$brain)),
    Dmat = Dmat[spp, spp] / max(Dmat)
)

# Stan model
# Now Stan model runs fast! 
path <- "~/projects/rethinking/rethinking22"
# file <- file.path(path, "stan", "18", "1b2.stan")
# Switched from arrays to vectors for the mixed parameter/data vectors
file <- file.path(path, "stan", "18", "1b2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic = TRUE)
fit <- mdl$sample(data=stan_ls, chains = 4, parallel_chains = 4)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$cmdstan_summary()

# Posterior
draws1b <- fit$draws(format = "data.frame")
M_impute <- fit$draws("M_mis", format = "matrix")
G_impute <- fit$draws("G_mis", format = "matrix")

# Show M imputed values against regression of B on M

plot(standardize(log(dd$body)), stan_ls$B, lwd = 3, col = grau(0.2),
    xlab = "body mass (standardized)", ylab = "brain volume (standardized)"
)
x <- apply(M_impute,2,mean)
y <- stan_ls$B[alstan_ls$ii_M_mis]
# Important: M and B strongly associated, so imputed M values follow the trend!
points(x, y, lwd=3 , col = 2)
for (i in 1:2) {
    y <- stan_ls$B[stan_ls$ii_M_mis][i]
    lines(PI(M_impute[, i]), c(y, y), lwd = 8, col = col.alpha(2, 0.7))
}

# Show relation between G estimates and M
Gest <- apply(G_impute,2,mean)
plot(standardize(log(dd$group_size)), standardize(log(dd$body)),
    lwd = 2, col = grau(), 
    xlab = "Body mass (standardized)", ylab = "Group size (standardized)"
)
# Important: Association between M and G not modeled, so imputed values do not 
# follow the trend!
for (i in 1:33) {
    x <- stan_ls$M[stan_ls$ii_G_mis][i]
    lines(rep(x,2), PI(G_impute[, i]), lwd = 8, col = col.alpha(2, 0.3))
}
points( stan_ls$M_obs[stan_ls$ii_G_mis], Gest , lwd=3 , col=2 )

# Compare posterior bG of complete case and imputation models

# Visualize
plot(density(draws1a$bG), lwd = 3, col = "steelblue", 
xlab = "effect of G on B", ylim = c(0, 25)) 
lines(density(draws1b$bG), lwd = 3)
abline(v = 0, lty = 3)

# Impute G using model

op <- par()
par(mfrow = c(3,1))

# Submodel 1 
dag <- dagitty::dagitty( 'dag {
Brainsize [outcome,pos="0,0"]
Groupsize [exposure,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Groupsize -> Brainsize
Bodymass -> Brainsize
u -> Brainsize
history -> u
}')
plot(dag)

# Submodel 2
dag <- dagitty::dagitty( 'dag {
Groupsize [outcome,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Bodymass -> Groupsize
u -> Groupsize
history -> u
}')
plot(dag)

# Submodel 3
dag <- dagitty::dagitty( 'dag {
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
history -> u
}')
plot(dag)
par(op)

# Stan model
path <- "~/projects/rethinking/rethinking22"
file <- file.path(path, "stan", "18", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic = TRUE)
fit <- mdl$sample(data=stan_ls, chains = 4, parallel_chains = 4)

# Posterior
draws2 <- fit$draws(format = "data.frame")
M_impute <- fit$draws("M_mis", format = "matrix")
G_impute <- fit$draws("G_mis", format = "matrix")

# Show relation between G estimates and M
Gest <- apply(G_impute,2,mean)
plot(standardize(log(dd$group_size)), standardize(log(dd$body)),
    lwd = 2, col = grau(), 
    xlab = "Body mass (standardized)", ylab = "Group size (standardized)"
)
# Important: Association between M and G modeled; imputed values follow trend!
for (i in 1:33) {
    x <- stan_ls$M[stan_ls$ii_G_mis][i]
    lines(rep(x,2), PI(G_impute[, i]), lwd = 8, col = col.alpha(2, 0.3))
}
points( stan_ls$M_obs[stan_ls$ii_G_mis], Gest , lwd=3 , col=2 )

# Visualize
plot(density(draws1a$bG), lwd = 3, col = "steelblue", 
xlab = "effect of G on B", ylim = c(0, 25)) 
lines(density(draws1b$bG), lwd = 3)
lines(density(draws2$bGB), lwd = 3, col = "red")
abline(v = 0, lty = 3)

#
# DAG (final model)
#

op <- par()
par(mfrow = c(3,1))

# Submodel 1 
dag <- dagitty::dagitty( 'dag {
Brainsize [outcome,pos="0,0"]
Groupsize [exposure,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Groupsize -> Brainsize
Bodymass -> Brainsize
u -> Brainsize
history -> u
}')
plot(dag)

# Submodel 2
dag <- dagitty::dagitty( 'dag {
Groupsize [outcome,pos="1,0"]
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
Bodymass -> Groupsize
u -> Groupsize
history -> u
}')
plot(dag)

# Submodel 3
dag <- dagitty::dagitty( 'dag {
Bodymass [outcome,pos="0.5,0.25"]
u [pos="0.5,0.5"]
history [pos="1,0.5"] 
u -> Bodymass 
history -> u
}')
plot(dag)

par(op)

# Data
spp <- d$name
# Phylogeny
spp <- as.character(d$name)
tree_trimmed <- keep.tip(Primates301_nex, spp)
Rbm <- corBrownian(phy = tree_trimmed)
V <- vcv(Rbm)
Dmat <- cophenetic(tree_trimmed)

stan_ls <- list(
    N = nrow(d),
    # Impute B
    N_B_obs = sum(complete.cases(d$brain)),
    N_B_mis = sum(is.na(d$brain)),
    B_obs = standardize(log(d$brain[complete.cases(d$brain)])),
    ii_B_obs = which(complete.cases(d$brain)),
    ii_B_mis = which(is.na(d$brain)),
    # Impute G
    N_G_obs = sum(complete.cases(d$group_size)),
    N_G_mis = sum(is.na(d$group_size)),
    G_obs = standardize(log(d$group_size[complete.cases(d$group_size)])),
    ii_G_obs = which(complete.cases(d$group_size)),
    ii_G_mis = which(is.na(d$group_size)),
    # Impute B
    N_M_obs = sum(complete.cases(d$body)),
    N_M_mis = sum(is.na(d$body)),
    M_obs = standardize(log(d$body[complete.cases(d$body)])),
    ii_M_obs = which(complete.cases(d$body)),
    ii_M_mis = which(is.na(d$body)),
    # Distance matrix
    Dmat = Dmat[spp, spp] / max(Dmat)
)

# Stan model
path <- "~/projects/rethinking/rethinking22"
file <- file.path(path, "stan", "18", "3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic = TRUE)
fit <- mdl$sample(data = stan_ls, chains = 4, parallel_chains = 4)

#
# How long does the solution run?
#

# Why does the model take so much longer?

#
# How long does the rethinking solution run?
#



# Posterior
draws3 <- fit$draws(format = "data.frame")
M_impute <- fit$draws("M_mis", format = "matrix")
G_impute <- fit$draws("G_mis", format = "matrix")

# Show relation between G estimates and M
Gest <- apply(G_impute,2,mean)
plot(standardize(log(dd$group_size)), standardize(log(dd$body)),
    lwd = 2, col = grau(), 
    xlab = "Body mass (standardized)", ylab = "Group size (standardized)"
)
# Important: Association between M and G modeled; imputed values follow trend!
for (i in 1:33) {
    x <- stan_ls$M[stan_ls$ii_G_mis][i]
    lines(rep(x,2), PI(G_impute[, i]), lwd = 8, col = col.alpha(2, 0.3))
}
points( stan_ls$M_obs[stan_ls$ii_G_mis], Gest , lwd=3 , col=2 )

# Visualize
plot(density(draws1a$bG), lwd = 3, col = "steelblue", 
xlab = "effect of G on B", ylim = c(0, 25)) 
lines(density(draws1b$bG), lwd = 3)
lines(density(draws2$bGB), lwd = 3, col = "red")
lines(density(draws3$bGB), lwd = 3, col = "red")
abline(v = 0, lty = 3)
