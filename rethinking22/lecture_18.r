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
data(Primates301)
d <- Primates301
d$name <- as.character(d$name)
dstan <- d[ complete.cases( d$group_size , d$body , d$brain ) , ]
spp_cc <- dstan$name

# complete case analysis
dstan <- d[complete.cases(d$group_size,d$body,d$brain),]
dat_cc <- list(
    N_spp = nrow(dstan),
    M = standardize(log(dstan$body)),
    B = standardize(log(dstan$brain)),
    G = standardize(log(dstan$group_size)),
    Imat = diag(nrow(dstan)) )








# Start with complete cases (1a.stan)



# Continue with one missing variable (1b.stan)
# Continue with two missing variables (1c.stan)