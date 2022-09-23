#
#
# Multinomial and categorical models
#
#

#
# I. Predictor matched to outcome
#

# Simulate data
# ...careeer choices among 500 individuals
N <- 500
# Expected income for each career (in K)
# Import: use small numbers for the simulation, 
# ...exp(1000) = inf ->  1/inf = NAN
income <- c(1,2,5) 
# Scores of each career
score <- 0.5 * income
# Convert scores to probabilites 
softmax <- function(k){
    exp(k)/sum(exp(k))
}
(p <- softmax(score))
# Simulate choice
# Outcome career holds event type values, not counts
# Empty vecor of choices for each individual
career <- rep(NA, N)
set.seed(34302)
for (i in 1:N) career[i] <- sample(1:3, size=1, prob=p)

# Data reduction
#
dat_ls <- list(N=N, K=3, career=career, career_income=income)

# Fit the model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_10.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
# Note: 1% divergent transitions
fit$cmdstan_diagnose()
fit$summary()

# Samples
#
(post <- fit$draws(format="matrix"))

# Predictions
#
# S3 is the pivot
s1 <- post[,"alpha[1]"]  +  post[,"beta"] * income[1]
s2 <- post[,"alpha[2]"]  +  post[,"beta"] * income[2]
# double the income of 2
s2_double <- post[,"alpha[2]"]  +  post[,"beta"] * income[2] * 2
N_samples <- nrow(post)
p <- sapply(seq(N_samples), function(i) softmax(c(s1[i], s2[i], 0)))
p_double <- sapply(seq(N_samples), function(i) softmax(c(s1[i], s2_double[i], 0)))
p_diff <- p_double[2,] - p[2,]
mean(p_diff)

# 
# Predictor matched to observations
#
N <- 5e2
# Simulate family income for each type of event
family_income <- runif(N)
# Assign a unique coefficient for each type of event
b <- c(-2,0,2)
career <- rep(NA, N) #empty vector of choices for each iindividual
for (i in 1:N) {
  score <- 0.5*(1:3) + b*family_income[i]
  p <- softmax(c(score[1], score[2], score[3]))
  career[i] <- sample( 1:3, size=1, prob=p )
}

# Data reduction
#
dat_ls <- list(N=N, K=3, career=career, family_income=family_income)

# Fit the model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_11.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat_ls)

# Diagnostics
# Note: 1% divergent transitions
fit$cmdstan_diagnose()
fit$summary()

# Posterior
#
post <- fit$draws(format="matrix")

# S3 is the pivot
N <- length(family_income)
s1 <- sapply(seq(N), function(i) post[,"alpha[1]"]  +  
             post[,"beta[1]"] * family_income[i])
s2 <- sapply(seq(N), function(i) post[,"alpha[2]"]  +  
             post[,"beta[2]"] * family_income[i])
# Doubling the family outcome
s1_new <- sapply(seq(N), function(i) post[,"alpha[1]"]  +  
                 post[,"beta[1]"] * family_income[i] * 3)
p <- sapply(seq(N_samples), function(i) softmax(c(s1[i], s2[i], 0)))
# Scores with doubled family income
p_new <- sapply(seq(N_samples), function(i) softmax(c(s1_new[i], s2[i], 0)))
rowMeans(p_new) - rowMeans(p)

