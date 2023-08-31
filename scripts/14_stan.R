#
# Adventure in Covariance
#

# 14.1 Varying slopes by construction
#

# Simulate the population
# (Data Generating Process)
#
# Average morging wait time (min)
a <- 3.5
# Average diverence afternoon wait time (min)
b <- (-1) 
# Standard deviation in intercepts 
sigma_a <- 1
# Standard deviation in slopes
sigma_b <- 0.5
# Correlation between intercepts and slopes
rho <- (-0.7)
# Population: 2D-Gaussian (means, variances, covariances)
#
# Mean Vector 
Mu <- c(a,b)

# Covariance Matrix
cov_ab <- sigma_a * sigma_b * rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), nrow = 2, ncol = 2)

# Alternative
sigmas <- c(sigma_a, sigma_b)
Rho <- matrix(c(1, rho, rho, 1), nrow = 2)

# Matrix multiply
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

# Number of cafes
N_cafes <- 20

# Sample from MVN
set.seed(5)
vary_effects <- MASS::mvrnorm(N_cafes, Mu, Sigma)
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

# Visualize
#
plot(a_cafe, b_cafe, xlab = "Intercept", ylab = "Slope", pch = 16, col =
 "steelblue")
library(ellipse)
sequence <- c(0.1, 0.3, 0.5, 0.8, 0.99)
for (l in sequence) {
    lines(ellipse(Sigma, centre = Mu, level = l), col = "black")
}

# Simulate observations
#
set.seed(123)

# Number of visits per cafe
N_visits <- 10

# Indicator for afternoon
afternoon <- rep(0:1, N_visits * N_cafes/2)

# Cafe ID
cafe_id <- rep(1:N_cafes, each = N_visits)

# Simulate average wait time within cafe
mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon

# Standard deviation in wait time within cafe
sigma <- 0.5

# Simulate wait time within cafe
wait <- rnorm(N_visits * N_cafes, mu, sigma)

# Create a balanced data frame
(d <- data.frame("cafe" = cafe_id, "afternoon" = afternoon, "wait" = wait))

curve(dlnorm(x, 1, 0.5), from = 0, to = 10, lwd = 2, col = "steelblue", ylab =
      "Density", xlab = "Wait time (min)")

# Varying slopes model
library(rethinking)
# LKJcorr prior
R <- rethinking::rlkjcorr(1e4, K=2, eta=1)[,2,1] 
plot(density(R) , main = "LKJcorr prior", xlab = "Correlation", lwd = 2, col =
     "steelblue", xlim = c(-1,1))

set.seed(867530)
m14.1 <- ulam(
    alist(
        wait ~ normal(mu, sigma),
        mu <- a_cafe[cafe] + b_cafe[cafe] * afternoon,
        c(a_cafe, b_cafe)[cafe] ~ multi_normal(c(a,b), Rho, sigma_cafe),
        # Prior for the average intercept 
        # (Average waiting time across cafes)
        a ~ normal(5, 2),
        # Prior for the average slope 
        # (Difference morning and afternoon waiting time)
        b ~ normal(-1, 0.5),
        # Prior for the standard deviation within cafe
        sigma_cafe ~ exponential(1),
        # Prior for the standard deviation among cafe
        sigma ~ exponential(1),
        # Prior for the correlation between intercepts and slopes
        Rho ~ lkj_corr(2)
    ), data = d, chains = 4, cores = 4 
)
stancode(m14.1)

# BRMS model
library(brms)
 b14.1 <- 
  brm(data = d, family = gaussian,
      wait ~ 1 + afternoon + (1 + afternoon | cafe),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 2), class = sd),
                prior(cauchy(0, 2), class = sigma),
                prior(lkj(2), class = cor)),
      iter = 5000, warmup = 2000, chains = 2, cores = 2,
      seed = 13)

brms::stancode(b14.1)
post <- posterior_samples(b13.1)

# Divergences....
post <- extract.samples(m14.1)
dens( post$Rho[,1,2] , xlim = c(-1,1) )
R <- rlkjcorr(1e4, K = 2, eta = 2)
dens( R[,1,2] , add = TRUE, lty = 2, lwd = 2, col = "steelblue" ) 

# Data list
#
dat_ls <- list(
    wait = d$wait,
    afternoon = d$afternoon,
    cafe = d$cafe,
    n_i = N_visits*N_cafes,
    n_j = N_cafes
)

# Fit the model 
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "14", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
# Note that the alpha parameters are on the log-odds scale!
fit$print(max_rows=200)

# Diagnostics
#
fit$sampler_diagnostics()
fit$cmdstan_diagnose()
fit$diagnostic_summary()

# Posterior
#
post <- fit$draws(format = "matrix") 
prior_draws_Rho <- rethinking::rlkjcorr(1e4, K = 2, eta = 2)
post_draws_Rho <- post[,grep("Rho", colnames(post), value = TRUE)]
plot(density(post_draws_Rho[,2]), main = "Rho Posterior", xlab = "Correlation", 
lwd = 3, col = "steelblue")
lines( density(prior_draws_Rho[,2,1]) )

# Compute unpooled estimates
a1 <- sapply(1:N_cafes, 
             function(i) mean(d$wait[cafe_id==i & d$afternoon==0]) )
b1 <- sapply(1:N_cafes, 
             function(i) mean(d$wait[cafe_id==i & d$afternoon==1]) ) - a1
# Posterior mean of partially pooled estimates
# Note: Dont name variables a and b! use alpha and beta!
# Why? Because it makes extracting variables with grep easier!
a2 <- apply(post[,grep("a_j", colnames(post))], 2, mean)[1:20]
b2 <- apply(post[,grep("b_j", colnames(post))], 2, mean)
plot(a1, b1, pch = 16, col = "steelblue", xlab = "Intercept", ylab = "Slope")
points(a2, b2, pch = 16, col = "orange")
for( i in 1:N_cafes ) lines( c(a1[i],a2[i]), c(b1[i],b2[i]) )

# Compute the posterior mean bivariate Gaussian
Mu_est <- c( mean( post[,"a"] ) , mean( post[,"b"] ) )
rho_est <- mean( post[,"Rho[1,2]"] )
sa_est <- mean( post[,"sigma_j[1]"] )
sb_est <- mean( post[,"sigma_j[2]"] )
cov_ab <- sa_est * sb_est * rho_est
Sigma_est <- matrix( c(sa_est^2, cov_ab, cov_ab, sb_est^2), ncol = 2 )

for(l in c(0.1,0.3,0.5,0.8,0.99)) {
    lines( ellipse(Sigma_est, centre = Mu_est, level = l), col = "black" )
}


