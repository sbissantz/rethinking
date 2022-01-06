
# 5.1 ---------------------------------------------------------------------

# Data wrangling
#
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
# Standardize the variables
d$D <- scale(d$Divorce)
d$A <- scale(d$MedianAgeMarriage)
# Model
# D_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_A*A_i 
# alpha ~ normal(0,0.2)
# beta ~ normal(?,?)

# Prior predictive simulation 
#
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
N <- 1e2
a <- rnorm(N,0,0.1)
b_A <- rnorm(N,0,0.5)
plot(c(-2,2), c(-2,2), type="n", xlab="Median Age at Marriage (std)", 
     ylab="Divorce rate (std)")
for(i in seq(N))  curve(a[i] + b_A[i] * x, from=min(d$A), to=max(d$A),
                        add=TRUE)

# Data list
#
dat_ls <- list(
  N = nrow(d),
  D = as.numeric(d$D),
  A = as.numeric(d$A)
)

# Model
#
file_path <- file.path(getwd(), "stan", "mdl51.stan")
fml <- cmdstanr::cmdstan_model(file_path, pedantic=TRUE)
fit <- fml$sample(data = dat_ls)
fit$cmdstan_diagnose()
samples <- fit$draws(format = "matrix")
bayesplot::mcmc_trace(samples, pars=c("a", "b_A", "sigma"))
fit$print()

# Posterior inference
#
mu <- fit$draws(variables = "mu", format = "matrix")
mu.mean <- apply(mu, 2, mean)
mu.hpdi <- apply(mu, 2, rethinking::HPDI)
plot(d$A, d$D, col=rangi2, ylab="Divorce rate (std)", 
     xlab="Median Age of Marriage (std)")
lines(d$A, mu.mean)
rethinking::shade(mu.hpdi, d$A)

# 5.2 ---------------------------------------------------------------------

# Data wrangling
#
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
d$D <- scale(d$Divorce)
d$M <- scale(d$Marriage)

# Prior predictive simulation
#
# D ~ normal(mu, sigma)
# mu_i = a + b_M * M_i  
# a ~ normal(0,0.1)
# b ~ normal(0,1)
plot(d$M, d$D, xlab="Marriage rate", ylab="Divorce rate")
N <- 1e2
a <- rnorm(N,0,0.1)
b_M <- rnorm(N,0,0.5)
for(i in seq(N)) curve(a[i] + b_M[i]*x, from=min(d$M), to=max(d$M), add=TRUE)

# Data list
#
dat_ls <- list(
  D = as.numeric(d$D),
  M = as.numeric(d$M),
  N = nrow(d),
  M_seq = as.numeric(seq(-2, 2, length.out=30)),
  K = 30 
)

# Model definition
#
file <- file.path(getwd(), "stan", "m52.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print(max_rows = 50)

# Samples
#
samples <- fit$draws(format = "matrix")
mu <- fit$draws(variables="mu", format="matrix")

# Visual posterior inference
#
mu.mean <- apply(mu, 2, mean)
mu.hpdi <- apply(mu, 2, rethinking::HPDI)
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xaxt="n", yaxt="n", 
     xlab="Marriage rate (%)", ylab="Divorce rate (%)", )
points(d$M, d$D, pch=20, col="lightblue") 
lbl_x <- round(sd(d$Marriage) * seq(-2,2), digits = 2)
lbl_y <- round(sd(d$Divorce) * seq(-2,2), digits = 2)
axis(side=1,at = seq(-2,2), labels = lbl_x ) 
axis(side=2,at = seq(-2,2), labels = lbl_y) 
lines(dat_ls$M_seq, mu.mean, lwd=2)
rethinking::shade(mu.hpdi, dat_ls$M_seq)






