#
# Lecture 19 (Generalized linear madness)
#

# Old friend
#
library(rethinking)
data(Howell1)
d <- Howell1

# Scale observed variables by mean
d$w <- d$weight / mean(d$weight)
d$h <- d$height / mean(d$height)

# Prior predictive simulation
#
N <- 30
p <- rbeta(n, 25, 50)
k <- rexp(n, 0.5)
x_seq <- seq(0, 1.3, length.out = 100)
plot(NULL, xlim = c(0, 1.3), ylim = c(0, 1.5))
for (i in 1:N) {
    mu <- log(pi * k[i] * p[i]^2 * x_seq^3)

# Stan list
stan_ls <- list(
    "N" = nrow(d),
    "w" = d$w,
    "h" = d$h
)

# Stan model
path <- "~/projects/rethinking/rethinking2nd"
# file <- file.path(path, "stan", "16", "1a.stan")
# Note: Do not use the stan translation of the ulam model! 7x 
# Stan translation of ULAM model is code in 1a.stan
file <- file.path(path, "stan", "16", "1b.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls, chains = 4, parallel_chains = 4)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print("p", max_rows=200) # 0.26
fit$print("k", max_rows=200) # 6.05
fit$print("sigma", max_rows=200) # 1.62
# Sigma is different! (1.62 vs. 0.21)

# Posterior draws
#
postdraws <- fit$draws(format = "data.frame")
p <- fit$draws("p", format = "matrix")
k <- fit$draws("k", format = "matrix")
mu <- fit$draws("mu", format = "matrix")
mu_mean <- colMeans(mu) 
w_sim <- fit$draws("w_sim", format = "matrix")
w_sim_mu <- colMeans(w_sim)

# Visualize
#
plot(d$h, d$w, type = "n")
for(i in 1:30) {
    points(d$h, w_sim[i,], col=col.alpha("steelblue", 0.2), pch=20)
}
points(d$h, d$w, col=col.alpha("black", 0.9), pch=16)
points(d$h, w_sim_mu, col=col.alpha("red", 0.5), pch=20, cex = 2)

# Important: Deterministic association between p and k 
# Important: Deterministic association between p and k 
plot(p, k, col = col.alpha("steelblue", 0.5), pch = 20)





