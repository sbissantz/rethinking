#
# Generalized Linear Madness
#

# 16.1 Geometric people
#
library(rethinking)
data(Howell1)
d <- Howell1

# Scale observed variables by mean
d$w <- d$weight / mean(d$weight)
d$h <- d$height / mean(d$height)

m16.1 <- ulam(
    alist(
        w ~ dlnorm( mu , sigma ),
        exp(mu) <- 3.141593 * k * p^2 * h^3,
        p ~ beta(2,18),
        k ~ exponential(0.5),
        sigma ~ exponential(1)
    ), data = d, chains = 4, cores = 4)

precis(m16.1)
      #mean   sd 5.5% 94.5% n_eff Rhat4
#p     0.25 0.06 0.17  0.35   497  1.01
#k     5.64 2.62 2.50 10.70   495  1.01
#sigma 0.21 0.01 0.20  0.22   674  1.00

h_seq <- seq(0.5, max(d$h), length.out=30)
w_sim <- sim(m16.1, data=list(h=h_seq)) 

colMeans(w_sim)


plot(h_seq, colMeans(w_sim))

# Note: Do not use the stan translation of the ulam model! 7x 
# Stan translation of ULAM model is code in 1a.stan
#model{
     #vector[544] mu;
    #sigma ~ exponential( 1 );
    #k ~ exponential( 0.5 );
    #p ~ beta( 2 , 18 );
    #for ( i in 1:544 ) {
        #mu[i] = 3.141593 * k * p^2 * h[i]^3;
        #mu[i] = log(mu[i]);
    #}
    #w ~ lognormal( mu , sigma );
#}

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
fit <- mdl$sample(data=stan_ls, chains=4 , parallel_chains=4)
fit$cmdstan_diagnose()
# Note that the alpha parameters are on the log-odds scale!
fit$print("p", max_rows=200) # 0.26
fit$print("k", max_rows=200) # 6.05
fit$print("sigma", max_rows=200) # 1.62
# Sigma is different! (1.62 vs. 0.21)

# Posterior draws
postdraws <- fit$draws(format = "data.frame")
p <- fit$draws("p", format = "matrix")
k <- fit$draws("k", format = "matrix")
mu <- fit$draws("mu", format = "matrix")
mu_mean <- colMeans(mu) 
w_sim <- fit$draws("w_sim", format = "matrix")
w_sim_mu <- colMeans(w_sim)

plot(d$h, d$w, type = "n")
for(i in 1:30) {
    points(d$h, w_sim[i,], col=col.alpha("steelblue", 0.2), pch=20)
}
points(d$h, d$w, col=col.alpha("black", 0.9), pch=16)
points(d$h, w_sim_mu, col=col.alpha("red", 0.5), pch=20, cex = 2)

# 16.2 Hidden minds and observed behavior
#

library(rethinking)
data(Boxes)
precis(Boxes)
# 1: unchosen color
# 2: majority demonstrated
# 3. minority demonstrated
# Note: `majority first` indiciates if majority demonstrated first

# table(Boxes$y) / length(Boxes$y)
prop.table(table(Boxes$y))

# Generative simulation

# In generative simulation we assume strategies and simulate behavior
# In statistical models we assume behavior (data) and infer strategies (pars)

# Different strategies produce the same outcome
set.seed(7)
N <- 30 # number of children
# Half use random strategy
y1 <- sample(1:3, size = N /2, replace = TRUE)
# Half use majority strategy
y2 <- rep(2, N/2)
# Combine and shuffle y1 and y2
y <- sample(c(y1, y2))
# Count the 2s
sum(y == 2) / N
# /2/3 choose majority color but only half followed majority strategy

# Model 5 different strategies:
# 1. Follow majority
# 2. Follow minority
# 3. Maverick
# 4. Random – 1/3, 1/3, 1/3
# 5. Follow first -  majority if majority first, minority if minority first

# Stan model
data(Boxes_model)
cat(Boxes_model)

# Data
stan_ls <- list(
    "N" = nrow(Boxes),
    "S" = 5,
    "y" = Boxes$y,
    majority_first = Boxes$majority_first
)

# Hidden state model (no covariates)

# Stan model
path <- "~/projects/rethinking/rethinking2nd"
file <- file.path(path, "stan", "16", "2.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls, chains=4 , parallel_chains=4)
# Diagnostics
fit$cmdstan_diagnose()
fit$print("p", max_rows=200)

# Posterior draws
postdraws <- fit$draws(format = "data.frame")
names(postdraws)

# Visualize
p <- fit$draws("p", format = "matrix")
p_labels <- c("follow majority", "follow minority", "maverick", "random", 
"follow first")
p_mu <- colMeans(p)
p_sd <- apply(p, 2, sd)
dotchart(p_mu, labels = p_labels, main = "Posterior means of strategies",
    xlim = c(0, 0.5)) 
for (i in 1:5) {
    lines(c(p_mu[i] - 2 * p_sd[i], p_mu[i] + 2 * p_sd[i]), c(i, i))
}

# Add a predictor variable
#
data(Boxes_model_gender)
cat(Boxes_model_gender)

# Data
stan_ls <- list(
    "N" = nrow(Boxes),
    "S" = 5,
    "y" = Boxes$y,
    majority_first = Boxes$majority_first,
    gender = Boxes$gender
)

# Hidden state model (gender as covariate)

# Stan model
path <- "~/projects/rethinking/rethinking2nd"
file <- file.path(path, "stan", "16", "3.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls, chains=4 , parallel_chains=4)
# Diagnostics
fit$cmdstan_diagnose()
fit$print("p", max_rows=200)

# Posterior draws
postdraws <- fit$draws(format = "matrix")
colnames(postdraws)

# Visualize
p <- fit$draws("p", format = "matrix")
p_labels <- c("follow majority", "follow minority", "maverick", "random", 
"follow first")
p_mu <- colMeans(p)
p_sd <- apply(p, 2, sd)
dotchart(p_mu, labels = rep(p_labels, 2, each = 2),
main = "Posterior means of strategies", xlim = c(0, 0.5)) 
my_col <- rep(c("blue", "pink"), 5)
for (i in 1:10) {
    lines(c(p_mu[i] - 2 * p_sd[i], p_mu[i] + 2 * p_sd[i]), c(i, i),
    col = my_col[i], lwd = 2)
}

# Hidden state model (age as covariate)
data(Boxes_model_age)
cat(Boxes_model_age)

# Data
stan_ls <- list(
    "N" = nrow(Boxes),
    "S" = 5,
    "y" = Boxes$y,
    majority_first = Boxes$majority_first,
    age = Boxes$age
)

# Stan model
path <- "~/projects/rethinking/rethinking2nd"
file <- file.path(path, "stan", "16", "4.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls, chains=4 , parallel_chains=4)
# Diagnostics
fit$cmdstan_diagnose()
fit$print()



# Posterior draws
postdraws <- fit$draws(format = "matrix")
colnames(postdraws)
colnames(postdraws)

# Data
stan_ls <- list(
    "N" = nrow(Boxes),
    "S" = 5,
    "G" = 2,
    "y" = Boxes$y,
    majority_first = Boxes$majority_first,
    age = Boxes$age,
    gender = Boxes$gender
)

# Hidden state model (gender and age as covariate)
# New!

file <- file.path(path, "stan", "16", "5.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls, chains=4 , parallel_chains=4)
# Diagnostics
fit$cmdstan_diagnose()
fit$print(max_rows = 50)


# 16.3 Ordinariy differential nut cracking
#
library(rethinking)
data(Panda_nuts)

# Prior predictive simulation
N <- 1e4
phi <- rlnorm(N, log(1), 0.1)
k <- rlnorm(N, log(2), 0.25)
theta <- rlnorm(N, log(5), 0.25)

# Relative growth curve
plot(NULL, xlim = c(0,1.5), ylim = c(0,1), xaxt = "n", xlab = "age", 
ylab = "body mass")
at <- seq(0, 1.5, by = 0.25)
axis(1, at = at, labels=round(at*max(Panda_nuts$age)))
for (i in 1:20) curve((1-exp(-k[i]*x))^theta[i], add = TRUE, 
col = col.alpha("black", 0.9))

# Stan
#
stan_ls <- list(
    N = nrow(Panda_nuts),
    n = as.integer(Panda_nuts$nuts_opened),
    age = Panda_nuts$age/max(Panda_nuts$age),
    seconds = Panda_nuts$seconds
)

# Stan model
path <- "~/projects/rethinking/rethinking2nd"
file <- file.path(path, "stan", "16", "6.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls, chains=4 , parallel_chains=4)
# Diagnostics
fit$cmdstan_diagnose()
fit$print(max_rows = 50)

postdraws <- fit$draws(format = "data.frame")
plot(NULL, xlim = c(0,1), ylim = c(0,1.5), xlab = "age", 
ylab = "nuts per second", xaxt = "n")
# Raw data
pts <- stan_ls$n / stan_ls$seconds
point_size <- normalize(stan_ls$seconds)
points( jitter(stan_ls$age, 0.02), pts, col = "steelblue", cex = point_size*3)
for(i in 1:30) { with(postdraws, 
    curve( phi[i] * (1-exp(-k[i]*x))^theta[i], add = TRUE, 
    col = col.alpha("black", 0.9), lwd = 2))
    
}


# 16.4 Population dynamics
#

library(rethinking)
data(Lynx_Hare)
plot(1:21, Lynx_Hare[, 3], ylim = c(0, 100), xlab = "year", 
ylab = "thousands of pelts", xaxt = "n", type = "l", lwd = 2)
at <- c(1,11,21)
axis(1, at = at, labels = Lynx_Hare$Year[at])
lines(1:21, Lynx_Hare[, 2], lwd = 2, col = "steelblue")
points(1:21, Lynx_Hare[, 3])
points(1:21, Lynx_Hare[, 2], col = "steelblue")
text(17, 80, "Lepus", pos = 2)
text(19, 50, "Lynx", pos = 2, col = "steelblue")

# Solve ODE through simulation
#

sim_lynx_hare <- function( n_steps , init , theta , dt=0.002 ) {
    L <- rep(NA,n_steps)
    H <- rep(NA,n_steps)
    L[1] <- init[1]
    H[1] <- init[2]
    for ( i in 2:n_steps ) {
    # theta 1: birth rate of hares, theta 2: mortality rate of hares
    H[i] <- H[i-1] + dt*H[i-1]*( theta[1] - theta[2]*L[i-1] )
    # theta 3: birth rate of lynx, theta 2: mortality rate of lynx 
    L[i] <- L[i-1] + dt*L[i-1]*( theta[3]*H[i-1] - theta[4] )
    }
return( cbind(L,H) )
}

# Visualize
#
theta <- c( 0.5 , 0.05 , 0.025 , 0.5 )
z <- sim_lynx_hare( 1e4 , as.numeric(Lynx_Hare[1,2:3]) , theta )
plot( z[,2] , type="l" , ylim=c(0,max(z[,2])) , lwd=2 , xaxt="n" ,
ylab="number (thousands)" , xlab="" )
lines( z[,1] , col=rangi2 , lwd=2 )
mtext( "time" , 1 )

# Visualize distribution 
#
N <- 1e4
Ht <- 1e4
p <- rbeta(N,2,18)
h <- rbinom(N, Ht, p)
h <- round(h/1000, 2)
plot(density(h), col = "seagreen", lwd = 4)

# Stan model
#
data(Lynx_Hare_model)
cat(Lynx_Hare_model)

stan_ls <- list(
    N = nrow(Lynx_Hare),
    pelts = Lynx_Hare[,2:3]
)

# Stan model
path <- "~/projects/rethinking/rethinking2nd"
file <- file.path(path, "stan", "16", "7.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=stan_ls, chains=4, parallel_chains=4, adapt_delta = 0.95)

# Diagnostics
fit$cmdstan_diagnose()
fit$print(max_rows=200) # 0.26
help("diagnostics", "posterior")

# Posterior draws
postdraws <- fit$draws(format = "data.frame")
pelts_pred <- fit$draws("pelts_pred", format = "matrix")
pop <- fit$draws("pop", format = "matrix")

pelts <- stan_ls$pelts
plot(1:21, pelts[,2], ylim = c(0, 120), xlab = "year", ylab = "pelts", 
xaxt = "n", cex = 3, pch = 20)
at <- c(1,11,21)
axis(1, at = at, labels = Lynx_Hare$Year[at])
points(1:21, pelts[,1], col = "blue", cex = 3, pch = 20)
for(s in 1:21) {
    lines(1:21, pelts_pred[s,1:21], col = col.alpha("black", 0.8))
    lines(1:21, pelts_pred[s,22:42], col = col.alpha("blue", 0.8))
}
plot(NULL, xlim = c(0, 21), ylim = c(0, 500), xlab = "year", ylab = "pelts",
xaxt = "n")
at <- c(1, 11, 21)
axis(1, at = at, labels = Lynx_Hare$Year[at])
for(s in 1:21) {
    lines(1:21, pop[s,1:21], col = col.alpha("black", 0.8))
    lines(1:21, pop[s,22:42], col = col.alpha("blue", 0.8))
}
