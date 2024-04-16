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

# Hidden state model

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

# Hidden state model

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

# TODO: DO the age model

data(Boxes_model_age)
cat(Boxes_model_age)
