#
# Zero-inflated  Poisson 
#

# Generative simulation
#
# Bernoulli process (drink vs. work)
prob_drink <- .2 #20% of the days monks drink, 80% the work
rate_work <- 1  # Average of 1 manuscript per day
# Sample 1 year of production
N <- 365
# Simulate days monk drinks
set.seed(365)
# Drink indicator (decision: drink 0/1)
drink <- rbinom(N, 1, prob_drink) # Bernoulli
# Simulate completed manuscripts
y <- (1-drink)*rpois(N, rate_work)
plot(table(y), xlab="manuscripts completed")

# Model sketch
#
# y_i ~ ZIPoisson(p_i, lambda_i)
# logit(p) = X_i beta_p + alpha_p
# log(lambda) = X_i beta_l + alpha_l
# a_p ~ normal(-1, 0.5)
# a_l ~ normal(0, 0.5)

# PPS
#
# inv_logit: logistic function
logit_alpha <- rnorm(N, -1 , 0.5 );
alpha_p <- plogis(logit_alpha)
hist(alpha_p, main="Average probability of drinking")
mtext(paste0("mean= ", round(mean(alpha_p), digits=2)))

N <- 1e3
lambda_log <- rnorm(N, 0, 0.5)
# inv_log: exp function
lambda <- exp(lambda_log)
hist(lambda, main="Average n° manuscripts per day")
mtext(paste0("mean= ", round(mean(lambda), digits=2)))

# Data
#
dat_ls <- list("N"=length(y), "y"=as.integer(y))

# Fit the model
#
# Ulam
m12.4_ulam <- rethinking::ulam(
                  alist(
                        y|y>0 ~ custom(log1m(p) + poisson_lpmf(y|lambda)),
                        y|y==0 ~ custom(log_mix(p, 0, poisson_lpmf(0|lambda))),
                        logit(p) <- a_p,
                        log(lambda) <- a_l,
                        a_p ~ dnorm(-1.5,1),
                        a_l ~ dnorm(1,0.5)
                  ), data=list("y"=as.integer(y)), chains=4)
rethinking::precis(m12.4_ulam)

# Adapted Stan version
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_14.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
print(fit)
#    variable    mean  median   sd  mad      q5     q95 rhat ess_bulk ess_tail
#  a_p          -1.27   -1.22 0.37 0.32   -1.95   -0.77 1.00     1215     1010
#  a_l           0.01    0.01 0.09 0.09   -0.14    0.16 1.00      970     1136
plogis(-1.27)
# [1] 0.2192573
exp(0.01)
# [1] 1.01005

# Stan (more direct version)
# 
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_14b.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
print(fit)
#  variable    mean  median   sd  mad      q5     q95 rhat ess_bulk ess_tail
#    p         0.23    0.23 0.06 0.06    0.13    0.32 1.00     1299     1438
#    lambda    1.00    1.00 0.09 0.09    0.86    1.16 1.00     1209     1547

# Stan (more direct & faster version from the Stan User Guide)
# 
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_14c.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
print(fit)
#  variable    mean  median   sd  mad      q5     q95 rhat ess_bulk ess_tail
#    p         0.23    0.23 0.06 0.06    0.13    0.32 1.00     1574     1628
#    lambda    0.99    0.99 0.09 0.09    0.85    1.15 1.00     1442     2032
