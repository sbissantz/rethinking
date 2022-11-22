#
# Ch.12: Monsters and Mixtures
#
library(rethinking)

# Beta-Binomial
#

#
# Data
#
data(UCBadmit)
d <- UCBadmit
#
# Data Wrangling
d$gid <- ifelse(d$applicant.gender=="male",1L, 2L)
#
# Data list 
dat <- list(A=d$admit, N=d$applications, gid=d$gid)
#
# Fit the model
m12.1 <- ulam( 
              alist(
                    A ~ dbetabinom(N, pbar, theta),
                    logit(pbar) <- a[gid],
                    a[gid] ~ dnorm(0, 1.5),
                    transpars> theta <<- phi + 2.0,
                    phi ~ dexp(1)
              ), data=dat, chains=4)
# 
# Show stancode
stancode(m12.1)
# data{
#     int N[12];
#     int A[12];
#     int gid[12];
# }
# parameters{
#     vector[2] a;
#     real<lower=0> phi;
# }
# transformed parameters{
#     real theta;
#     theta = phi + 2;
# }
# model{
#     vector[12] pbar;
#     phi ~ exponential( 1 );
#     a ~ normal( 0 , 1.5 );
#     for ( i in 1:12 ) {
#         pbar[i] = a[gid[i]];
#         pbar[i] = inv_logit(pbar[i]);
#     }
#     A ~ beta_binomial( N , pbar*theta , (1-pbar)*theta );
# }
# 

# Reduced data list
dat_ls <- list("N"=nrow(d),"A"=d$admit, "N_bern"=d$applications, "gid"=d$gid,
               "gno"=length(unique(dat$gid)))

#
# Fit the model
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "12", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
#
# Diagnostics
fit$cmdstan_diagnose()
fit$print(max_rows=30)
#
# Samples
samples <- fit$draws()
# Posterior (reshaped samples)
# post <- aperm(samples, perm=c(1,3,2))
post <- fit$draws(format="matrix")
#
# Postrior contrast
post_contrast <- post[,"a[1]"] - post[,"a[2]"]
#
# Visualize 
#
# Posterior distribution
beta_pkr <- function(x, phi, kappa) {
      alpha <- phi*kappa
      beta <- (1-phi)*kappa
      dbeta(x, shape1=alpha, shape2=beta)
}

plot(NULL, xlim=c(0,1), ylim=c(0,3), ylab="Density", xlab="Probability of
     admission")
for(i in 1:50) {
      phi_i <- plogis(post_contrast[i])
      kappa_i <- post[i,"kappa"]
      curve(beta_pkr(x, phi_i, kappa_i), from=0, to=1, col="steelblue", add=TRUE)
}
phi_bar <- mean(plogis(post_contrast))
kappa_bar <- mean(post[,"kappa"])
curve(beta_pkr(x, phi_bar, kappa_bar), from=0, to=1, lwd=3, add=TRUE)

# PPC
A_tilde <- fit$draws("A_tilde", format="matrix") 
plot(c(1,12), c(0,1), type="n", ylab="Admission", xlab="case", 
main="Posterior validation check")
mtext("89% HPDI")
points(1:12, with(dat_ls, A/N_bern), col="steelblue", pch=20)
for(i in 1:12) {
      mu <- mean(A_tilde[,i]/dat_ls$N_bern[i])
      points(i, mu, pch=20)
      hpdi <- rethinking::HPDI(A_tilde[,i]/dat_ls$N_bern[i])
      lines(rep(i, 2) , c(hpdi[1], hpdi[2]), lty=2)
}

# Gamma-Poisson 
# 
library(rethinking)
data(Kline)
d <- Kline

# Data transformation 
#
# Standardized log population
P_log <- log(d$population)
d$P <- (P_log - mean(P_log)) / sd(P_log)
# d$P <- scale(log(d$population))
# Contact rate id
d$cid <- ifelse(d$contact=="low", 1, 2)

# Reduction
#
N <- nrow(d)
cno <- length(unique(d$cid))
dat_ls <- list("cno"=cno, "N"=N, "T"=d$total_tools, "C"=d$cid,
               "P"=d$population)

# Fit
#
file <- file.path("..", "stan", "12", "2.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
post <- fit$draws(format="matrix")
LL <- fit$draws("log_lik")

# Pareto_k
#
r_eff <- loo::relative_eff(exp(LL))
psis <- loo::loo(LL, r_eff=r_eff, is_method="psis")
pareto_k <- psis$diagnostics$pareto_k

# Visualize
#
N_rep <- nrow(post)
x_lim <- range(d$population) + c(-1e3, 5e4)
y_lim <- range(d$total_tools) + c(-10,10)
cex <- 5*pareto_k ; 
pch <- ifelse(d$cid==1,21, 20)
plot(NULL, xlim=x_lim, ylim=y_lim, ylab="total tools", xlab="population")
for(i in 1:100) {
# high contact
curve((post[i,"alpha[2]"]*x^post[i,"beta[2]"])/post[i, "gamma"],
      from=x_lim[1], to=x_lim[2], col=scales::alpha("cadetblue3", .2), add=TRUE)
# low contact
curve((post[i,"alpha[1]"]*x^post[i,"beta[1]"])/post[i, "gamma"],
      from=x_lim[1], to=x_lim[2], col=scales::alpha("steelblue", .2), add=TRUE)
}
# high contact
curve((mean(post[i,"alpha[2]"])*x^mean(post[i,"beta[2]"]))/mean(post[i, "gamma"]),
      from=x_lim[1], to=x_lim[2], col="cadetblue3", add=TRUE, lwd=4)
# low contact
curve((mean(post[i,"alpha[1]"])*x^mean(post[i,"beta[1]"]))/mean(post[i, "gamma"]),
      from=x_lim[1], to=x_lim[2], col="steelblue", add=TRUE, lwd=4)
points(d$population, d$total_tools,  pch=pch, cex=cex, col="black")
text(d$population+3e4, d$total_tools, as.character(d$culture))

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

#
# Ordered categorical outcomes
#

# 12.3.1 – Example: Moral intuitions
#

library(rethinking)
data(Trolley)
d <- Trolley

# 12.3.2 – Describing and ordered distribution with intercepts
#

# Visualize
#
par(mfrow=c(1,3))

# Outcome 
plot(table(d$response), xlab="response", ylab="Freq.")
# Proportions
pr_k <- table(d$response) / nrow(d)
# Cumulative proportions
cum_pr_k <- cumsum(pr_k)
# Visualize
plot(1:7, cum_pr_k, type="b", ylim=c(0,1), xlab="response", ylab="cumulative
     proportion")
# logit (inverse logistic)
lco <- qlogis(cum_pr_k)
N_lco <- length(lco)
plot(1:7, lco, xlab="response", ylab="log-cumulative-odds", type="b")

# Data reduction
#
N <- nrow(d) 
zero <- rep(0, length=N) 
one <- rep(1, length=N) 
K <- d$response |> unique() |> length()
dat_ls <- list("N" = N, "K" = K, "R" = d$response, "zero" = zero, "one" = one)

# Fit the models
path <- "~/projects/stanmisc/stan"
file <- file.path(path, "12", "mdl_4a.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

path <- "~/projects/stanmisc/stan"
file <- file.path(path, "12", "mdl_4b.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
print(fit)

# Samples
#
post <- fit$draws(format="matrix")
c_mean <- colMeans(post[,-1])
plogis(c_mean)

# 12.3.3 Adding predictor variables
#
pk <- rethinking::dordlogit(1:7, 0, c_mean)
sum(pk*(1:7))
pk_ast <-  rethinking::dordlogit(1:7, 0, c_mean-0.5)
sum(pk_ast*(1:7))

# Data reduction
#
dat_ls <- list("N"=N, "K"=K, "R"=d$response, "I"=d$intention, "A"=d$action,
               "C"=d$contact) 

#
# Model does not work
#

# Fit the model
#
path <- "~/projects/stanmisc/stan"
file <- file.path(path, "12", "mdl_5.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)
print(fit)


# Prior predictive simulation?


