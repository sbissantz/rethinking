#
#
# Poisson regression 
#
#
library(rethinking)
data(Kline)
d <- Kline

# Data transformation 
#
# Standardized log population
P_log <- log(d$population)
d$P <- (P_log - mean(P_log)) / sd(P_log)
# Contact rate id
d$cid <- ifelse(d$contact=="low", 1, 2)

# Model sketch
#
# T_i ~ Poission(lambda_i)
# log(lambda_i) = alpha[CID[i]] + beta[CID[i]] * log P_i #IA: Popl & Tools
# alpha_j ~  
# beta_j ~  

# PPS

# alpha
curve(dlnorm(x, 3, .5), from=-1, to=100, n=200, xlab="mean number of tools")
#
# Simulate the expected number of tools (mean)
#
# log(lambda) = alpha
N <- 1e3
alpha <- rnorm(N, 3, 0.5)
# ...thereofre, lambda = exp(alpha)
lambda <- exp(alpha)
mean(lambda)
#
# Shorty: log(lambda) = alpha
# If alpha has a normal, lambda has a log-normal
lambda <- rlnorm(N, 3, 0.5)
mean(lambda)
#
# Even shorter: mean of a log-normal distribution
exp(3 + 0.5^2 /2)

# beta (coefficient of log-population)
N <- 1e2
alpha <- rnorm(N, 3, 0.5)
beta <- rnorm(N, 0, 0.25)
#
# Visualize on the log standardized scale
#
plot(NULL, xlim=c(-2,2), ylim=c(0,100), ylab="total tools",
     xlab="log-population (std)")
for (i in 1:N) curve(exp(alpha[i] + beta[i] * x), add=TRUE, col=grau())
# Mean of beta
exp(0 + 0.25^2/2)
# log(lambda) = alpha + beta*x, thereofre lambda = exp(alpha + beta *x)
alpha <- rnorm(N, 3, 0.5)
beta <- rnorm(N,0, 0.25)
lambda <- exp(alpha + beta)
mean(lambda)
#
# Visualize on the log scale (unstandardized)
#
x_seq <- seq(from=log(1e2), to=log(2e4), length.out=1e2)
lambda <- sapply(x_seq, function(x) exp( alpha + beta*x))
plot(NULL, xlim=range(x_seq), ylim=c(0,500), xlab="log population", ylab="total tools")
for(i in 1:N) lines(x_seq, lambda[i,], col=grau(), lwd=1.5)
#
# Visualize on the natural scale (un-log)
#
plot(NULL, xlim=range(exp(x_seq)), ylim=c(0,500), xlab="population", ylab="total tools")
for (i in 1:N) lines(exp(x_seq), lambda[i,], col=grau())

# Model sketch
#
# T_i ~ Poission(lambda_i)
# log(lambda_i) = alpha[CID[i]] + beta[CID[i]] * log P_i #IA: Popl & Tools
# alpha_j ~  normal(3, 0.5)
# beta_j ~ normal(0, 0.2) 

# Reduction
#
N <- nrow(d)
cno <- length(unique(d$cid))
data_ls <- list(cno=cno, N=N, T = d$total_tools, C=d$cid, P=d$P)

# Fit
#
file <- file.path("..", "stan", "11", "mdl_7.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=data_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$summary()

# Samples 
#
post <- fit$draws(c("alpha", "beta_P"), format="matrix")
log_lambda <- fit$draws("log_lambda", format="matrix")
LL <- fit$draws("log_lik") 

#lambda_c1 <- exp(log_lambda_c1) ; lambda_c2 <- exp(log_lambda_c2) 
#lambda_c1_mu <- colMeans(lambda_c1) ; lambda_c2_mu <- colMeans(lambda_c2)

# Model comparison & Influential observations 
# Note: I dont want to compare models but see influential points
#
r_eff <- loo::relative_eff(exp(LL))
psis <- loo::loo(LL, r_eff=r_eff, is_method="psis")
pareto_k <- psis$diagnostics$pareto_k
plot(d$P, pareto_k, pch=20, ylim=c(0,1), xlim=c(-1.5, 2.5),
ylab="Pateo k values", xlab="log population (std)")
text(d$P+0.1, pareto_k+0.025, as.character(d$culture))

# Interesting: p_loo
#
psis$estimates

# Visualization
#
N_rep <- nrow(post)
plot(NULL, xlim=range(d$P)+c(-1,.5), ylim=range(d$total_tools) + c(-5,5),
     pch=20, cex=5*pareto_k, ylab="total tools", xlab="log population (std)")
for(i in 1:100) {
# high contact
curve( exp(post[[i,"alpha[2]"]] + post[[i,"beta_P[2]"]] * x), 
      from=-3, to=3, col=scales::alpha("cadetblue3", .2), add=TRUE)
curve( exp(post[[i,"alpha[1]"]] + post[[i,"beta_P[1]"]] * x), 
      from=-3, to=3, col=scales::alpha("steelblue", .2), add=TRUE)
}
curve( exp(mean(post[[i,"alpha[2]"]]) + mean(post[[i,"beta_P[2]"]]) * x), 
      from=-3, to=3, col="cadetblue3", add=TRUE, lwd=4)
curve( exp(mean(post[[i,"alpha[1]"]]) + mean(post[[i,"beta_P[1]"]]) * x), 
      from=-3, to=3, col="steelblue", add=TRUE, lwd=4)
points(d$P, d$total_tools,  pch=20, cex=5*pareto_k, col="black")
text(d$P+0.1, d$total_tools+3, as.character(d$culture))

# On the natural scale
#
N_rep <- nrow(post)
x_lim <- range(d$population) + c(-1e3, 5e4)
y_lim <- range(d$total_tools) + c(-5,5)
cex <- 5*pareto_k ; 
pch <- ifelse(d$cid==1,21, 20)
plot(NULL, xlim=x_lim, ylim=y_lim, ylab="total tools", xlab="population")
P_seq <- seq( from=-5, to=3, length.out=N_rep )
mu_log_popl <- mean(log(d$population))
sd_log_popl <- sd(log(d$population))
# Unstandardize
pop_seq <- exp(P_seq * sd_log_popl + mu_log_popl)
# high contact
lambda_c1 <- sapply(P_seq, function(x) 
                    rpois(N_rep, exp(post[,"alpha[1]"] + 
                                     post[,"beta_P[1]"] * x)))
lambda_c1_mu <- colMeans(lambda_c1)
lambda_c1_HPDI <- apply(lambda_c1, 2, rethinking::HPDI)
# low contact 
lambda_c2 <- sapply(P_seq, function(x) 
                    rpois(N_rep, exp(post[,"alpha[2]"] + 
                                     post[,"beta_P[2]"] * x)))
lambda_c2_mu <- colMeans(lambda_c2)
lambda_c2_HPDI <- apply(lambda_c2, 2, rethinking::HPDI)
# Polygon 
pop_seq_rev <- pop_seq[seq(length(pop_seq),1)]
# High contact polyon 
y <- c(lambda_c2_HPDI[1, ], lambda_c2_HPDI[2, ][seq(ncol(lambda_c2_HPDI),1)])
x <- c(pop_seq, pop_seq_rev)
col <- scales::alpha("steelblue", .2)
polygon(x, y, col=col, border=col) 
# Low contact polygon
y <- c(lambda_c1_HPDI[1, ], lambda_c1_HPDI[2, ][seq(ncol(lambda_c1_HPDI),1)])
x <- c(pop_seq, pop_seq_rev)
col <- scales::alpha("cadetblue3", .2)
polygon(x, y, col=col, border=col) 
# Mean lines
lines(pop_seq, lambda_c2_mu, col="steelblue", lwd=3)
lines(pop_seq, lambda_c1_mu, col="cadetblue3", lwd=3)
# Points & text
points(d$population, d$total_tools,  pch=pch, cex=cex, col="black")
text(d$population+3e4, d$total_tools, as.character(d$culture))

#
# Scientific model
#

# Reduction
#
N <- nrow(d)
cno <- length(unique(d$cid))
dat_ls <- list(cno=cno, N=N, T = d$total_tools, C=d$cid, P=d$population)

# Fit
#
file <- file.path("..", "stan", "11", "mdl_8.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$summary()

# Posterior
#
post <- fit$draws(format="matrix")

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
# Example: Exposure and offset.
#
# Monastery I
N_days <- 30
# Simulate a typical month
lambda_1 <- 1.5
y_1 <- rpois(N_days, lambda_1)
# Monastery II
N_weeks <- 4
lambda_2 <- 0.5
y_2 <- rpois(N_weeks, lambda_2 * 7)

# Data set
#
y <- c(y_1, y_2)
exposure <- c(rep(1,30) , rep(7,4))
monastery <- c(rep(0,30) , rep(1,4))
d <- data.frame(y = y, days=exposure, monastery=monastery)

# Reduction
#
dat_ls <- list(N=nrow(d), y=y, t=exposure, M=monastery)

# Fit
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_9.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$summary()

# Posterior
#
post <- fit$draws(format="matrix")

# Dont use the offset for predictions, because the thetas are already on the
# daily scale!
lambda_old = exp(post[,"alpha"])
colMeans(lambda_old)
# 1.510594 
lambda_new = exp(post[,"alpha"] + post[,"beta_M"])
colMeans(lambda_new)
# 1.047983 

lambda_diff <- lambda_old - lambda_new 
(lambda_diff_mu <- colMeans(lambda_diff))
# 0.3958329 
(lambda_diff_HPDI <- apply(lambda_diff, 2, rethinking::HPDI))
