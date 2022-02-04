
# 5.1 Spurious associations -----------------------------------------------

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
file_path <- file.path(getwd(), "stan", "mdl_51.stan")
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

# Visual posterior line inference
#
mu.mean <- apply(mu, 2, mean)
mu.hpdi <- apply(mu, 2, rethinking::HPDI)
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xaxt="n", yaxt="n", 
     xlab="Marriage rate (%)", ylab="Divorce rate (%)", )
points(d$M, d$D, pch=20, col="lightblue") 
lbl_x <- round(sd(d$Marriage) * seq(-2,2), digits = 1)
lbl_y <- round(sd(d$Divorce) * seq(-2,2), digits = 1)
axis(side=1,at = seq(-2,2), labels = lbl_x ) 
axis(side=2,at = seq(-2,2), labels = lbl_y) 
lines(dat_ls$M_seq, mu.mean, lwd=2)
# for( i in seq(dat_ls$K) ) curve(samples[i,"a"] + samples[i,"b_M"] * x, add=TRUE)
# for( i in seq(dat_ls$K) ) points(dat_ls$M_seq, mu[i,], type="l", 
#                                  col=alpha("black",.3))
for( i in seq(dat_ls$K) ) lines(dat_ls$M_seq, mu[i,], col=alpha("black",.4))


# 5.3 ---------------------------------------------------------------------

# DAG
#
dag_5.3 <- dagitty::dagitty('dag {
A [pos="0,0"]
M [pos="1,0"]
D [pos="0.5,1"]
D <- A -> M
}')
plot(dag_5.3)
# Testable implications
dagitty::impliedConditionalIndependencies(dag_5.3)

# Prior predictive simulation
#
# Note: Combine the prior from m5.1 & m5.2
# D ~ normal(mu, sigma)
# mu_i = alpha + beta_M*M + beta_A*A
# alpha ~ normal(0,0.2)
# beta_M ~ normal(0,0.5)
# beta_A ~ normal(0,0.5)
# sigma ~ exponential(1)

# Simulation based calibration
#
# Generative model
alpha <- 0.0                            
beta_M <- 0.5
beta_A <- 0.5
sigma <- .5 
# Simulated data 
N <- 1e2
M <- rnorm(N) 
A <- rnorm(N)
mu <- alpha + beta_M*M + beta_A*A
D <- rnorm(N, mu, sigma)

# Model validation
#
val_dat_ls <- list( N=N, K=2, X=cbind(M,A), D=D )
fml <- file.path(getwd(), "stan", "m53.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(data = val_dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Data wrangling
#
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
d$D <- scale(d$Divorce)
d$M <- scale(d$Marriage)
d$A <- scale(d$MedianAgeMarriage)
X <- cbind( d$M, d$A)
colnames(X) <- c("M", "A")
N_seq <- 100
X_seq <- replicate(2, seq(-3,3,length.out=N_seq))  

# Reduced data list
#
dat_ls <- list(
N = nrow(d),
K = 2,
N_seq = nrow(X_seq),
D = as.numeric(d$D),
X = X,
X_seq=X_seq
)

# Model
#
fml <- file.path(getwd(), "stan", "m53.stan")
mdl <- cmdstanr::cmdstan_model(fml, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
samples <- fit$draws(format = "matrix")
mu <- fit$draws(variables="mu", format = "matrix")
sigma <- fit$draws(variables="sigma", format = "matrix")
alpha <- fit$draws(variables="alpha", format = "matrix")
beta <- fit$draws(variables="beta", format = "matrix")
beta_M <- beta[,1] ; beta_A <- beta[,2]

# Posterior Cormat 
#
cor(beta_M, beta_A)

# Visualize inference
#
# D ~ M | A
plot(d$M, d$D, xlab="Marriage rate", ylab="Divorce rate", pch=20, 
     col=alpha("lightblue", 1))
for(i in seq(N_seq)){
  curve(alpha[i] + beta_M[i] * x, from=min(d$M), to=max(d$M), add=TRUE, 
        col=alpha("black", 0.3))
}
# D ~ A | M
plot(d$A, d$D, xlab="Median Age at Marriage", ylab="Divorce rate", pch=20, 
     col=alpha("lightblue", 1))
for(i in seq(N_seq)){
  curve(alpha[i] + beta_A[i] * x, from=min(d$A), to=max(d$A), add=TRUE, 
        col=alpha("black", 0.3))
}
# Assuming the data are true, the effect of Mariage rate on divorce rate
# vanishes conditioning on median age of marriage.

# Visual inference II 
#
# density plot for one parameter 
dlayer <- function(par,...) {
  d <- density(par)
  lines(d,...)
  abline(v=mean(par), lty=2)
  text(mean(d$x)+0.05, max(d$y)+0.05, paste0(colnames(par)), cex = .5)
}
plot(NULL, xlim=c(-2,2), ylim=c(0,5), type="n", xlab="Relative plausibilty", 
     ylab="Density",)
plot_ls <- list(alpha, beta_A, beta_M)
lapply(plot_ls, dlayer, lwd=2)
# Konklusion: Once we know the median age of marriage in a state there is no 
# additional value of the marriage rate: D _||_ A | M

# Fake data simulation 
#
dag_5.3 <- dagitty::dagitty('dag {
A [pos="0,0"]
M [pos="1,0"]
D [pos="0.5,1"]
D <- A -> M
}')
plot(dag_5.3)
N <- 50
# Simulate A (age_std)
A <- rnorm(N)
# Simulate M (marriage rate_std)
M <- rnorm(N, A)
# Simulate N (divorce rate_std)
D <- rnorm(N, A)
dat_ls <- list(N=N, A=A, M=M, D=D)

# Model definition 1
#
file <- file.path(getwd(), "stan", "mdl_51.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_m51 <- mdl$sample(data = dat_ls)
# Samples 
b_A_m51 <- fit_m51$draws(variables="b_A", format="matrix")

# Model definition 2
#
file <- file.path(getwd(), "stan", "mdl_52.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_m52 <- mdl$sample(data = dat_ls)
# Samples
b_M_m52 <- fit_m52$draws(variables="b_M", format="matrix")

# Model definition 3
#
file <- file.path(getwd(), "stan", "mdl_53.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic = TRUE)
dat_ls <- list(N = N, X = cbind(A, M), D = D, K = 2)
fit_m53 <- mdl$sample(data = dat_ls)
samples <- fit_m53$draws(format = "matrix")
b_A_m53 <- samples[,3]
b_M_m53 <- samples[,4]

# Posterior inference parameter based
#
dlayer <- function(par, label = colnames(par), ...) {
  d <- density(par)
  lines(d,...)
  abline(v = mean(par), lty = 2)
  text(mean(d$x) + 0.05, max(d$y) + 0.05, paste0(label), cex = .5)
}
plot(NULL, xlim = c(-2,2), ylim = c(0,5), type = "n", 
     xlab = "Relative plausibilty",  ylab = "Density",)
plot_ls <- list(b_M_m52, b_A_m51, b_M_m53, b_A_m53)
mapply(dlayer, plot_ls, c("b_M_m52", "b_A_m51", "b_M_m53", "b_A_m53")) 

#
# Predictor residual plots
#

# Data wrangling
#
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
# Standardization 
d$M <- scale(d$Marriage)
d$A <- scale(d$MedianAgeMarriage)
d$D <- scale(d$Divorce)
# Reduced data list 
dat_ls_1 <- list(M=as.numeric(d$M), A=as.numeric(d$A), N=nrow(d))

# Predictor residual plot (1)
# A <- M

# Model (5.4.1)
# A ~ normal(mu, sigma)
# mu_i = a + b_M * M_i
# a ~ normal(0, 0.2)
# b_M ~ normal(0, 0.5)
# sigma ~ exponential(1)

# Fit
#
file <- file.path( getwd(), "stan", "mdl_541.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_1 <- mdl$sample(dat_ls_1) 

# Diagnostics
#
fit_1$cmdstan_diagnose()
fit_1$print()

# Samples
#
samples <- fit_1$draws(format = "matrix")
alpha <- fit_1$draws(variables="alpha",format = "matrix")
beta_M <- fit_1$draws(variables="beta_M",format = "matrix")
mu_mean_line <- mean(alpha)+mean(beta_M) * d$M

# Visualize (A ~ M)
#
plot(x=d$M, y=d$A, pch=20, xlab="Marriage rate (std)", ylab="Age at marriage (std)")
text(x=d$M+0.05, y=d$A+0.05,  labels = abbreviate(d$Location, minlength = 2))
text(1, 1, "observed > predicted: model underpredicts")
text(-1, -1, "predicted > observed: model overpredicts")
abline(a = mean(alpha), b=mean(beta_M))
for(i in 1:50) lines(x=rep(d$M[i],2), y=c(d$A[i],mu_mean_line[i]),
                     col=alpha("black", .3))

# Age at marriag residuals
#
calc_mu <- function( M ) {
  alpha + beta_M * M
}
# Predicted median age 
mu <- vapply( d$M, calc_mu, double(nrow(samples)) )
mu_mean <-apply(mu, 2, mean)
# Observed (A) - predicted (A)
mu_resid_A <- d$A - mu_mean


# Predictor residual plot (2)
# M <- A

# Model (5.4.2)
# M ~ normal(mu, sigma)
# mu_i = alpha + beta_A*A_i  
# alpha ~ normal(0,0.2)
# beta_A ~ normal(0,0.5)
# sigma ~ exponential(1)

# Fit
#
file <- file.path( getwd(), "stan", "mdl_542.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_2 <- mdl$sample(dat_ls_1)

# Diagnostics
#
fit_2$cmdstan_diagnose()
fit_2$print()
samples <- fit_2$draws(format="matrix")
bayesplot::mcmc_trace(samples)

# Samples
#
alpha <- samples[,"alpha"] 
beta_A <- samples[,"beta_A"] 

# Visualize (M ~ R)
#
plot( x=d$A, y=d$M, xlab="Median Age at Marriage", ylab="Marriage rate", pch=20 )
text( d$A+0.05, d$M+0.05, abbreviate(d$Location, 2) )
abline( a=mean(alpha), b=mean(beta_A), lty=2 )
text(1, 1, "observed > predicted: model underpredicts")
text(-1, -1, "predicted > observed: model overpredicts")
mu_mean_line <- mean(alpha) + mean(beta_A) * d$A
for(i in seq(50)) { 
  lines( rep(d$A[i], 2), c(d$M[i], mu_mean_line[i]), col=alpha("black", .3) )
}

# Marriage rate residuals
#
calc_mu <- function( A ) {
  alpha + beta_A * A
}
# Predicted median age 
mu <- vapply( d$A, calc_mu, double(nrow(samples)) )
mu_mean <-apply(mu, 2, mean)
# Observed (A) - predicted (A)
mu_resid_M <- d$M - mu_mean

# Joint model (5.4.3)
# D ~ normal(mu, sigma)
# mu = alpha + beta_M * M + beta_A * A
# alpha ~ normal(0, 0.2)
# beta_A ~ normal(0, 0.5)
# beta_M ~ normal(0, 0.5)
# sigma ~ exponential(1)

# Fitting 
#
file <- file.path( getwd(), "stan", "mdl_543.stan" )
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
dat_ls <- list(M=as.numeric(d$M), A=as.numeric(d$A), D=as.numeric(d$D),
                 N=nrow(d))
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()
samples <- fit$draws(format="matrix")
bayesplot::mcmc_trace(samples)

# Samples
#
alpha <- samples[,"alpha"] 
beta_A <- samples[,"beta_A"] 
beta_M <- samples[,"beta_M"] 

par(mfrow=c(1,2))
# Predictor residual plots (D ~ A_resid)
#
plot(d$D ~ mu_resid_M, ylab="Divorce rate (std)", xlab="Age at Marriage residuals", 
     pch=20, col="lightblue")
abline(v=0, lty=2)
abline(a = mean(alpha), b=mean(beta_M), lwd=2)
for(i in 1:100) abline(a = alpha[i], b=beta_M[i], col=alpha("black", 0.3))

# Predictor residual plot (D ~ M_resid)
#
plot(d$D ~ mu_resid_A, ylab="Divorce rate (std)", xlab="Marriage rate residuals", 
     pch=20, col="lightblue")
abline(v=0, lty=2)
abline(a = mean(alpha), b=mean(beta_A), lwd=2)
for(i in 1:100) abline(a = alpha[i], b=beta_A[i], col=alpha("black", 0.3))
par(mfrow=c(1,1))

#
# Posterior predictive plots
#

# Data wrangling
#
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
# Standardization 
d$M <- scale(d$Marriage)
d$A <- scale(d$MedianAgeMarriage)
d$D <- scale(d$Divorce)
# Reduced data list 
dat_ls <- list(N=nrow(d), K=2, D=as.numeric(d$D), X=cbind(as.numeric(d$A), 
                                                          M=as.numeric(d$M)))

# Model fitting
#
file <- file.path( getwd(), "stan", "mdl_53_pp.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample( data = dat_ls )

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
mu <- fit$draws(variables = "mu", format = "matrix")
D_tilde <- fit$draws(variables = "D_tilde", format = "matrix")

# Parameter preparation
#
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, rethinking::HPDI)
D_tilde_HPDI <- apply(D_tilde, 2, rethinking::HPDI)

# Posterior predictive plots
#
plot(mu_mean ~ d$D, ylab = "Predicted divorce", xlab = "Observed divorce", 
     pch = 20, col = "lightblue")
abline(a = 0, b = 1, lty = 2)  
# Posterior line uncertainty 
for (i in seq(1:nrow(d))) lines(rep(d$D[i], 2), mu_HPDI[, i], 
                                 col = alpha("black", 0.7))
# Posterior uncertainty 
for (i in seq(1:nrow(d))) lines(rep(d$D[i], 2), D_tilde_HPDI[,i], 
                                 col = alpha("black", 0.3))
# Identify some states
# identify(d$D, mu_mean, labels = d$Loc) 

# Counterfactual plots
#
# Assuming the dag... 
dag <- dagitty::dagitty('dag{
A [pos="0,0"]
M [pos="1,0"]
D [pos="0.5,01"]
A -> D <- M
A -> M 
                        }')
plot(dag)

# Causal System 
# (conditional on the DAG)
# Stat model 1
# D ~ normal(mu, sigma)
# mu_i = a + b_AD*A + b_MD*M 
# a ~ normal(0,0.2)
# b_AD ~ normal(0,0.5)
# b_MD ~ normal(0,0.5)
# sigma ~ exponential(1)
# Stat model 2
# M ~ normal(nu, tau)
# nu = k + b_AM*A
# tau ~ exponential(1)

# Data wrangling
#
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce
d$D <- scale(d$Divorce)
d$A <- scale(d$MedianAgeMarriage)
d$M <- scale(d$Marriage)
N = nrow(d)
s <- seq(-2,2, length.out=N)
# Reduced data list
dat_ls <- list(
  D=as.numeric(d$D),  A=as.numeric(d$A),  M=as.numeric(d$M), 
  A_seq=as.numeric(s),  M_seq=as.numeric(s), N = N)

# Fitting
#
file <- file.path(getwd(), "stan", "sys_53.stan")
sys <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- sys$sample(dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Samples
#
D_tilde <- fit$draws(variables="D_tilde",format="matrix")
M_tilde <- fit$draws(variables="M_tilde",format="matrix")
D_tilde_A0 <- fit$draws(variables="D_tilde_A0",format="matrix")

par(mfrow=c(3,1))
# Total causal effect of A on D
plot( c(-2,2), c(-2,2), type="n", xlab="Manipulated A", ylab="Counterfactual D")
title("Total causal effect of A on D")
mtext("A on D & A on M on D")
lines(s, colMeans(D_tilde), pch=20, lwd=2)
for (i in seq(s)) {
  lines(s, D_tilde[i,], pch=20, col=alpha("black", 0.2))
}
# Direct causal effect of A on D
plot( c(-2,2), c(-2,2), type="n", xlab="Manipulated A", ylab="Counterfactual M")
title("Direct causal effect of A on D")
lines(s, colMeans(M_tilde), pch=20, lwd=2)
for (i in seq(s)) {
  lines(s, D_tilde[i,], pch=20, col=alpha("black", 0.2))
}
# Total causal effect of M on D
plot( c(-2,2), c(-2,2), type="n", xlab="Manipulated M", ylab="Counterfactual D")
title("Total causal effect of M on D")
lines(s, colMeans(D_tilde_A0), pch=20, lwd=2)
for (i in seq(s)) {
  lines(s, D_tilde_A0[i,], pch=20, col=alpha("black", 0.2))
}

# 5.2 Masked relationships ------------------------------------------------

library(rethinking)
data(milk)
d <- milk
str(d)

# DAG
#
# dag <- dagitty::dagitty('dag{
# N [pos="0,0"]
# K [pos="0,1"]
# M [pos="1,1"]
# K -> N <- M;
# }')
# plot(dag)
# Data wrangling
#
d$K <- scale(d$kcal.per.g)
d$N <- scale(d$neocortex.perc)
d$M <- scale(log(d$mass))

# mdl_55 ------------------------------------------------------------------

# Sketch 
#
# K_i ~ normal(mu_i, sigma) 
# mu_i = alpha + beta_N * N 
# alpha ~ normal(0,0.2)
# beta ~ normal(0,0.5)
# sigma ~ exponential(1)

# PPS
#
N <- 1e3
N_seq <- seq(-2,2, length.out=N) 
alpha <- rnorm(N, 0,0.2)
beta <- rnorm(N, 0,0.5) 

# PPS Visualization
# (standardized)
#
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab="Neocortex percent (std)", 
     ylab="Milk energy density in k_cal (std)")
for(i in 1:100) abline(a = alpha[i], b = beta[i], col=alpha("steelblue", 0.3), 
                       lwd=2)

# PPS Visualization
# (unstandardized)
#
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab="Neocortex percent (std)", 
     ylab="Milk energy density in k_cal", xaxt="n", yaxt="n")
for(i in 1:100) abline(a = alpha[i], b = beta[i], col=alpha("steelblue", 0.3), 
                       lwd=2)
lbl_x <- round(sd(d$neocortex.perc, na.rm = TRUE) * seq(-2,2), digits = 1)
axis(side=1,at = seq(-2,2), labels = lbl_x ) 
lbl_y <- round(sd(d$kcal.per.g, na.rm = TRUE) * seq(-2,2), digits = 1)
axis(side=2,at = seq(-2,2), labels = lbl_y ) 

# Reduction 
#
# Note: complete case analysis
d <- d[complete.cases(d$N),]
# d <- d[complete.cases(d$N, d$K, d$M),]
dat_ls <- list(n=nrow(d), N=as.numeric(d$N), K=as.numeric(d$K))

# Definition 
#
file <- file.path(getwd(), "stan", "mdl_55.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Sampling
#
samples <- fit$draws(format="data.frame")

# Visual inference
#
plot(d$K ~ d$N, pch=20, col=alpha("black", 0.5), xlab="Neocortex percent (std)", 
     ylab="Milk energy density in k_cal") 
for(i in 1:1e2) {
  abline(a=samples$alpha[i], b=samples$beta_N[i], col=alpha("steelblue", .3),
         lwd=2)
}

# mdl_56 ------------------------------------------------------------------

# Sketch 
#
# K_i ~ normal(mu_i, sigma) 
# mu_i = alpha + beta_M * M
# alpha ~ normal(0,0.2)
# beta ~ normal(0,0.5)
# sigma ~ exponential(1)

# PPS
#
N <- 1e3
N_seq <- seq(-2,2, length.out=N) 
alpha <- rnorm(N, 0,0.2)
beta <- rnorm(N, 0,0.5) 

# PPS Visualization
# (standardized)
#
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab="Log body mass (std)", 
     ylab="Milk energy density in k_cal (std)")
for(i in 1:100) abline(a = alpha[i], b = beta[i], col=alpha("steelblue", 0.3), 
                       lwd=2)

# PPS Visualization
# (unstandardized)
#
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab="Log body mass (std)", 
     ylab="Milk energy density in k_cal", xaxt="n", yaxt="n")
for(i in 1:100) abline(a = alpha[i], b = beta[i], col=alpha("steelblue", 0.3), 
                       lwd=2)
lbl_x <- round(sd(d$mass, na.rm = TRUE) * seq(-2,2), digits = 1)
axis(side=1,at = seq(-2,2), labels = lbl_x ) 
lbl_y <- round(sd(d$kcal.per.g, na.rm = TRUE) * seq(-2,2), digits = 1)
axis(side=2,at = seq(-2,2), labels = lbl_y ) 

# Reduction 
#
# Note: complete case analysis
d <- d[complete.cases(d$M, d$K),]
dat_ls <- list(n=nrow(d), M=as.numeric(d$M), K=as.numeric(d$K))

# Definition 
#
file <- file.path(getwd(), "stan", "mdl_56.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Sampling
#
samples <- fit$draws(format="data.frame")

# Visual inference
#
plot(d$K ~ d$M, pch=20, col=alpha("black", 0.5), xlab="Log body mass (std)", 
     ylab="Milk energy density in k_cal") 
with(samples, {
  for(i in 1:1e2) { 
    abline(a=alpha[i], b=beta_M[i], col=alpha("steelblue", .3), lwd=2)
  }})

# mdl_57 ------------------------------------------------------------------

# Sketch 
#
# K_i ~ normal(mu_i, sigma) 
# mu_i = alpha + beta_M * M + beta_N *N
# alpha ~ normal(0,0.2)
# beta ~ normal(0,0.5)
# sigma ~ exponential(1)

# Reduction 
#
# Note: complete case analysis
d <- d[complete.cases(d$M, d$K, d$K),]
dat_ls <- list(n=nrow(d), N=as.numeric(d$N), M=as.numeric(d$M), 
               K=as.numeric(d$K))

# Definition 
#
file <- file.path(getwd(), "stan", "mdl_57.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Sampling
#
samples <- fit$draws(format="data.frame")

# Posterior correlations
#
pairs(~K+N+M, dat_ls)
with(samples, cor(beta_M, beta_N))

# Visual inference
#
par(mfrow=c(2,1))
plot(d$K ~ d$M, pch=20, col=alpha("black", 0.5), xlab="Log body mass (std)", 
     ylab="Milk energy density in k_cal") 
with(samples, {
  for(i in 1:1e2) { 
    abline(a=alpha[i], b=beta_M[i], col=alpha("steelblue", .3), lwd=2)
  }})
plot(d$K ~ d$N, pch=20, col=alpha("black", 0.5), xlab="Neocortex percent (std)", 
     ylab="Milk energy density in k_cal") 
with(samples, {
  for(i in 1:1e2) { 
    abline(a=alpha[i], b=beta_N[i], col=alpha("steelblue", .3), lwd=2)
  }})
par(mfrow=c(1,1))

# Counterfactual plots
#
n <- 1e3
calc_mu <- function(N, M) {
  with(samples, alpha + beta_N*N + beta_M*M)
}
sim_K <- function(n, N, M) {
  with(samples, {
    rnorm(n,
          mean=alpha + beta_N*N + beta_M*M,
          sd=sigma )
    })
}
x_seq <- seq(-3,3, length.out=n)
  
# Assuming N=0
#
mu_N0 <- mapply(calc_mu, M=x_seq, N=0)
K_tilde_N0 <- mapply(sim_K, n, M=x_seq, N=0)
mu_N0_mean <- colMeans(mu_N0)

par(mfrow=c(2,1))
plot(d$K ~ d$N, pch=20, col=alpha("black", .3))
for(i in 1:1e2) lines(x_seq, K_tilde_N0[i,], col=alpha("steelblue", .1))
for(i in 1:1e2) lines(x_seq, mu_N0[i,], col=alpha("white", .3), lwd=2)
lines(x_seq, mu_N0_mean, lwd=3)

# Assuming M=0
#
mu_M0 <- mapply(calc_mu, N=x_seq, M=0)
K_tilde_M0 <- mapply(sim_K, n, N=x_seq, M=0)
mu_M0_mean <- colMeans(mu_M0)

plot(d$K ~ d$N, pch=20, col=alpha("black", .3))
for(i in 1:100) lines(x_seq, K_tilde_M0[i,], col=alpha("steelblue", .1))
for(i in 1:100) lines(x_seq, mu_M0[i,], col=alpha("white", .4), lwd=2)
lines(x_seq, mu_M0_mean, lwd=3)
par(mfrow=c(1,1))

# 5.3 Categorical variables -----------------------------------------------

library(rethinking)
data("Howell1")
d <- Howell1
str(d)

# Model sketch (indicator variable)
# height_i ~ normal(mu_i, sigma)
# mu_i = alpha + beta_M * M_i
# alpha ~ normal(178, 20)
# beta_m ~ normal(0, 10)
# sigma ~ uniform(0, 50)

# PPS
#
n <- nrow(d) 
alpha <- rnorm(n, 178, 20)
beta_m <- rnorm(n, 0, 10)
mu <- alpha + beta_m * d$male
# Summarize
mean(mu[d$male==1])
mean(mu[d$male==0])
# Visualize
# Expected difference between male and female heights 
plot(NULL, ylim=c(0, 250), xlim=c(0,1), ylab="Height (cm)", xlab="Male", 
     pch=20, xaxt="n")
axis(1, at=c(0,1))
for(i in 1:100) abline(a=alpha[i], b=beta_m[i], col=alpha("steelblue", 0.3))
# Alternative coding
mu_female <- rnorm(n, 178, 20)
mu_male <- rnorm(n, 178, 20) + rnorm(n, 0, 10)
rethinking::precis(data.frame(mu_female, mu_male))

# Model sketch (index variable)
# height_i ~ normal(mu_i, sigma)
# mu_i = alpha_sex[i] 
# alpha_sex[i] ~ normal(178, 20)
# sigma ~ uniform(0, 50)

# Data wrangling
#
# Ladys first coding
d$sex <- d$male + 1
d$H <- scale(d$height)

# Reduction
#
dat_ls <- list(N=nrow(d), K=2, S=as.integer(d$sex), H=as.numeric(d$H))

# Fitting
#
file <- file.path(getwd(), "stan", "mdl_58.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data = dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$print()

# Sampling
#
samples <- fit$draws(format = "matrix")

# Summary
#
# female - male
mu_contrast <- samples[, "alpha[1]"] - samples[, "alpha[2]"]
plot(density(mu_contrast))


# PPD 
#
N <- 1e3
ppd_female <- rnorm(N, samples[,"alpha[1]"], samples[,"sigma"])
ppd_male <- rnorm(N, samples[,"alpha[2]"], samples[,"sigma"])
height_contrast <- ppd_female - ppd_male
plot(density(height_contrast))

# Visualize
#









