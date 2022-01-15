
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
file <- file.path(getwd(), "stan", "mdl51.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_m51 <- mdl$sample(data = dat_ls)
# Samples 
b_A_m51 <- fit_m51$draws(variables="b_A", format="matrix")

# Model definition 2
#
file <- file.path(getwd(), "stan", "m52.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_m52 <- mdl$sample(data = dat_ls)
# Samples
b_M_m52 <- fit_m52$draws(variables="b_M", format="matrix")

# Model definition 3
#
file <- file.path(getwd(), "stan", "m53.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
dat_ls <- list(N=N, X=cbind(A,M), D=D, K=2)
fit_m53 <- mdl$sample(data = dat_ls)
samples <- fit_m53$draws(format="matrix")
b_A_m53 <- samples[,3]
b_M_m53 <- samples[,4]

# Posterior inference parameter based
#
dlayer <- function(par,label=colnames(par),...) {
  d <- density(par)
  lines(d,...)
  abline(v=mean(par), lty=2)
  text(mean(d$x)+0.05, max(d$y)+0.05, paste0(label), cex = .5)
}
plot(NULL, xlim=c(-2,2), ylim=c(0,5), type="n", xlab="Relative plausibilty", 
     ylab="Density",)
plot_ls <- list(b_M_m52, b_A_m51, b_M_m53, b_A_m53)
mapply(dlayer, plot_ls, c("b_M_m52", "b_A_m51", "b_M_m53", "b_A_m53")) 

#
# Predictor residual plot
#

# Data wrangling
#
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
# Standardization 
d$M <- scale(d$Marriage)
d$A <- scale(d$MedianAgeMarriage)
# Reduced data list 
dat_ls_1 <- list(M=as.numeric(d$M), A=as.numeric(d$A), N=nrow(d))

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
mu <- fit_1$draws(variables="mu",format = "matrix")
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$A - mu_mean

plot(x=d$A, y=d$M)
text(d$A, d$M+.2, labels = abbreviate(d$Location, minlength = 2))
abline(a = mean(alpha), b=mean(beta_M))
for(i in 1:50) lines(x=rep(d$A[i],2), y=c(mu_resid[i],d$M[i]))

