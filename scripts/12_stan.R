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

