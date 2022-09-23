# 3 Solutions for deconfounding
# 1. Experiments
# 2. Sensitivity analysis
# 3. Measure proxy of confound

#
# Decondound with a proxy
#

# Imagune we had measured a couple of independent test scores for the
# applicants abilities.
# 
T1 <- rnorm(N, u, 0.1)
T2 <- rnorm(N, u, 0.5)
T3 <- rnorm(N, u, 0.25)

# Model sketch
# I.
# A_i ~ Bernoulli(p_i)
# logit(p_i) = Alpha[G[i]],D[i]] + beta_u * u_i
# Alpha ~ normal(0,1)
# beta_u ~ normal(0,1)
# II.
# u_k ~ normal(0,1)
# T_i,j ~ normal(u_i, tau_j)
# Note: could also model as all having the same variance 'tau'
# T_1 ~ norma(u, tau[1])
# T_2 ~ norma(u, tau[2])
# T_3 ~ norma(u, tau[3])

# Data reduction
#
dat_ls <- list(N=N, A=A, D=D, G=G, T1=T1, T2=T2, T3=T3)
dat_ls$gno <- length(unique(G))
dat_ls$dno <- length(unique(D))
dat_ls$tno <- 3 

# Fit the model
#
file <- file.path("..", "stan", "11", "mdl_6e.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$summary("Alpha")

post <- fit$draws(format="data.frame")
ncol(post) ; length(D)
Alpha <- fit$draws("Alpha")

# Contrasts
#
D1_FM_contrast <- Alpha[,,1] - Alpha[,,3]
D2_FM_contrast <- Alpha[,,2] - Alpha[,,4]

# Visualize
#
plot(density(D1_FM_contrast_lo), col="blue", xlim = c(-2,1), lwd=1,
xlab="FM_contast", main="Deconfound the estimates")
lines(density(D2_FM_contrast_lo), col="red", lwd=1)
lines(density(D1_FM_contrast), col="blue", lwd=3)
lines(density(D2_FM_contrast), col="red", lwd=3)
abline(v=0, lty=2)

# Posterior mean u
#
u_est <- fit$draws("u", format="matrix")
mu_u <- colMeans(u_est)

#  Visualize
#
N <- length(mu_u)
j <- rnorm(N,0,0.04)
plot(u+j, mu_u, pch=20, col=ifelse(dat_ls$G==1, "red", "blue"),
     ylab="Posterior mean u", xlab="u (true)") 
mtext("Model correctly detected u, even though prior was uk ~ Normal(0,1)")
