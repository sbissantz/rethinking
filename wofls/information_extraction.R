
# Information extraction
#
N <- 1e4
alpha_prior <- rnorm(N, 0, 0.2)
beta_prior <- rnorm(N, 0, 0.5) 

plot(beta_L_d, xlim=c(-2,2)) 
lines(beta_F_d, lty=2) ; lines(density(beta_prior), lty=2)
