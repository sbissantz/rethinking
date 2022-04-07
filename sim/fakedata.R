
# Spurious correlation ----------------------------------------------------

# Spurious correlation
#
sim_spur_cor_dat_ls <- function(N=1e3, seed=NULL) {
  if(!is.null(seed)) { 
    set.seed(seed)
  }
 x_real <- rnorm(N) 
 x_spur <- rnorm(N, x_real) 
 y <- rnorm(N, x_real)
 msg <- "
 Functional relationsships:
 x_real
 x_spur = f(x_real)
 y = f(x_real)
 "
 message(msg)
 list(x_real=x_real, x_spur=x_spur, y=y, N=N)
}
dat_ls <- sim_spur_cor_dat_ls(N=1e3, seed=123)
df <- as.data.frame(dat_ls[-4]) ; cor(df)
pairs(df)

# Fit
#
file <- file.path( getwd(), "stan", "mdl_1_spur.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_1 <- mdl$sample(dat_ls)
fit_1$print()
beta_real_1 <- fit_1$draws(variables = "beta", format = "matrix")

# Fit 2
#
file <- file.path( getwd(), "stan", "mdl_2_spur.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_2 <- mdl$sample(dat_ls)
fit_2$print()
beta_fake_1 <- fit_2$draws(variables = "beta", format = "matrix")

# Fit 3
#
file <- file.path( getwd(), "stan", "mdl_3_spur.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit_3 <- mdl$sample(dat_ls)
fit_3$print()
beta_real_2 <- fit_3$draws(variables = "beta_real", format = "matrix")
beta_fake_2 <- fit_3$draws(variables = "beta_spur", format = "matrix")

# Posterior inference 
#
dlayer <- function(par,label=colnames(par),...) {
  d <- density(par)
  lines(d,...)
  abline(v=mean(par), lty=2)
  text(mean(d$x)+0.05, max(d$y)+0.05, paste0(label), cex = .5)
}
plot(NULL, xlim=c(-0.5,1.5), ylim=c(0,18), type="n", xlab="Relative plausibilty", 
     ylab="Density",)
plot_ls <- list(beta_real_1 = beta_real_1,  beta_fake_1 = beta_fake_1, 
                beta_real_2 = beta_real_2,  beta_fake_2 = beta_fake_2)
mapply(dlayer, plot_ls, as.list(names(plot_ls)))
