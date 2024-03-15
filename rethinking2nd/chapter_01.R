
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

#
# Stan
#

# Generative process
#
N <- 1e3
y <- rbinom(N, 1, prob=.73)

# data list
dat_ls <- list(
               y = y,
               N = N
               )

model.stan <- "
data{
    int<lower=0> N;
    int<lower=0, upper=1> y[N];
}
parameters{
    real<lower=0, upper=1> p;
}
model{
    y ~ bernoulli(p);
    p ~ normal(.6, 0.1);
}"
fit <- rstan::stan(model_code = model.stan, data = dat_ls)
print(fit)
summary(fit)
plot(fit)
pairs(fit)
rstan::traceplot(fit)

samples <- rstan::extract(fit)

# Visualization
#
plot(density(samples$p), main=NA)
boundaries <- c(0.71, 0.73)
abline(v = boundaries)
