stanfit <- rstan::read_stan_csv(fit$output_files())
rstan::traceplot(stanfit)
