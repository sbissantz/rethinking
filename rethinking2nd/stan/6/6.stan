data {
  int<lower=0> N;
  vector[N] h0;
  vector[N] h1;
}

parameters {
  real<lower=0> p;
  real<lower=0> sigma;
}

model {
  vector[N] mu; 
  mu = h0 * p;
  p ~ lognormal(0, 0.25);
  sigma ~ exponential(1);
  h1 ~ normal(mu, sigma);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(h1[n] | h0[n] * p, sigma); 
    }
}
