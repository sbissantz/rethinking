data {
  int<lower=0> N;
  row_vector[N] h0;
  vector[N] h1;
  vector[N] T;
}

parameters {
  real<lower=0> alpha;
  real beta_T;
  real<lower=0> sigma;
}

model {
  vector[N] mu; 
  vector[N] p; 
  alpha ~ lognormal(0, 0.25);
  beta_T ~ normal(0, 0.5);
  sigma ~ exponential(1);
  p = alpha + beta_T*T;
  for(i in 1:N) { 
    mu[i] = h0[i]*p[i];
  }
  h1 ~ normal(mu, sigma);
}
