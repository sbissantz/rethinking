data {
  int<lower=1> n;
  vector[n] K;
  vector[n] M;
}
parameters {
  real alpha;
  real beta_M;
  real<lower=0> sigma;
}
model {
  vector[n] mu;
  alpha ~ normal(0,0.2);
  beta_M ~ normal(0,0.5);
  sigma ~ exponential(1);
  mu = alpha + beta_M * M;
  K ~ normal(mu, sigma);
}