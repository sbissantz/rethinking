data {
  int<lower=1> n;
  vector[n] K;
  vector[n] M;
  vector[n] N;
}
parameters {
  real alpha;
  real beta_M;
  real beta_N;
  real<lower=0> sigma;
}
model {
  vector[n] mu;
  alpha ~ normal(0,0.2);
  beta_M ~ normal(0,0.5);
  beta_N ~ normal(0,0.5);
  sigma ~ exponential(1);
  mu = alpha + beta_M * M + beta_N * N;
  K ~ normal(mu, sigma);
}