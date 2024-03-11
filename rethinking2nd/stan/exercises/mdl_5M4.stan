data {
  int<lower=0> n;
  vector[n] D;
  vector[n] L;
  vector[n] A;
  vector[n] M;
}
parameters {
  real alpha;
  real beta_L;
  real beta_A;
  real beta_M;
  real<lower=0> sigma;
}
model {
  D ~ normal(alpha + beta_L*L + beta_A*A + beta_M*M, sigma);
  alpha ~ normal(0, 0.2);
  beta_L ~ normal(0, 0.5);
  beta_A ~ normal(0, 0.5);
  beta_M ~ normal(0, 0.5);
  sigma ~ exponential(1);
}
