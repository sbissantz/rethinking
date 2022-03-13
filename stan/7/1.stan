data {
  int<lower=0> N;
  vector[N] B;
  vector[N] M;
}

parameters {
  real alpha;
  real beta_M;
  real<lower=0> sigma;
}

model {
  B ~ normal(alpha + beta_M * M, sigma);
  alpha ~ normal(0, 0.2);
  beta_M ~ normal(0, 10);
  sigma ~ exponential(1);
}

