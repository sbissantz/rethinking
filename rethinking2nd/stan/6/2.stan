data {
  int<lower=0> N;
  vector[N] H;
  vector[N] L;
}

parameters {
  real alpha;
  real beta_L;
  real<lower=0> sigma;
}

model {
  H ~ normal(alpha + beta_L*L, sigma);
  alpha ~ normal(0,0.2);
  beta_L ~ normal(0,0.5);
  sigma ~ exponential(1);
}
