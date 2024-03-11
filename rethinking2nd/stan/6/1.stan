data {
  int<lower=0> N;
  vector[N] H;
  vector[N] L;
  vector[N] R;
}

parameters {
  real alpha;
  real beta_L;
  real beta_R;
  real<lower=0> sigma;
}

model {
  H ~ normal(alpha + beta_L*L + beta_R*R, sigma);
  alpha ~ normal(0,0.2);
  beta_L ~ normal(0,0.5);
  beta_R ~ normal(0,0.5);
  sigma ~ exponential(1);
}
