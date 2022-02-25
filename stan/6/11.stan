data {
  int<lower=0> n;
  vector[n] C;
  vector[n] P;
  vector[n] G;
}

parameters {
  real alpha;
  real beta_G;
  real beta_P;
  real<lower=0> sigma;
}

model {
  C ~ normal(alpha + beta_G*G + beta_P*P, sigma);
  alpha ~ normal(0, 0.2);
  beta_G ~ normal(0, 0.5);
  beta_P ~ normal(0, 0.5);
  sigma ~ exponential(1);
}

