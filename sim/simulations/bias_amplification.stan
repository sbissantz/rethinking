data {
  int<lower=0> N;
  vector[N] Y;
  vector[N] X;
  vector[N] Z;
}
parameters {
  real alpha;
  real beta_X;
  real beta_Z;
  real<lower=0> sigma;
}
model {
  Y ~ normal(alpha + beta_X*X + beta_Z*Z, sigma);
  alpha ~ normal(0,0.2);
  beta_X ~ normal(0,0.5);
  beta_Z ~ normal(0,0.5);
  sigma ~ exponential(1);
}

