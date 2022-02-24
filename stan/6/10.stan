data {
  int<lower=0> n;
  vector[n] H;
  vector[n] A;
}

parameters {
  real alpha;
  real beta_A;
  real<lower=0> sigma;
}

model {
  H ~ normal(alpha + beta_A * A, sigma);
  alpha ~ normal(0,1);
  beta_A ~ normal(0,2);
  sigma ~ exponential(1);
}

