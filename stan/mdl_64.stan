data {
  int<lower=0> N;
  vector[N] K;
  vector[N] F;
}

parameters {
  real alpha;
  real beta_F;
  real<lower=0> sigma;
}

model {
  vector[N] mu =  
  alpha + beta_F*F;
  K ~ normal(mu, sigma);
  alpha ~ normal(0,0.2);
  beta_F ~ normal(0,0.5);
  sigma ~ exponential(1);
}

