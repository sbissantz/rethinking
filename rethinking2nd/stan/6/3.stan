data {
  int<lower=0> N;
  vector[N] K;
  vector[N] L;
}

parameters {
  real alpha;
  real beta_L;
  real<lower=0> sigma;
}

model {
  vector[N] mu =  
  alpha + beta_L*L;
  K ~ normal(mu, sigma);
  alpha ~ normal(0,0.2);
  beta_L ~ normal(0,0.5);
  sigma ~ exponential(1);
}

