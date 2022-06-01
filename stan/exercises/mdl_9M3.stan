data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta_x; 
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0,0.1);
  beta_x ~ normal(0,1);
  sigma ~ exponential(1);
  y ~ normal(alpha + beta_x * x, sigma);
}

