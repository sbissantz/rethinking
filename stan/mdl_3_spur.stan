data {
  int<lower=0> N;  
  vector[N] x_real;
  vector[N] x_spur;
  vector[N] y;
}
parameters{
  real alpha;
  real beta_real;
  real beta_spur;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta_real * x_real + beta_spur * x_spur, sigma);
  alpha ~ normal(0,0.2);
  beta_real ~ normal(0,0.5);
  beta_spur ~ normal(0,0.5);
  sigma ~ exponential(1);
}
