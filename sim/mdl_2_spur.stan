data {
  int<lower=0> N;  
  vector[N] x_spur;
  vector[N] y;
}
parameters{
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x_spur, sigma);
  alpha ~ normal(0,0.2);
  beta ~ normal(0,0.5);
  sigma ~ exponential(1);
}
