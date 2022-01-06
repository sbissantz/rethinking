data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] M;
  vector[N] D;
  vector[K] M_seq;
}
parameters {
  real a;
  real b_M;
  real<lower=0> sigma;
}
model {
  D ~ normal(a + b_M*M, sigma);
  a ~ normal(0,0.1);
  b_M ~ normal(0,0.5);
  sigma ~ exponential(1);
}
generated quantities {
  vector[K] mu = a + b_M*M_seq;  
}
