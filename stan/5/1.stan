data {
 int<lower=0> N;
 vector[N] A;
 vector[N] D;
}
parameters {
  real<lower=0> a;
  real b_A;
  real<lower=0> sigma;
}
model {
  D ~ normal(a + b_A*A, sigma);
  a ~ normal(0,0.1);
  b_A ~ normal(0,0.5);
  sigma ~ exponential(1);
}
generated quantities {
  vector[N] mu = a + b_A*A;
}
