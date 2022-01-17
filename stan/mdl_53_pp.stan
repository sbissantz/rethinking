data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] D; 
  matrix[N,K] X;
}
parameters {
  real alpha; 
  vector[K] beta;
  real<lower=0> sigma; 
}
model {
  D ~ normal(X*beta + alpha, sigma);
  alpha ~ normal(0, 0.2);
  beta ~ normal(0, 0.5);
  sigma ~ exponential(1);
}
generated quantities {
  vector[N] mu =
    X*beta + alpha;
  real D_tilde[N] =
  normal_rng(mu, sigma);
  }
