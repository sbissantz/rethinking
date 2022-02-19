data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N,K] B;
  vector[N] D;
}
parameters {
  vector[K] w;
  real alpha;
  real<lower=0> sigma;
}
model {
  D ~ normal(B*w + alpha, sigma);
  w ~ normal(100,10);
  alpha ~ normal(0,10);
  sigma ~ exponential(1);
}
generated quantities {
  vector[N] mu = B*w + alpha;
}