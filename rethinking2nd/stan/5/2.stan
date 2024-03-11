data {
  int<lower=0> N;
  vector[N] M;
  vector[N] D;
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
 vector[N] log_lik;
 for(i in 1:N) {
 log_lik[i] = normal_lpdf(D[i] | a + b_M*M[i], sigma);
 }
}
