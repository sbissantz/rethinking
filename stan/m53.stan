data {
  int<lower=0> N;
  int<lower=0> K;
  // int<lower=0> N_seq;
  vector[N] D; 
  matrix[N,K] X;
  // matrix[N_seq,K] X_seq;
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
// generated quantities {
//   vector[N_seq] mu =
//   X_seq*beta + alpha;
// }
