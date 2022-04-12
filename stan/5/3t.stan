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
  D ~ student_t(2, X*beta + alpha, sigma);
  alpha ~ normal(0, 0.2);
  beta ~ normal(0, 0.5);
  sigma ~ exponential(1);
}
generated quantities {
  vector[N] log_lik;
  for( i in 1:N ) {
  log_lik[i] = student_t_lpdf(D[i] | 2, X[i] * beta, sigma);
  }
}
