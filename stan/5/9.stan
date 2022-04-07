data {
  int<lower=1> n;
  int<lower=1> k;
  int<lower=1, upper=k> C[n];
  vector[n] K;
}
parameters {
  vector[k] alpha;
  real<lower=0> sigma;
}
model {
  vector[n] mu;
  for(i in 1:n) {
    mu[i] = alpha[C[i]];
  }
  alpha ~ normal(0,0.5);
  sigma ~ exponential(1);
  K ~ normal(mu, sigma);
}
