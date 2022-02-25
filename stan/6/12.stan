data {
  int<lower=0> n;
  int<lower=0> l;
  vector[n] C;
  vector[n] P;
  vector[n] G;
  int U[l];
}

parameters {
  vector[l] alpha;
  real beta_G;
  real beta_P;
  real<lower=0> sigma;
}

model {
  vector[n] mu;
  alpha[l] ~ normal(0, 0.2);
  beta_G ~ normal(0, 0.5);
  beta_P ~ normal(0, 0.5);
  sigma ~ exponential(1);
  for(i in 1:n) { 
    mu[i] = alpha[U[i]] + beta_G*G[i] + beta_P*P[i];
  }
  C ~ normal(mu, sigma);
}


