data {
  int<lower=0> n;
  int<lower=0> l;
  vector[n] H;
  vector[n] A;
  int MID[n];
}

parameters {
  vector[l] alpha;
  real beta_A;
  real<lower=0> sigma;
}

model {
  vector[n] mu;
  alpha ~ normal(0,1);
  beta_A ~ normal(0,2);
  sigma ~ exponential(1);
  for (i in 1:n) { 
    mu[i] = alpha[MID[i]] + beta_A * A[i];
  }
  H ~ normal(mu, sigma);
}

