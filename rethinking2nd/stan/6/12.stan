data {
  int<lower=0> n;
  int<lower=0> k;
  vector[n] C;
  vector[n] P;
  vector[n] G;
  int U[n];
}

parameters {
  vector[k] alpha;
  real beta_CG;
  real beta_CP;
  real<lower=0> sigma;
}

model {
  alpha[k] ~ normal(0, 0.2);
  beta_CG ~ normal(0, 0.5);
  beta_CP ~ normal(0, 0.5);
  sigma ~ exponential(1);
  
  vector[n] mu;
  for(i in 1:n) { 
    mu[i] = alpha[U[i]] + beta_CG*G[i] + beta_CP*P[i];
  }

  C ~ normal(mu, sigma);
}


