data {
  int<lower=0> N;
  row_vector[N] h0;
  vector[N] h1;
  vector[N] F;
  vector[N] T;
}

parameters {
  real<lower=0> alpha;
  real beta_F;
  real beta_T;
  real<lower=0> sigma;
}

model {
  vector[N] mu; 
  vector[N] p; 
  alpha ~ lognormal(0, 0.25);
  beta_F ~ normal(0, 0.5);
  beta_T ~ normal(0, 0.5);
  sigma ~ exponential(1);
  p = alpha + beta_T*T + beta_F*F;
  for(i in 1:N) { 
    mu[i] = h0[i]*p[i];
  }
  h1 ~ normal(mu, sigma);
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(h1[i] | h0[i] * (alpha + beta_T*T[i] +
    beta_F*F[i]), sigma); 
    }
}
