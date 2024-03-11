data {
  int<lower=0> N;
  vector[N] W; // predictor
  vector[N] S; // ... 
  vector[N] B; // outcome 
}
parameters { 
  real alpha;
  real beta_W;
  real beta_S;
  real beta_WS;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] mu; 
  // pointwise multiplication in STAN ./
  // see: https://vasishth.github.io/bayescogsci/book/regression-models-in-stan.html
  // Note: there is also a more efficient implementation. See: url above.
  mu = alpha + beta_W*W + beta_S*S + beta_WS*(W.*S);
}
model{  
  B ~ normal(mu, sigma);
  alpha ~ normal(0.5, 0.25);
  beta_S ~ normal(0.0, 0.25);
  beta_W ~ normal(0.0, 0.25);
  beta_WS ~ normal(0.0, 0.25);
  sigma ~ exponential(1);
}
generated quantities {
  real B_tilde[N] = 
    normal_rng(mu, sigma); 
}



