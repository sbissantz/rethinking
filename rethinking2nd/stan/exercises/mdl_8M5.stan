data {
 int<lower=0> N;
 int<lower=0> L;
 vector[N] W;
 vector[N] S;
 vector[N] B;
 int<lower=0, upper=L> BID[N];
}
parameters {
  vector[L] alpha;
  real<lower=0> beta_W;
  real<upper=0> beta_S;
  real beta_WS;
  real<lower=0> sigma;
}
transformed parameters { 
  vector[N] mu;
  for(i in 1:N) {
    mu[i] = alpha[BID[i]] + beta_W*W[i] + beta_S*S[i] + beta_WS*(W[i].*S[i]);
  }
}
model {
  B ~ normal(mu, sigma);
  alpha ~ normal(0.5, 0.25);
  // half-normal
  beta_W ~ normal(0, 0.25);
  // half-normal
  beta_S ~ normal(0, 0.25);
  beta_WS ~ normal(0, 0.25);;
  sigma ~ exponential(1);
}
