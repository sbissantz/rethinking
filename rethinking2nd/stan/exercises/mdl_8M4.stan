data {
 int<lower=0> N;
 vector[N] W;
 vector[N] S;
 vector[N] B;
}
parameters {
  real alpha;
  real<lower=0> beta_W;
  real<upper=0> beta_S;
  real beta_WS;
  real<lower=0> sigma;
}
transformed parameters { 
  vector[N] mu = alpha + beta_W*W + beta_S*S + beta_WS*(W.*S);
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
