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
model {
  vector[N] mu;
  alpha ~ normal(0.5, 0.25);
  // half-normal
  beta_W ~ normal(0, 0.25);
  // half-normal
  beta_S ~ normal(0, 0.25);
  beta_WS ~ normal(0, 0.25);
  sigma ~ exponential(1);
    for(i in 1:N) {
      mu[i] = alpha[BID[i]];
      //mu[i] = alpha[BID[i]] + beta_W*W + beta_S*S + beta_WS*(W.*S);
    }
  B ~ normal(mu, sigma);
}
