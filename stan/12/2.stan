data {
 int<lower=0> N;
 int<lower=0> cno;
 array[N] int T;
 array[N] int C;
 vector[N] P;
}
parameters {
  vector<lower=0>[cno] alpha;
  vector<lower=0>[cno] beta;
  real<lower=0> gamma;
  real<lower=0> phi;
}
transformed parameters {
 vector[N] mu;
  for (i in 1:N) {
    mu[i] = alpha[C[i]] * P[i]^beta[C[i]] / gamma;
  }
}
model {
  T ~ neg_binomial_2(mu, phi);
  alpha[C] ~ normal(1,1);
  beta[C] ~ exponential(1);
  gamma ~ exponential(1);
  phi ~ exponential(1);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = neg_binomial_2_lpmf(T[n] | mu[n], phi);
  }
}
