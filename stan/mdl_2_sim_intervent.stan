data {
 int<lower=1> N;
 int<lower=1> K;
 vector[N] W;
 vector[N] H;
 int<lower=1, upper=K> S[N];
}
parameters {
  vector[K] alpha;
  vector<lower=0>[K] beta;
  real<lower=0> sigma;
}
model {
  W ~ normal(alpha[S] + beta[S], sigma);
  alpha ~ normal(0, .5);
  beta ~ lognormal(0, .5);
  sigma ~ exponential(1);
}
