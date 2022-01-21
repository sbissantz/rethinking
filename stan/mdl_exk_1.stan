data {
 int<lower=0> N;
 int<lower=1, upper=2> J;
 int S[N];
 vector[N] W;
}

parameters {
  vector[J] alpha;
  real<lower=0> sigma;
}

model {
  W ~ normal(alpha[J], sigma);
  alpha[J] ~ normal(0, 0.5);
  sigma ~ exponential(1);
}
