data {
 int<lower=1> N; //observations
 int<lower=1> J; //levels
 int<lower=1, upper=J> S[N]; //categorical predictor
 vector[N] W; //predictor
 vector[N] H;
}
parameters {
  vector[J] alpha;
  real beta_H;
  real<lower=0> sigma;
}

model {
  W ~ normal(alpha[J]+beta_H*H, sigma);
  alpha[J] ~ normal(0, 0.2);
  beta_H ~ normal(0, 0.5);
  sigma ~ exponential(1);
}
