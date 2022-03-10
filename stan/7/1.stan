// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] B;
  vector[N] M;
}

parameters {
  real alpha;
  real<lower=0> beta;
  real<lower=0> sigma;
}

model {
  B ~ normal(mu, sigma);
}

