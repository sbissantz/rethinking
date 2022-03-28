data {
  int<lower=0> N;
  vector[N] B;
  vector[N] M;
}

parameters {
  real alpha;
  real beta_M;
  real<lower=0> sigma;
}

model {
  vector[N] logprob;
  B ~ normal(alpha + beta_M * M, sigma);
  alpha ~ normal(0, 0.2);
  beta_M ~ normal(0, 10);
  sigma ~ exponential(1);
  for (i in 1:N) {
    logprob[i] = bernoulli_logit_lpmf(B | alpha + beta_M * M[i]); 
    }
}

