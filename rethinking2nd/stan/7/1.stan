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
  B ~ normal(alpha + beta_M * M, sigma);
  alpha ~ normal(0, 0.2);
  beta_M ~ normal(0, 10);
  sigma ~ exponential(1);
}
generated quantities {
  //http://avehtari.github.io/BDA_R_demos/demos_rstan/rstan_demo.html#51_Gaussian_linear_model_with_adjustable_priors
  vector[N] logprob;
  for (i in 1:N) {
    logprob[i] = normal_lpdf(B[i] | alpha + beta_M * M[i], sigma); 
    }
}

