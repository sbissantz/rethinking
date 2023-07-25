data{
  int<lower=1> N, tno, ano, bno;
  array[N] int tid, aid, bid;
  array[N] int<lower=0,upper=1> y;
}
parameters {
  vector[ano] alpha_j;
  vector[tno] beta_j;
  vector[bno] gamma_j;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_g;
  real alpha_bar;
}
transformed parameters {
  vector[N] logit_p;
  for( i in 1:N ) {
    logit_p[i] = alpha_j[aid[i]] + gamma_j[bid[i]] + beta_j[tid[i]];
  }
}
model { 
  // Hyper-priors
  sigma_a ~ exponential(1);
  sigma_b ~ exponential(1);
  sigma_g ~ exponential(1);
  alpha_bar ~ normal(0,1.5);
  // Adaptive priors
  gamma_j ~ normal(0, sigma_g); 
  beta_j ~ normal(0, sigma_b); 
  alpha_j ~ normal(alpha_bar, sigma_a); 
  // Likelihood
  y ~ bernoulli_logit(logit_p);
}
generated quantities { 
  vector[bno] gamma_p = inv_logit(gamma_j);
  vector[ano] alpha_p = inv_logit(alpha_j);
  vector[tno] beta_p = inv_logit(beta_j);
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p[n]);
  }
}
