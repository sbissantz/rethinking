data{
  int<lower=1> N, tno, ano, bno;
  array[N] int tid, aid, bid;
  array[N] int<lower=0,upper=1> y;
}
parameters {
  vector[ano] a_j;
  vector[bno] g_j;
  vector[tno] b_j;
  real alpha_bar;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_g;
}
transformed parameters {
  vector[N] logit_p;
  for( i in 1:N ) {
    logit_p[i] = alpha_bar + a_j[aid[i]] * sigma_a + g_j[bid[i]] * sigma_g +
    b_j[tid[i]] * sigma_b;
  }
}
model { 
  // Hyper-priors
  sigma_a ~ exponential(1);
  sigma_b ~ exponential(1);
  sigma_g ~ exponential(1);
  alpha_bar ~ normal(0,1.5);
  // Adaptive priors
  g_j ~ std_normal();
  b_j ~ std_normal();
  a_j ~ std_normal(); 
  // Likelihood
  y ~ bernoulli_logit(logit_p);
}
generated quantities { 
  vector[ano] alpha_j = alpha_bar + a_j * sigma_a; 
  vector[ano] alpha_p = inv_logit(alpha_j);

  vector[bno] gamma_j = g_j * sigma_g;  
  vector[bno] gamma_p = inv_logit(gamma_j);

  vector[tno] beta_j = b_j * sigma_b;  
  vector[tno] beta_p = inv_logit(beta_j);

  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p[n]);
  }
}
