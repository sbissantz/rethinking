data{
  int<lower=1> N, tno, ano;
  array[N] int tid, aid;
  array[N] int<lower=0,upper=1> y;
}
parameters {
  vector[ano] alpha;
  vector[tno] beta;
}
transformed parameters {
  vector[N] p;
  for( i in 1:N ) {
    p[i] = alpha[aid[i]] + beta[tid[i]];
  }
}
model { 
  alpha ~ normal(0,1.5);
  beta ~ normal(0,1.5);
  y ~ bernoulli_logit(p);
}
generated quantities { 
  array[N] int y_tilde = bernoulli_logit_rng(p);
  vector[ano] alpha_p = inv_logit(alpha);
  vector[tno] beta_p = inv_logit(beta);
}
