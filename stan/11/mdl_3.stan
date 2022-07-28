data{
  int<lower=1> N;
  int<lower=1> ano;
  int<lower=1> cno;
  int<lower=1> sno;
  array[N] int aid;
  array[N] int cid;
  array[N] int sid;
  array[N] int<lower=2,upper=18> y;
}
parameters {
  vector[ano] alpha;
  vector[cno] beta;
  vector[sno] gamma;
}
transformed parameters {
  vector[N] logis_p;
  vector[N] p;
  // Calculate the probability
  for( i in 1:N ) {
    logis_p[i] = alpha[aid[i]] + beta[cid[i]] + gamma[sid[i]];
  }
  // Trick for binomial_lpmf
  p = inv_logit(logis_p);
}
model { 
  alpha ~ normal(0,1.5);
  beta ~ normal(0,0.5);
  gamma ~ normal(0,0.5);
  y ~ binomial(N, p);
}
generated quantities { 
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = binomial_lpmf(y[i] | N, p[i]);
  }
}

