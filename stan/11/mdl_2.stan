data{
  int<lower=1> N;
  int<lower=1> ano;
  int<lower=1> cno;
  int<lower=1> sno;
  array[N] int aid;
  array[N] int cid;
  array[N] int sid;
  array[N] int<lower=0,upper=1> y;
}
parameters {
  vector[ano] alpha;
  vector[cno] beta;
  vector[sno] gamma;
}
transformed parameters {
  vector[N] logit_p;
  for( i in 1:N ) {
    logit_p[i] = alpha[aid[i]] + beta[cid[i]] + gamma[sid[i]];
  }
}
model { 
  alpha ~ normal(0,1.5);
  beta ~ normal(0,0.5);
  gamma ~ normal(0,0.5);
  y ~ bernoulli_logit(logit_p);
}
generated quantities { 
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p[n]);
  }
}
