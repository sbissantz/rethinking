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
  vector[N] logit_p;
  for( i in 1:N ) {
    logit_p[i] = alpha[aid[i]] + beta[tid[i]];
  }
}
model { 
  // alpha ~ normal(0,1.5);
  alpha ~ normal(0,10);
  beta ~ normal(0,0.5);
  y ~ bernoulli_logit(logit_p);
}
