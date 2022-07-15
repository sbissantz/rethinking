data {
  int<lower=0> N;
  array[N] int tid;
}
generated quantities {
  //real alpha = normal_rng(0, 10);
  real alpha = normal_rng(0, 1.5);
  // Make it more efficient (someday)
  vector[4] beta;
    for (i in 1:4) {
      //normal_rng(0, 10);
      beta[i] = normal_rng(0, 0.5);
      } 
  array[N] int y_tilde;
    for(i in 1:N) {
      y_tilde[i] = bernoulli_logit_rng(alpha + beta[tid[i]]);
  }
}
