data {
  int<lower=0> N;
  vector[N] x;
}
generated quantities {
  //real alpha = normal_rng(0, 10);
  real alpha = normal_rng(0, 1.5);
  //real beta = normal_rng(0, 10);
  real beta = normal_rng(0, 0.5);
  //for (n in 1:N) {
  //y[n] ~ bernoulli(inv_logit(alpha + beta * x[n]));
  //} equivalent; but more efficiecient, is:
  array[N] int y_tilde = bernoulli_logit_rng(alpha + beta*x);
}

