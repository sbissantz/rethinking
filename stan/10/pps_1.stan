data {
  int<lower=0> N;
  vector[N] x;
}
generated quantities {
  real alpha = normal_rng(0, 10);
  real beta = normal_rng(0, 10);
  int y_sim[N] = bernoulli_logit_rng(alpha + beta * x);
}
