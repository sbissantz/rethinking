data {
  int<lower=0> N;
  vector[N] x;
}
generated quantities {
  real alpha_r = normal_rng(0, 1);
  real beta_r = normal_rng(0, 1);
  array[N] real y_sim = poisson_log_rng(alpha_r + beta_r * x);
}
