data {
  int<lower=0> N;
  vector[N] x;
}
generated quantities {
  real alpha = normal_rng(0, 1.5);
  real beta = normal_rng(0, 0.5);
}

