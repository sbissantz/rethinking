data {
  int<lower = 0> N;
  vector[N] S;
  vector[N] W;
}
generated quantities {
  real alpha = normal_rng(0.5, 0.25);
  real<lower=0> beta_W = normal_rng(0, 0.25);
  real<upper=0> beta_S = normal_rng(0, 0.25);
  real beta_WS = normal_rng(0, 0.25);
  real mu_sim[N] = normal_rng(alpha + beta_W*W + beta_S*S + beta_WS*(W.*S));
}
