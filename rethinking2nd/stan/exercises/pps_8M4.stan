generated quantities {
  real alpha = normal_rng(0.5, 0.25);
  // beta_W ~ halfnormal
  real<lower=0> beta_W = normal_rng(0, 0.25);
  // beta_S ~ halfnormal
  real<upper=0> beta_S = normal_rng(0, 0.25);
  real beta_WS = normal_rng(0, 0.25);
  //vector[N] mu_gz = alpha + beta_W*W + beta_S*S_gz + beta_WS*(W.*S_gz);
}













