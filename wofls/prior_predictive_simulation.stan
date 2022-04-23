//
// Prior predictive simulation 
//

// Note: The snippet is c-p from:
// https://mc-stan.org/docs/2_23/stan-users-guide/prior-predictive-checks.html 
// I have nothing to add. 

// Note: I just fixed a typo in the original "loer" should be "lower"

data {
  int<lower = 0> N;
  vector[N] x;
}
generated quantities {
  real alpha = normal_rng(0, 1);
  real beta = normal_rng(0, 1);
  real y_sim[n] = poisson_log_rng(alpha + beta * x);
}
