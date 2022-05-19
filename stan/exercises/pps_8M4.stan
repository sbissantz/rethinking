//
// Prior predictive simulation 
//

// Note: The snippet is c-p from:
// https://mc-stan.org/docs/2_23/stan-users-guide/prior-predictive-checks.html 
// I have nothing to add. 

// Note: I just fixed a typo in the original "loer" should be "lower"
//data {
 //int<lower=0> N;
 //vector[N] W;
 //vector[N] S_lz;
 //vector[N] S_ez;
 //vector[N] S_gz;
//}
generated quantities {
  real alpha = normal_rng(0.5, 0.25);
  real<lower=0> beta_W = normal_rng(0, 0.25);
  real<upper=0> beta_S = normal_rng(0, 0.25);
  real beta_WS = normal_rng(0, 0.25);
  //vector[N] mu_gz = alpha + beta_W*W + beta_S*S_gz + beta_WS*(W.*S_gz);
}
