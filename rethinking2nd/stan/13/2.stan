// Observed Variables
data{
  int<lower=1> n_tanks; 
  array[n_tanks] int N; 
  array[n_tanks] int S;
  array[n_tanks] int T;
}
// Unobserved Variables
parameters{
  vector[n_tanks] alpha; 
  real alpha_bar; 
  real<lower=0> sigma;
}
transformed parameters{
  vector[n_tanks] p_lo;
  p_lo = alpha[T];
  //for(i in 1:n_tanks) {
    //p_lo[i] = alpha[T[i]];
  //}
}
// Model Definition
model{
  sigma ~ exponential(1);
  alpha_bar ~ normal(0, 1.5);
  alpha ~ normal(alpha_bar, sigma);
  S ~ binomial_logit(N, p_lo);
}
generated quantities{
  vector[n_tanks] log_lik;
  vector[n_tanks] p;
  for (i in 1:n_tanks){
    log_lik[i] = binomial_logit_lpmf(S[i] | N[i], p_lo[i]);
  }
  p = inv_logit(p_lo);
}
