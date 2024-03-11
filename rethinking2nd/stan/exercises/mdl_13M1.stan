
// Observed Variables
data{
  int<lower=1> n_tanks; 
  int<lower=1> n_siz; 
  int<lower=1> n_pred; 
  array[n_tanks] int N; 
  array[n_tanks] int surv;
  array[n_tanks] int tank;
  array[n_tanks] int pred;
  array[n_tanks] int siz;
}
// Unobserved Variables
parameters{
  vector[n_tanks] alpha; 
  vector[n_pred] gamma; 
  vector[n_siz] delta; 
  real alpha_bar; 
  real<lower=0> sigma;
}
transformed parameters{
  vector[n_tanks] p_lo;
  p_lo = alpha[tank] + gamma[pred] + delta[siz];  
}
// Model Definition
model{
  sigma ~ exponential(1);
  alpha_bar ~ normal(0, 1.5);
  alpha ~ normal(alpha_bar, sigma);
  gamma ~ normal(0, 1.5);
  delta ~ normal(0, 1.5);
  surv ~ binomial_logit(N, p_lo);
}
generated quantities{
  vector[n_tanks] log_lik;
  vector[n_tanks] p;
  for (i in 1:n_tanks){
    log_lik[i] = binomial_logit_lpmf(surv[i] | N[i], p_lo[i]);
  }
  p = inv_logit(p_lo);
}
