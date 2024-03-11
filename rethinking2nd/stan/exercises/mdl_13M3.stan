
// Observed Variables
data{
  int<lower=1> n_tanks; 
  array[n_tanks] int N; 
  array[n_tanks] int surv;
  array[n_tanks] int tank;
}
// Unobserved Variables
parameters{
  vector[n_tanks] alpha; 
  real alpha_bar; 
  real<lower=0> sigma;
}
transformed parameters{
  vector[n_tanks] p_lo;
  p_lo = alpha[tank];
}
// Model Definition
model{
  sigma ~ exponential(1);
  alpha_bar ~ normal(0, 1.5);
  alpha ~ cauchy(alpha_bar, sigma);
  surv ~ binomial_logit(N, p_lo);
}