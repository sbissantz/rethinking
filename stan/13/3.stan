// Observed Variables
data{
  int<lower=1> n_ponds; 
  array[n_ponds] int N; 
  array[n_ponds] int S;
  array[n_ponds] int P;
}
// Unobserved Variables
parameters{
  vector[n_ponds] alpha; 
  real alpha_bar; 
  real<lower=0> sigma;
}
transformed parameters{
  vector[n_ponds] p_lo;
  p_lo = alpha[P];
  //for(i in 1:n_ponds) {
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
  vector[n_ponds] p;
  p = inv_logit(p_lo);
}
