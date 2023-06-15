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
}
// Model Definition
model{
  vector[n_tanks] p;
  alpha ~ normal(0, 1.5);
  p = alpha[T];
  S ~ binomial_logit(N, p);
}
generated quantities{
  vector[n_tanks] log_lik;
  for (i in 1:n_tanks){
    log_lik[i] = binomial_logit_lpmf(S[i] | N[i], alpha[T[i]]);
  }
}
