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
  for (i in 1:n_tanks) {
    p_lo[i] = alpha[T[i]];
  }
}
// Model Definition
model{
  alpha ~ normal(0, 1.5);
  S ~ binomial_logit(N, p_lo);
}
generated quantities{
  vector[n_tanks] p;
  vector[n_tanks] log_lik;
  for (i in 1:n_tanks){
    log_lik[i] = binomial_logit_lpmf(S[i] | N[i], p_lo[T[i]]);
  }
  p = inv_logit(p_lo);
}
