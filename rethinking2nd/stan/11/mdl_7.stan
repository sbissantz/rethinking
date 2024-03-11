data {
 int<lower=1> N;
 int<lower=1> cno;
 array[N] int T;
 array[N] int C;
 vector[N] P;
}
parameters{
  vector[cno] alpha;
  vector[cno] beta_P;
}
transformed parameters {
  vector[N] log_lambda;
    for (i in 1:N) {
      log_lambda[i] = alpha[C[i]] + beta_P[C[i]] * P[i];
    }
}
model{ 
  T ~ poisson_log(log_lambda);
  alpha ~  normal(3, 0.5);
  beta_P ~ normal(0, 0.2);
}
generated quantities {
vector[N] log_lik;
  for(i in 1:N) {
    log_lik[i] = poisson_log_lpmf(T[i] | log_lambda[i]);
  }
// Posterior line predictions
//vector[N] log_lambda_c1 = alpha[1] + beta_P[1] * P;
//vector[N] log_lambda_c2 = alpha[2] + beta_P[2] * P;
// Posterior predictions
//array[N] int T_tilde_c1
    //= poisson_log_rng(alpha[1] + beta_P[1] * P);
//array[N] int T_tilde_c2
    //= poisson_log_rng(alpha[2] + beta_P[2] * P);
}
