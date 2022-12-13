data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> L;
  int<lower=0> M;
  vector[N] I;
  vector[N] A;
  vector[N] C;
  array[N] int<lower=1, upper=M> E;
  array[N] int<lower=1, upper=L> G;
  array[N] int<lower=1, upper=K> R;
  vector[K] alpha;
}
parameters {
  ordered[K-1] c;
  vector[L] beta_A;
  vector[L] beta_C;
  vector[L] beta_I;
  vector[L] beta_E;
  simplex[K] delta;
}
transformed parameters{
    vector[N] eta;
    vector[K+1] delta_j;
    delta_j = append_row(0, delta);
    for ( i in 1:N ) {
        eta[i] = beta_E[G[i]] * sum(delta_j[1:E[i]]) + beta_A[G[i]]*A[i] + beta_C[G[i]]*C[i] + beta_I[G[i]] * I[i];
    }
}

model{
    delta ~ dirichlet(alpha);
    beta_A ~ normal(0,0.5);
    beta_I ~ normal(0,0.5);
    beta_C ~ normal(0,0.5);
    beta_E ~ normal(0,0.5);
    c ~ normal(0,1);
    R ~ ordered_logistic(eta,c);
}

