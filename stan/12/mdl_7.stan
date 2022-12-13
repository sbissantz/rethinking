data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> L;
  vector[N] I;
  vector[N] A;
  vector[N] C;
  array[N] int<lower=1, upper=L> G;
  array[N] int<lower=1, upper=K> R;
}

parameters {
  ordered[K-1] c;
  vector[L] beta_A;
  vector[L] beta_C;
  vector[L] beta_I;
}

transformed parameters {
  vector[N] eta;
  for(i in 1:N) {
    eta[i] = beta_A[G[i]]*A[i] + beta_C[G[i]]*C[i] + beta_I[G[i]] * I[i];
  }
}

model {
  beta_A ~ normal(0,0.5);
  beta_C ~ normal(0,0.5);
  beta_I ~ normal(0,0.5);
  c ~ normal(0, 1.5);
  R ~ ordered_logistic(eta, c);
}

