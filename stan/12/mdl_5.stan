data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] I;
  vector[N] A;
  vector[N] C;
  array[N] int<lower=1, upper=K> R;
}

parameters{
  ordered[K-1] c;
  real beta_A;
  real beta_C;
  real beta_I;
  real beta_IA;
  real beta_IC;
}

transformed parameters {
  real eta;
  vector[N] Beta_I;
  Beta_I = beta_I*I + beta_IA*A + beta_IC*C;
  eta = beta_A*A + beta_C*C + Beta_I*I;
}

model {
  R ~ ordered_logistic(eta, c);
  c ~ normal(0, 1.5);
}

