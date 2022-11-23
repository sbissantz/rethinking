data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] I;
  vector[N] A;
  vector[N] C;
  array[N] int<lower=1, upper=K> R;
}

parameters {
  ordered[K-1] c;
  real beta_A;
  real beta_C;
  real beta_I;
  real beta_IA;
  real beta_IC;
}

transformed parameters {
  vector[N] eta;
  eta = beta_A*A + beta_C*C + beta_I * I + beta_IA*(I.*A) + beta_IC*(I.*C);
}

model {
  beta_A ~ normal(0,0.5);
  beta_C ~ normal(0,0.5);
  beta_I ~ normal(0,0.5);
  beta_IA ~ normal(0,0.5);
  beta_IC ~ normal(0,0.5);
  c ~ normal(0, 1.5);
  R ~ ordered_logistic(eta, c);
}
