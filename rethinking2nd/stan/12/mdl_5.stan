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

//generated quantities {
  //vector[N] R_hat; 
  //R_hat_A0C1I0 ~ ordered_logistic_rng(beta_A*0 + beta_C*1 + beta_I * 0 + beta_IA*(0.*0) +
  //beta_IC*(0.*1), c);
  //R_hat_A0C1I1 ~ ordered_logistic_rng(beta_A*0 + beta_C*1 + beta_I * 1 + beta_IA*(1.*0) +
  //beta_IC*(1.*1), c);
//}

