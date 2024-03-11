data {
  int<lower=1> N;
  int<lower=1> gno;
  int<lower=1> dno;
  int<lower=1> tno;
  array[N] int A;
  array[N] int G;
  array[N] int D;
  vector[N] T1;
  vector[N] T2;
  vector[N] T3;
}
parameters {
  matrix[gno, dno] Alpha;
  real<lower=0> beta_u;
  // Since we haven't observed u, we make it a parameter
  vector[N] u;
  vector<lower=0>[tno] tau;
}
transformed parameters {
 vector[N] p_logit;
 for(i in 1:N) {
  p_logit[i] = Alpha[G[i],D[i]] + beta_u*u[i];
 }
}
model {
  // A model
  to_vector(Alpha) ~ normal(0,1);
  beta_u ~ normal(0,1);
  A ~ bernoulli_logit(p_logit);
  // Test score model
  T3 ~ normal(u, tau[3]);
  T2 ~ normal(u, tau[2]);
  T1 ~ normal(u, tau[1]);
  tau ~ exponential(1);
  u ~ normal(0,1);
}
