data {
  int<lower=1> N;
  int<lower=1> gno;
  int<lower=1> dno;
  vector[gno] beta;
  vector[gno] gamma;
  array[N] int A;
  array[N] int G;
  array[N] int D;
  array[N] int D2;
}
parameters {
  matrix[gno, dno] Alpha;
  vector[gno] delta;
  // Since we haven't observed u we make it a parameter
  vector[N] u;
}
transformed parameters {
 vector[N] p_logis;
 vector[N] q_logis;
 for(i in 1:N) {
  p_logis[i] = Alpha[G[i],D[i]] + beta[G[i]] * u[i];
  q_logis[i] = delta[G[i]] + gamma[G[i]] * u[i];
 }
}
model {
  to_vector(Alpha) ~ normal(0,1);
  delta ~ normal(0,1);
  // Because u is unobserved we can't figure out the scale
  u ~ normal(0,1);
  A ~ bernoulli_logit(p_logis);
  D2 ~ bernoulli_logit(q_logis);
}
generated quantities {

}
