data {
 int<lower=0> N;
 int<lower=0> cno;
 array[N] int T;
 array[N] int C;
 vector[N] P;
}
parameters {
  vector<lower=0>[cno] alpha;
  vector<lower=0>[cno] beta;
  real<lower=0> gamma;
}
transformed parameters {
 vector[N] lambda;
  for (i in 1:N) {
    lambda[i] = (alpha[C[i]] * P[i]^beta[C[i]]) / gamma;
  }
}
model {
  T ~ poisson( lambda );
  alpha ~ exponential(1);
  gamma ~ exponential(1);
  beta ~ exponential(1);
}
