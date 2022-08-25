data {
  int<lower=1> N;
  int<lower=1> gno;
  int<lower=1> dno;
  array[N] int gid;
  array[N] int did;
  array[N] int A;
  vector[N] u;
}
parameters {
  matrix[dno, gno] Alpha_lo;
  real<lower=0> beta_uA;
}
transformed parameters {
  vector[N] p_logit;
    for(i in 1:N) {
      p_logit[i] = Alpha_lo[did[i],gid[i]] + beta_uA * u[i];
    }
}
model {
  //Convert the matrix m to a column vector in column-major order.
  to_vector(Alpha_lo) ~ normal(0,1);
  beta_uA ~ normal(0,1);
  A ~ bernoulli_logit(p_logit);
}
generated quantities {
  matrix[dno, gno] Alpha_p = inv_logit(Alpha_lo);
}
