data {
  int<lower=1> N;
  int<lower=1> gno;
  int<lower=1> dno;
  array[N] int gid;
  array[N] int did;
  array[N] int A;
}
parameters {
  matrix[dno, gno] Alpha_lo;
}
transformed parameters {
  vector[N] p_logis;
    for(i in 1:N) {
      p_logis[i] = Alpha_lo[did[i],gid[i]];
    }
}
model {
  //Convert the matrix m to a column vector in column-major order.
  to_vector(Alpha_lo) ~ normal(0,1);
  A ~ bernoulli_logit(p_logis);
}
generated quantities {
  matrix[dno, gno] Alpha_p = inv_logit(Alpha_lo);
  array[N] int y_tilde = bernoulli_logit_rng(p_logis);
}


