data {
  int<lower=1> N;
  int<lower=1> gno;
  int<lower=1> dno;
  array[N] int gid;
  array[N] int did;
  array[N] int aid;
  array[N] int A;
}
parameters {
  matrix[dno, gno] Alpha_lo;
}
transformed parameters {
  vector[N] p_logit;
  vector[N] p_logit_m;
  vector[N] p_logit_f;
    for(i in 1:N) {
      p_logit[i] = Alpha_lo[did[i],gid[i]];
      p_logit_f[i] = Alpha_lo[did[i], 1];
      p_logit_m[i] = Alpha_lo[did[i], 2];
    }
    // Can't use 'binomial_logit()' because 'binomial_logit_rng() is not
    // implemented'
     vector[N] p = inv_logit(p_logit);
}
model {
  //Convert the matrix m to a column vector in column-major order.
  to_vector(Alpha_lo) ~ normal(0,1);
  A ~ binomial(aid, p);
}
generated quantities {
  matrix[dno, gno] Alpha_p = inv_logit(Alpha_lo);
  vector[N] p_diff = inv_logit(p_logit_f) - inv_logit(p_logit_m);
  // binomial_logit_rng() is not implemented yet!
  // Switch to binomial()
  array[N] int y_tilde = binomial_rng(aid, p);
}


