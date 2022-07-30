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
  vector[N] p_logis;
    for(i in 1:N) {
      p_logis[i] = Alpha_lo[did[i],gid[i]];
    }
    // Using the binomial_logit() parametrizaition I can stick p_logis
    // directly into the likelihood function. Saves this line of code:
     vector[N] p = inv_logit(p_logis);
     //...and is more efficient
}
model {
  //Convert the matrix m to a column vector in column-major order.
  to_vector(Alpha_lo) ~ normal(0,1);
  A ~ binomial(aid, p);
}
generated quantities {
  matrix[dno, gno] Alpha_p = inv_logit(Alpha_lo);
  // binomial_logit_rng() is not implemented yet!
  // Switch to binomial()
  array[N] int y_tilde = binomial_rng(aid, p);
}


