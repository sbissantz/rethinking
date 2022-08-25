data {
  int<lower=1> N;
  int<lower=1> gno;
  array[N] int aid;
  array[N] int gid;
  array[N] int A;
}
parameters {
  vector[gno] alpha_lo;
}
transformed parameters {
  vector[N] p_logit;
    for(i in 1:N) {
      p_logit[i] = alpha_lo[gid[i]];
    }
    // Using the binomial_logit() parametrizaition I can stick p_logis
    // directly into the likelihood function. Saves this line of code:
    // vector[N] p = inv_logit(p_logis);
    // ...and is more efficient
}
model {
  alpha_lo ~ normal(0, 1.5);
  // Using the binomial_logit() parametrizaition I can stick p_logis
  // directly into the likelihood function (i.e., not p)... more efficient!
  // A ~ binomial( aid, p );
  A ~ binomial_logit(aid, p_logis);
}
generated quantities {
  vector[gno] alpha_p = inv_logit(alpha_lo);
  vector[N] p = inv_logit(p_logit);
}

