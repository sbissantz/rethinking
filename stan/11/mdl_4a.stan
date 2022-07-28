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
  vector[N] p_logis;
    for(i in 1:N) {
      p_logis[i] = alpha_lo[gid[i]];
    }
    vector[N] p = inv_logit(p_logis);
}
model {
  alpha_lo ~ normal(0, 1.5);
  A ~ binomial( aid, p_logis );
}
generated quantities {
  vector[gno] alpha_p = inv_logit(alpha_lo);
}

