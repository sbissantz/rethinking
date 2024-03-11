data {
  int<lower=0> N;
  int<lower=0> K;
  array[N] int career;
  vector[N] family_income;
}
parameters {
  vector[K-1] alpha;
  vector[K-1] beta;
}
transformed parameters {
  vector[K] p_invsoft;
  for (i in 1:N) {
    for (j in 1:(K-1)) {
      p_invsoft[j] = alpha[j] + beta[j]*family_income[i];
    }
  }
  p_invsoft[K] = 0;
}

model {
  alpha ~ normal(0, 1.5);
  beta ~ normal(0, 1);
  career ~ categorical_logit(p_invsoft);
}
generated quantities {

}
