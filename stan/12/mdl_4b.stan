data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] zero;
  row_vector[N] one;
  array[N] int<lower=1, upper=K> R;
}

parameters{
  ordered[K-1] c;
}

model {
  R ~ ordered_logistic_glm(one, zero, c);
  c ~ normal(0, 1.5);
}
