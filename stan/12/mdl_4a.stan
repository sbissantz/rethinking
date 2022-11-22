data {
  int<lower=0> N;
  int<lower=0> K;
  array[N] int<lower=1, upper=K> R;
  vector[N] zero;
}

parameters{
  ordered[K-1] c;
}

model {
  R ~ ordered_logistic(zero, c);
  c ~ normal(0, 1.5);
}

