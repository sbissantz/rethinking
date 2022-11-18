data {
  int<lower=0> N;
  int<lower=0> K;
  vector[N] zero;
  array[N] int<lower=1, upper=K> R;
}

parameters{
  vector[K-1] c;
}

transformed parameters{
  vector[N] eta = zero;
}

model {
  R ~ ordered_logistic(eta, c);
  c ~ normal(0, 1.5);
}

