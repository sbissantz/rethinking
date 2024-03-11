data { 
    int<lower=0> N;
}
parameters {
  real v;
  real x;
}
model {
  v ~ normal(0, 3); 
  x ~ normal(0, exp(v));
}
