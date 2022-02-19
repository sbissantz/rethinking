data {
  int<lower=0> N;
  vector[N] h;
  vector[N] w;
  vector[N] w_2;
}
parameters {
  real a;  
  real<lower=0> b1;
  real b2;
  real<lower=0> sigma;
}
model{
  h ~ normal(a + b1*w + b2*w_2, sigma);
      a ~ normal(178, 20);
      b1 ~ lognormal(0, 1);
      b2 ~ normal(0, 1);
    sigma ~ exponential(1);
}