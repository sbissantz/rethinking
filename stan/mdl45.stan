data {
  int<lower=0> N;
  vector[N] w_s;
  vector[N] w_s2;
  vector[N] w_s3;
  real h[N];
}
parameters {
  real a;
  real<lower=0> b1;
  real b2;
  real b3;
  real<lower=0> sigma;
}
model {
 h ~ normal(a + b1*w_s + b2*w_s2 + b3*w_s3, sigma); 
    a ~ normal(178,20);
    b1 ~ lognormal(0,1);
    b2 ~ normal(0,1);
    b3 ~ normal(0,1);
  sigma ~ exponential(1);
}