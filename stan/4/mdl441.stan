data{
 int<lower=0> N; // n° data items
 vector[N] w_s;  // predictor vector
 vector[N] w_s2;  // predictor vector
 vector[N] h;  // outcome vector
}
parameters{
 real alpha; // intercept
 real<lower=0> beta_1; // predictor
 real beta_2; // predictor 
 real<lower=0> sigma; // error scale 
}
model{
  h ~ normal(alpha + beta_1 * w_s + beta_2 * w_s2, sigma); //likelihood
    alpha ~ normal(178,20);
    beta_1 ~ lognormal(0,1);
    beta_2 ~ normal(0,1);
    sigma ~ exponential(1);
}