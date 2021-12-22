data{
 int<lower=0> N; // n° data items
 vector[N] w;  // predictor vector
 vector[N] h;  // outcome vector
}
transformed data{
 vector[N] w_s =  // standardized weight
  (w - mean(w)) / sd(w);
 vector[N] w_s2 = //squared standardized weight
  square(w_s);
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
generated quantities{
  vector[N] mu 
  = alpha + beta_1 * w_s + beta_2 * w_s2;
}
