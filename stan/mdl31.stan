//
// Model Code
//
data{
    int<lower=0> N;
    real<lower=0> h[N];
}
parameters{
   real mu; 
   real<lower=0> sigma; 
}
model{
    h ~ normal(mu, sigma);
    mu ~ normal(170, 20);
    sigma ~ exponential(1);
}
