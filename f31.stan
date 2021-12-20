//
// Model Code
//
data{
    int<lower=0> N;
    real<lower=0> h[N];
}
parameters{
   real mu; 
   real sigma; 
}
model{
    h ~ normal(mu, sigma);
    mu ~ normal(170, 20);
    sigma ~ uniform(0, 50);
}
