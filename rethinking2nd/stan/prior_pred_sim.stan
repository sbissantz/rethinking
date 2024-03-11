//
// Prior predictive simulation
//
data{
    int<lower=0> N;
    real<lower=0> h[N];
}
parameters{
   real<offset=178,multiplier=20> mu;
   real<lower=0> sigma; 
}
model{
    h ~ normal(mu, sigma);
    mu ~ std_normal();
    sigma ~ exponential(1);
}
