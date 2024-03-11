data {
    int<lower=0> N;
    vector[N] A;
    vector[N] M;
    vector[N] D;
}
parameters {
    real alpha;
    real beta_A;
    real beta_M;
    real <lower=0> sigma;
}
model {
    D ~ normal(alpha + beta_A*A + beta_M*M, sigma);
    alpha ~ normal(0,0.2);
    beta_A ~ normal(0, 0.5);
    beta_M ~ normal(0, 0.5);
    sigma ~ exponential(1);
}
