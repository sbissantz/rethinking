data {
    int<lower=0> N;
    vector[N] M; 
    vector[N] A;
}
parameters {
    real alpha;
    real beta_M;
    real<lower=0> sigma;
}
model {
        A ~ normal(alpha + beta_M*M, sigma);
        alpha ~ normal(0,0.1);
        beta_M ~ normal(0, 0.5);
        sigma ~ exponential(1);
}
generated quantities {
    vector[N] mu = 
            alpha + beta_M*M;
}
