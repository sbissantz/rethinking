data {
    int<lower=0> N;
    vector[N] M;
    vector[N] A;
}
parameters {
    real alpha;
    real beta_A;
    real<lower=0> sigma;
}
model {
    M ~ normal( alpha+beta_A*A, sigma );
    alpha ~ normal( 0, 0.2 );
    beta_A ~ normal( 0, 0.5 );
    sigma ~ exponential( 1 );
}
