data{
    int<lower=0> N;
    vector[N] rugged_norm;
    vector[N] log_gdp_std;
}

transformed data {
    real rugged_bar;
    rugged_bar = mean(rugged_norm);
}

parameters{
    real alpha;
    real beta_r;
    real<lower=0> sigma;
}

model{
    vector[N] mu;
    alpha ~ normal(1, 0.1);
    beta_r ~ normal(0,0.25);
    mu = alpha + beta_r * rugged_norm - rugged_bar;
    sigma ~ exponential(2);
    log_gdp_std ~ normal(mu, sigma);
}

