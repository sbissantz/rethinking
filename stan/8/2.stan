data{
    int<lower=1> N; //data items
    int<lower=1> L; //levels
    vector[N] rugged_norm; //predictor
    int<lower=1, upper=L> cid[N]; //predictor
    vector[N] log_gdp_std; //outcome
}

transformed data {
    real rugged_bar;
    rugged_bar = mean(rugged_norm);
}

parameters{
    vector[L] alpha;
    real beta_r;
    real<lower=0> sigma;
}

model{
    vector[N] mu;
    alpha ~ normal(1, 0.1);
    beta_r ~ normal(0,0.25);
    sigma ~ exponential(2);
    for(n in 1:N) {
        mu[n] = alpha[cid[n]] + beta_r * (rugged_norm[n] - rugged_bar);
    }
    log_gdp_std ~ normal(mu, sigma);
}
