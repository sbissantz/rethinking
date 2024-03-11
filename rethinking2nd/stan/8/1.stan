data{
    int<lower=0> N;
    vector[N] rugged_norm;
    vector[N] log_gdp_std;
}

transformed data {
    real rugged_bar;
    rugged_bar = mean(rugged_norm);
}

parameters {
    real alpha;
    real beta_r;
    real<lower=0> sigma;
}

transformed parameters {
    vector[N] mu; 
    mu = alpha + beta_r * rugged_norm - rugged_bar;
}

model {
    alpha ~ normal(1, 0.1);
    beta_r ~ normal(0,0.25);
    sigma ~ exponential(2);
    log_gdp_std ~ normal(mu, sigma);
}

generated quantities {
    vector[N] logprob;
    for(i in 1:N) {
      logprob[i] = normal_lpdf(log_gdp_std[i] | mu, sigma);
    }
}

