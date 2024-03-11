data {
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

parameters {
    vector[L] alpha;
    vector[L] beta_r;
    real<lower=0> sigma;
}

transformed parameters { 
    // Variables declared in the model block are always local to the model
    // block and may not be accessed in the generated quantities block
    // https://mc-stan.org/docs/2_18/reference-manual/overview-of-stans-program-blocks.html
    // to make a variable accessible in the model and generated quantities
    // block, it must be declared as a transformed parameter.
    vector[N] mu;
    for(i in 1:N) {
        mu[i] = alpha[cid[i]] + beta_r[cid[i]] * (rugged_norm[i] - rugged_bar);
    }
}

model{
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
