// Model to estimate the effect effect of Education on Wages without control
// variables. We expect the effect beta_EW to be biased due to omitted variable

data {
    int<lower=0> N;  // number of observations
    vector[N] W;     // outcome variable 
    vector[N] E;     // predictor variable
    vector[N] Q;     // instrumental variable 
}

parameters {
    real alpha_W;      // intercept
    real beta_EW;       // slope
    real<lower=0> sigma;  // error scale
}

transformed parameters {
    vector[N] mu;  // linear predictor
    mu = alpha_W + beta_EW * E;  // linear predictor 
}

model {
    sigma ~ exponential(1);  // prior
    alpha_W ~ normal(0, 0.2);  // prior
    beta_EW ~ normal(0, 0.5);  // prior
    W ~ normal(mu, sigma);  // likelihood
}
