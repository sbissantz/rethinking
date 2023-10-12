// Model to estimate the effect effect of Education on Wages using the
// instrument Q as an ordinary predictor. We expect the effect beta_EW to be
// biased – more biased than befor – due to the bias amplifier Q
data {
    int<lower=0> N;  // number of observations
    vector[N] W;     // outcome variable 
    vector[N] E;     // predictor variable
    vector[N] Q;     // instrumental variable 
}

parameters {
    real alpha_W;      // intercept
    real beta_EW;       // Treatment effect 
    real beta_QW;       // Total effect: instrument on outcome 
    real<lower=0> sigma;  // error scale
}

transformed parameters {
    vector[N] mu;  // linear predictor
    mu = alpha_W + beta_EW * E + beta_QW * Q;  // linear predictor 
}

model {
    sigma ~ exponential(1);  // prior)             
    alpha_W ~ normal(0, 0.2);  // prior
    beta_EW ~ normal(0, 0.5);  // prior
    beta_QW ~ normal(0, 0.5);  // prior
    W ~ normal(mu, sigma);  // likelihood
}
