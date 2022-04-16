data { 
    int<lower=0> N;
    vector[N] x;
    vector[N] y;
}

parameters {
    real alpha;
    real beta;
    real<lower=0> sigma;
}

model {
    y ~ normal( alpha + beta*x, sigma );
    alpha ~ normal(0, 0.2);
    beta ~ normal(0, 0.5);
    sigma ~ exponential(1);
}

generated quantities {
    vector[N] log_lik;
    for(i in 1:N) {
        log_lik[i] = normal_lpdf( y[i] | alpha + beta*x[i], sigma );
    }
}

