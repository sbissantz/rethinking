data {
    int<lower=0> N ;
//    int<lower=0> W ;
    vector[N] h, w;
//    vector[W] w_seq;
}
transformed data {
    real wbar; 
    wbar = mean(w);
}
parameters {
   real alpha; 
   real<lower=0> beta; 
   real<lower=0> sigma; 
}
model {
   h ~ normal(alpha + beta * (w - wbar), sigma);
        alpha ~ normal(178, 20);
        beta ~ lognormal(0, 1);
   sigma ~ exponential(1);
}
generated quantities {
// vector[W] mu 
//    = alpha + beta * (w_seq-wbar);
// real h_tilde[W]
//    = normal_rng(alpha + beta * w_seq, sigma);
}
