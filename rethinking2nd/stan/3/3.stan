data {
    int<lower=0> N, W;
    vector[N] h, w;
    vector[W] w_seq;
}
transformed data {
    real wbar; 
    wbar = mean(w);
}
parameters {
   real alpha, beta; 
   real<lower=0> sigma; 
}
model {
   h ~ normal(alpha + beta * (w - wbar), sigma);
   alpha ~ normal(0,0.1);
   beta ~ normal(0,1);
   sigma ~ exponential(1);
}
generated quantities {
vector[N] mu 
    = alpha + beta * (w_seq-wbar);
real h_tilde[N]
    = normal_rng(alpha + beta * w, sigma);
}
