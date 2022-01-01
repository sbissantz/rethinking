data{
    int<lower=0> N ;
    vector[N] t ;
}
parameters{
//
}
model{
//
}
generated quantities{
    h ~ normal(mu, sigma) ;
    vector[N] mu = alpha + beta*t ;
        alpha ~ normal(0, 0.2) ;
        beta ~ normal(0,1) ;
    sigma ~ exponential(1) ;
}
