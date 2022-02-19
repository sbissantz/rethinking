data {
    int<lower=0> N ;
    vector[N] h ;
    vector[N] w ;
}
parameters {
    real a ;
    real<lower=0> b ;
    real<lower=0> sigma ;
}
model {
    h ~ normal(a+b*w, sigma) ; 
    a ~ normal(178,20) ;
    b ~ lognormal(0,1) ;
    sigma ~ uniform(0,50) ; 
}
