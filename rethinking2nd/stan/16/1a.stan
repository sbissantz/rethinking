data{
     int<lower=0> N;
     vector[N] w;
     vector[N] h;
}
parameters{
     real<lower=0,upper=1> p;
     real<lower=0> k;
     real<lower=0> sigma;
}
model{
     vector[N] mu;
    sigma ~ exponential( 1 );
    k ~ exponential( 0.5 );
    p ~ beta( 2 , 18 );
    for ( i in 1:N ) {
        mu[i] = 3.141593 * k * p^2 * h[i]^3;
        mu[i] = log(mu[i]);
    }
    w ~ lognormal( mu , sigma );
}
