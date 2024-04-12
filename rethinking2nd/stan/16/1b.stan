
data{
     int<lower=0> N;
     vector[N] w;
     vector[N] h;
}
transformed data {
     vector[N] h_cube = pow(h, 3);
}
parameters{
     real<lower=0,upper=1> p;
     real<lower=0> sigma;
     real<lower=0> k;
}
transformed parameters {
     real<lower=0,upper=1> p_square = square(p); 
     vector[N] mu_exp;
     for ( i in 1:N ) { 
          // Do not use the snippet below. "^2" and "^3" are 7x slower!
          // mu[i] = 3.141593 * k * p^2 * h[i]^3;
          mu_exp[i] = pi() * k * p_square * h_cube[i];
     }
     vector[N] mu = log(mu_exp);
}
model{
    sigma ~ exponential( 1 );
    k ~ exponential( 0.5 );
    p ~ beta( 2 , 18 );
    w ~ lognormal( mu , sigma );
}
generated quantities {
   array[N] real w_sim = lognormal_rng( mu , sigma );
}
