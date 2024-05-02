
data{
     int<lower=0> N;
     vector[N] w;
     vector[N] h;
}
transformed data {
     vector[N] h_cube = pow(h, 3);
}
parameters{
     real<lower=0> sigma;
}
transformed parameters {
     vector[N] mu_exp;
     for ( i in 1:N ) { 
          mu_exp[i] = h_cube[i];
     }
     vector[N] mu = log(mu_exp);
}
model{
    sigma ~ exponential( 1 );
    w ~ lognormal( mu , sigma );
}
generated quantities {
   array[N] real w_sim = lognormal_rng( mu , sigma );
}
