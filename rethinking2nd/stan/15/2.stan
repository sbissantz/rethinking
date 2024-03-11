data{
     int N;
     vector[N] D_sd;
     vector[N] D_obs;
     vector[N] M_sd;
     vector[N] M_obs;
     vector[N] A;
}
parameters{
     vector[N] D_true;
     vector[N] M_true;
     real a;
     real bA;
     real bM;
     real<lower=0> sigma;
}
transformed parameters {
     vector[N] mu;
     mu = a + bA * A + bM * M_obs;
}
model{
    sigma ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bA ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 0.2 );
    M_true ~ normal( 0 , 1 );
    M_obs ~ normal( M_true , M_sd );
    D_true ~ normal( mu , sigma );
    D_obs ~ normal( D_true , D_sd );
}
