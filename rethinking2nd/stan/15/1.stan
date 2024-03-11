data{
     int N;
     vector[50] D_sd;
     vector[50] D_obs;
     vector[50] M;
     vector[50] A;
}
parameters{
     vector[N] D_true;
     real a;
     real bA;
     real bM;
     real<lower=0> sigma;
}
transformed parameters {
    vector[50] mu;
    mu = a + bA * A + bM * M;
}
model{
    sigma ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bA ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 0.2 );
    D_true ~ normal( mu , sigma );
    D_obs ~ normal( D_true , D_sd );
}
