data{
     int N;
     vector[N] D_sd; // luckily, these are data
     vector[N] D_obs;
     vector[N] M;
     vector[N] A;
}
parameters{
     vector[N] D_true;
     real a;
     real bA;
     real bM;
     real<lower=0> sigma;
}
model{
    vector[N] mu;
    sigma ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bA ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 0.2 );
    // Generative model
    for ( i in 1:50 ) {
        mu[i] = a + bA * A[i] + bM * M[i];
    }
    D_true ~ normal( mu , sigma );
    // Measurement model
    // D_obs = D_true + e_D
    // e_D ~ Normal(0, D_sd)
    D_obs ~ normal( D_true , D_sd );
}
