data{
     int N;
     vector[N] M_sd; // luckily, these are data
     vector[N] D_sd; // luckily, these are data
     vector[N] M_obs;
     vector[N] D_obs;
     vector[N] M;
     vector[N] A;
}
parameters{
     vector[N] M_true;
     vector[N] D_true;
     real<lower=0> sigma;
     real<lower=0> tau;
     real bM;
     real bAM;
     real bA;
     real aM;
     real a;
}
model{
    vector[N] nu;
    vector[N] mu;
    sigma ~ exponential( 1 );
    tau ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bAM ~ normal( 0 , 0.5 );
    bA ~ normal( 0 , 0.5 );
    aM ~ normal( 0 , 0.2 );
    a ~ normal( 0 , 0.2 );
    // Generative model
    for ( i in 1:50 ) {
        mu[i] = a + bA * A[i] + bM * M_true[i];
    }
    D_true ~ normal( mu , sigma );
    // Measurement model
    // D_obs = D_true + e_D
    // e_D ~ Normal(0, D_sd)
    D_obs ~ normal( D_true , D_sd );
    // Generative model
    for ( i in 1:50 ) {
        nu[i] = aM + bA * A[i];
    }
    M_true ~ normal( nu , tau );
    // Measurement model
    // M_obs = A_true + e_D
    // e_M ~ Normal(0, M_sd)
    M_obs ~ normal( M_true , M_sd );
}
