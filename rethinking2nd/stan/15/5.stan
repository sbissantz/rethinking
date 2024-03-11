data{
    int<lower=0> N;
    int<lower=0> N_obs;
    int<lower=0> N_mis;
    array[N_obs] int<lower=1, upper=N> ii_B_obs;
    array[N_mis] int<lower=1, upper=N> ii_B_mis;
    array[N_obs] real B_obs;
    vector[N] K;
    vector[N] M;
}
parameters{
    array[N_mis] real B_mis;
     real a;
     real nu;
     real bB;
     real bM;
     real<lower=0> sigmaB;
     real<lower=0> sigma;
}
//transformed parameters {
    // Move this part to the model block, then output is suppressed
    // array[N] real B;
    // B[ii_B_obs] = B_obs;
    // B[ii_B_mis] = B_mis;
//}
model{
    //Merge missing and observed values
    array[N] real B;
    B[ii_B_obs] = B_obs;
    B[ii_B_mis] = B_mis;
    //Oldje model 
     vector[29] mu;
    sigma ~ exponential( 1 );
    sigmaB ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bB ~ normal( 0 , 0.5 );
    nu ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 0.5 );
    // Imputation model (like the measurement error model)
    B ~ normal( nu , sigmaB );
    for ( i in 1:29 ) {
        mu[i] = a + bB * B[i] + bM * M[i];
    }
    K ~ normal( mu , sigma );
}
