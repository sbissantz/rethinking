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
     corr_matrix[2] Rho_BM;
     vector<lower=0>[2] Sigma_BM;
     real muM;
     real muB;
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
     matrix[29,2] MB;
     vector[29] mu;
    muB ~ normal( 0 , 0.5 );
    muM ~ normal( 0 , 0.5 );
    Sigma_BM ~ exponential( 1 );
    Rho_BM ~ lkj_corr( 2 );
    sigma ~ exponential( 1 );
    sigmaB ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bB ~ normal( 0 , 0.5 );
    nu ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 0.5 );
    // Important: to_vector(), since we cannot append cols to an array!
    MB = append_col(M, to_vector(B));
    {
    // Imputation model (like the measurement error model)
    vector[2] MU;
    // With this definition Mu is a row vector and then transposed to be a 
    // column vector...
    MU = [ muM , muB ]';
    // ":" here means all columns
    for ( i in 1:29 ) MB[i,:] ~ multi_normal( MU , quad_form_diag(Rho_BM , Sigma_BM) );
    }
    for ( i in 1:29 ) {
        mu[i] = a + bB * B[i] + bM * M[i];
    }
    K ~ normal( mu , sigma );
}
