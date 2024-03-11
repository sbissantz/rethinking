data{
    int N_households;
    int N;
    array[300] int giftsAB;
    array[300] int giftsBA;
    array[300] int did;
    array[300] int hidA;
    array[300] int hidB;
}
parameters{
    real a;
    // List of vectors for the multivariate normal
    array[N_households] vector[2] gr;
    corr_matrix[2] Rho_gr;
    vector<lower=0>[2] sigma_gr;
    matrix[2,N] z;
    cholesky_factor_corr[2] L_Rho_d;
    real<lower=0> sigma_d;
}
transformed parameters{
    vector[300] lambdaBA;
    vector[300] lambdaAB;
    matrix[N,2] d;
//////////////////////////////
    d = (diag_pre_multiply(rep_vector(sigma_d, 2), L_Rho_d) * z)';
    // Linear models
    for ( i in 1:300 ) { 
        lambdaBA[i] = a + gr[hidB[i], 1] + gr[hidA[i], 2] + d[did[i], 2];
        lambdaAB[i] = a + gr[hidA[i], 1] + gr[hidB[i], 2] + d[did[i], 1];
    }
}
model{
    sigma_d ~ exponential( 1 );
    L_Rho_d ~ lkj_corr_cholesky( 8 );
    to_vector( z ) ~ normal( 0 , 1 );
    sigma_gr ~ exponential( 1 );
    Rho_gr ~ lkj_corr( 4 );
    gr ~ multi_normal( rep_vector(0,2) , quad_form_diag(Rho_gr , sigma_gr) );
    a ~ normal( 0 , 1 );
    // Outcome variables and likelihoods
    giftsBA ~ poisson_log( lambdaBA );
    giftsAB ~ poisson_log( lambdaAB );
}
generated quantities{
    matrix[2,2] Rho_d;
    // Rho = LL^T
    Rho_d = multiply_lower_tri_self_transpose(L_Rho_d);
}
