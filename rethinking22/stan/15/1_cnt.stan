data {
    int N_dyads;
    array[N_dyads] int D; // dyad ID 
    array[N_dyads] int GAB; 
    array[N_dyads] int GBA;
}

parameters { 
    real alpha; // intercept
    real sigma_T; // dyad variation
    corr_matrix[2] Rho; // Correlation matrix of dyad-specific effects
    array[N_dyads] row_vector[2] T; // Matrix of dyad-specific effects
}

model { 
    vector[N_dyads] lambdaAB; 
    vector[N_dyads] lambdaBA;
    // priors
    alpha ~ normal(0, 1); // intercept
    sigma_T ~ exponential(1); // variance
    Rho ~ lkj_corr(2); // correlation matrix
    // Multivariate prior for dyad-specific effects
    T ~ multi_normal(rep_vector(0, 2), quad_form_diag(Rho, rep_vector(sigma_T, 2))); 
    for(i in 1:N_dyads) {
        lambdaAB[i] = alpha + T[D[i],2]; 
        lambdaBA[i] = alpha + T[D[i],1];
    }
    GBA ~ poisson_log(lambdaBA); 
    GAB ~ poisson_log(lambdaAB); 
}
