data {
     int N_dyads;
    array[300] int GAB;
    array[300] int GBA;
    array[300] int D;

}

parameters { 
    real alpha; // intercept
    real<lower=1> sigma_T; // dyad variation
    matrix[2,N_dyads] Z; 
    cholesky_factor_corr[2] L_Rho_T; // Cholesky factor Cormat
}

transformed parameters {
  matrix[N_dyads,2] T; // Matrix of dyad-specific effects
  T = (diag_pre_multiply(rep_vector(sigma_T,2), L_Rho_T) * Z)';
}

model { 
    vector[N_dyads] lambdaAB; 
    vector[N_dyads] lambdaBA;
    // priors
    to_vector(Z) ~ normal(0,1);
    alpha ~ normal(0, 1); // intercept
    sigma_T ~ exponential(1); // variance
    L_Rho_T ~ lkj_corr_cholesky(2); // Cholesky factor Cormat 
    // Likelihood and linear models
    for(i in 1:N_dyads) {
        lambdaAB[i] = alpha + T[D[i],2]; 
        lambdaBA[i] = alpha + T[D[i],1];
    }
    GBA ~ poisson_log(lambdaBA); 
    GAB ~ poisson_log(lambdaAB); 
}

generated quantities { 
    matrix[2,2] Rho_T; 
    Rho_T = multiply_lower_tri_self_transpose(L_Rho_T); 
}