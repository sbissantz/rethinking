data{
     int N_households;
     int N_dyads;
    array[300] int GAB;
    array[300] int GBA;
     vector[300] A;
    array[300] int D;
    array[300] int HB;
     vector[25] W;
    array[300] int HA;
}
parameters{
     real a;
     vector[2] bW;
     real bA;
     matrix[2,N_dyads] Z;
     cholesky_factor_corr[2] L_Rho_T;
     real<lower=0> sigma_T;
     matrix[2,N_households] Zgr;
     cholesky_factor_corr[2] L_Rho_gr;
     vector<lower=0>[2] sigma_gr;
}
transformed parameters{
     matrix[N_dyads,2] T;
     matrix[N_households,2] gr;
    gr = (diag_pre_multiply(sigma_gr, L_Rho_gr) * Zgr)';
    T = (diag_pre_multiply(rep_vector(sigma_T, 2), L_Rho_T) * Z)';
}
model{
     vector[300] lambdaAB;
     vector[300] TAB;
     vector[300] GA;
     vector[300] RB;
     vector[300] lambdaBA;
     vector[300] TBA;
     vector[300] GB;
     vector[300] RA;
    sigma_gr ~ exponential( 1 );
    L_Rho_gr ~ lkj_corr_cholesky( 2 );
    to_vector( Zgr ) ~ normal( 0 , 1 );
    sigma_T ~ exponential( 1 );
    L_Rho_T ~ lkj_corr_cholesky( 2 );
    to_vector( Z ) ~ normal( 0 , 1 );
    bA ~ normal( 0 , 1 );
    bW ~ normal( 0 , 1 );
    a ~ normal( 0 , 1 );
    for ( i in 1:300 ) {
        // LM 4 receiving A: Varying effects & substantial 'W'
        RA[i] = gr[HA[i], 2] + bW[2] * W[HA[i]];
        // LM 4 giving of B: Varying effects & substantial 'W'
        GB[i] = gr[HB[i], 1] + bW[1] * W[HB[i]];
        // LM 4 tie strength BA: Vayring effects & substantial 'A' 
        TBA[i] = T[D[i], 2] + bA * A[i];
        // Lower level linear model  
        lambdaBA[i] = a + TBA[i] + GB[i] + RA[i];

        // SYMMETRIC
        
        // LM 4 receiving B: Varying effects & substantial 'W'
        RB[i] = gr[HB[i], 2] + bW[2] * W[HB[i]];
        // LM 4 giving of A: Varying effects & substantial 'W'
        GA[i] = gr[HA[i], 1] + bW[1] * W[HA[i]];
        // LM 4 tie strength AB: Vayring effects & substantial 'A' 
        TAB[i] = T[D[i], 1] + bA * A[i];
        // Lower level linear model 
        lambdaAB[i] = a + TAB[i] + GA[i] + RB[i];
    }
    GBA ~ poisson_log( lambdaBA );
    GAB ~ poisson_log( lambdaAB );
}
generated quantities{
     matrix[2,2] Rho_T;
     matrix[2,2] Rho_gr;
    Rho_gr = multiply_lower_tri_self_transpose(L_Rho_gr);
    Rho_T = multiply_lower_tri_self_transpose(L_Rho_T);
}
