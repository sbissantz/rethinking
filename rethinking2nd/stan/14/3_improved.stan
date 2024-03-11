data{
    array[504] int P;
    array[504] int B;
    array[504] int T;
    array[504] int A;
}
parameters{
     matrix[4,7] zA; # Treatments x Actors – order; will  be transposed!
     matrix[4,6] zB; # Treatments x Blocks - order; will be transposed!
     vector[7] zAbar;
     vector[6] zBbar;
     real<lower=0> tau_B;
     real<lower=0> tau_A;
     vector<lower=0>[4] sigma_A;
     vector<lower=0>[4] sigma_B;
     # Cholesky factor is a paramter to be estimated!
     # 4x4 lower triangular matrix
     cholesky_factor_corr[4] L_Rho_B;
     cholesky_factor_corr[4] L_Rho_A;
}
transformed parameters{
     matrix[7,4] a; # Actors x Treatments
     matrix[6,4] b; # Blocks x Treatments
     vector[7] abar; # Actor means
     vector[6] bbar; # Block means
    # Noncentered Note: Pool means, too!
    bbar = zBbar * tau_B;
    abar = zAbar * tau_A;
    # Non-centerd 
    b = (diag_pre_multiply(sigma_B, L_Rho_B) * zB)';
    a = (diag_pre_multiply(sigma_A, L_Rho_A) * zA)';
    # 4 x 7 = [((4 x 4) * (4 x 4)) * (4 x 7)]^t
}
model{
     vector[504] p;
    L_Rho_B ~ lkj_corr_cholesky( 4 );
    L_Rho_A ~ lkj_corr_cholesky( 4 );
    sigma_B ~ exponential( 1 );
    sigma_A ~ exponential( 1 );
    tau_A ~ exponential( 1 );
    tau_B ~ exponential( 1 );
    zBbar ~ normal( 0 , 1 );
    zAbar ~ normal( 0 , 1 );
    to_vector( zB ) ~ normal( 0 , 1 );
    to_vector( zA ) ~ normal( 0 , 1 );
    for ( i in 1:504 ) {
        p[i] = abar[A[i]] + a[A[i], T[i]] + bbar[B[i]] + b[B[i], T[i]];
        p[i] = inv_logit(p[i]);
    }
    P ~ bernoulli( p );
}
generated quantities{
    vector[504] log_lik;
     vector[504] p;
     matrix[4,4] Rho_A;
     matrix[4,4] Rho_B;
    # Rho = LL' – cannot interpret Choleskey factors, therefore need to convert 
    Rho_B = multiply_lower_tri_self_transpose(L_Rho_B);
    Rho_A = multiply_lower_tri_self_transpose(L_Rho_A);
    for ( i in 1:504 ) {
        p[i] = abar[A[i]] + a[A[i], T[i]] + bbar[B[i]] + b[B[i], T[i]];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:504 ) log_lik[i] = bernoulli_lpmf( P[i] | p[i] );
}