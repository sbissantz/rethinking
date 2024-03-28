functions {
    matrix cov_GPL1(matrix x, real alpha_sq, real rho, real delta) {
        int N = rows(x);
        matrix[N, N] K;
        for (i in 1:(N-1)) {
            K[i,i] = alpha_sq + delta;
            for (j in (i+1) : N) {
                K[i,j] = alpha_sq * exp(-rho * x[i,j]); 
                K[j,i] = K[i,j];
            }
        }
    K[N,N] = alpha_sq + delta;
    return K;
    }
}
// Impute Group Size
data { 
    int<lower = 0>  N;
    // Group Size (imputation preparation)
    int<lower = 0>  N_G_obs;
    int<lower = 0>  N_G_mis;
    vector[N_G_obs] G_obs;
    array[N_G_obs] int<lower = 1, upper = N>  ii_G_obs; 
    array[N_G_mis] int<lower = 1, upper = N>  ii_G_mis;
    // Body Mass (imputation preparation) 
    int<lower = 0> N_M_obs;
    int<lower = 0> N_M_mis;
    vector[N_M_obs] M_obs;   
    array[N_M_obs] int<lower = 1, upper = N> ii_M_obs; 
    array[N_M_mis] int<lower = 1, upper = N> ii_M_mis;
    // Oldje stuff 
    vector[N] B; // Only complete cases of B are studied (outcome)
    // vector[N] G; // Only complete cases of M are studied (outcome)
    matrix[N, N] Dmat;
}
parameters {
    // Impute Group Size
    vector[N_G_mis] G_mis; 
    // Impute Body Mass 
    vector[N_M_mis] M_mis; 
    // Oldje stuff
    real aB;
    real aG;
    real bGB;
    real bMB;
    real bMG;
    real<lower=0> eta_sq_B;
    real<lower=0> eta_sq_G;
    real<lower=0> rho_B;
    real<lower=0> rho_G;
}
transformed parameters {
    // Impute Group Size
    vector[N] G;
    G[ii_G_obs] = G_obs;
    G[ii_G_mis] = G_mis;
    // Impute Body Mass
    vector[N] M;
    M[ii_M_obs] = M_obs;
    M[ii_M_mis] = M_mis;
}
model { 
    vector[N] mu; 
    vector[N] nu; 
    matrix[N, N] KB;
    matrix[N, N] KG;
    // Priors
    rho_B ~ normal(3, 0.25);
    rho_G ~ normal(3, 0.25);
    eta_sq_B ~ normal(1, 0.25);
    eta_sq_G ~ normal(1, 0.25);
    bMB ~ normal(0, 0.5);
    bGB ~ normal(0, 0.5);
    bMG ~ normal(0, 0.5);
    aB ~ std_normal();
    aG ~ std_normal();
    // Kernel matrix
    KB = cov_GPL1(Dmat, eta_sq_B, rho_B, 0.01);
    KG = cov_GPL1(Dmat, eta_sq_G, rho_G, 0.01);
    // Prior/likelihood for mixed variables
    // IMPORTANT: Folks Theorem of Statistical Computing!
    // If you forget to set these priors your runtime goes to 3 hours!
    // Took me 3 days to find out and fix this!
    M ~ normal( 0 , 1 );
    // Likelihood or residual prior 
    for(i in 1:N) {
        nu[i] = aG + bMG * M[i];       
    }
    G ~ multi_normal(nu , KG);
    // Linear model
    for(i in 1:N) {
        mu[i] = aB + bGB * G[i] + bMB * M[i];       
    }
    // Likelihood or residual prior 
    B ~ multi_normal(mu, KB);
}
