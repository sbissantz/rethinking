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
    // Brain Size (imputation preparation) 
    int<lower = 0> N_B_obs;
    int<lower = 0> N_B_mis;
    vector[N_B_obs] B_obs;   
    array[N_B_obs] int<lower = 1, upper = N> ii_B_obs; 
    array[N_B_mis] int<lower = 1, upper = N> ii_B_mis;
    // Distance matrix
    matrix[N, N] Dmat;
}
parameters {
    // Impute Group Size
    vector[N_G_mis] G_mis; 
    // Impute Body Mass 
    vector[N_M_mis] M_mis; 
    // Impute Brain Size 
    vector[N_B_mis] B_mis; 
    // Oldje stuff
    real aB;
    real aG;
    real bGB;
    real bMB;
    real bMG;
    real<lower=0> eta_sq_M;
    real<lower=0> eta_sq_G;
    real<lower=0> eta_sq_B;
    real<lower=0> rho_M;
    real<lower=0> rho_G;
    real<lower=0> rho_B;
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
    // Impute Brain SIze 
    vector[N] B;
    B[ii_B_obs] = B_obs;
    B[ii_B_mis] = B_mis;
}
model { 
    vector[N] mu; 
    vector[N] nu; 
    matrix[N, N] KM;
    matrix[N, N] KG;
    matrix[N, N] KB;
    // Priors
    rho_M ~ normal(3, 0.25);
    rho_G ~ normal(3, 0.25);
    rho_B ~ normal(3, 0.25);
    eta_sq_M ~ normal(1, 0.25);
    eta_sq_G ~ normal(1, 0.25);
    eta_sq_B ~ normal(1, 0.25);
    bMB ~ normal(0, 0.5);
    bGB ~ normal(0, 0.5);
    bMG ~ normal(0, 0.5);
    aG ~ std_normal();
    aB ~ std_normal();
    // Kernel matrix
    KM = cov_GPL1(Dmat, eta_sq_M, rho_M, 0.01);
    KG = cov_GPL1(Dmat, eta_sq_G, rho_G, 0.01);
    KB = cov_GPL1(Dmat, eta_sq_B, rho_B, 0.01);
    // Likelihood or residual prior 
    for(i in 1:N) {
        nu[i] = aG + bMG * M[i];       
        mu[i] = aB + bGB * G[i] + bMB * M[i];       
    }
    M ~ multi_normal(rep_vector(0,N) , KM);
    G ~ multi_normal(nu , KG);
    B ~ multi_normal(mu, KB);
}
