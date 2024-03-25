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
    array[N_G_obs] real G_obs;
    array[N_G_obs] int<lower = 1, upper = N>  ii_G_obs; // TODO: switch to vector
    array[N_G_mis] int<lower = 1, upper = N>  ii_G_mis;
    // Body Mass (imputation preparation) 
    int<lower = 0> N_M_obs;
    int<lower = 0> N_M_mis;
    array[N_M_obs] real M_obs;   
    array[N_M_obs] int<lower = 1, upper = N> ii_M_obs; // TODO: switch to vector
    array[N_M_mis] int<lower = 1, upper = N> ii_M_mis;
    // Oldje stuff 
    vector[N] B; // Only complete cases of B are studied (outcome)
    // vector[N] G; // Only complete cases of M are studied (outcome)
    matrix[N, N] Dmat;
}
parameters {
    // Impute Group Size
    array[N_G_mis] real G_mis; 
    // Impute Body Mass 
    array[N_M_mis] real M_mis; 
    // Oldje stuff
    real a;
    real bG;
    real bM;
    real<lower=0> sigma;
    real<lower=0> eta_sq;
    real<lower=0> rho;
}
transformed parameters {
    real<lower=0> sigma_sq;
    sigma_sq = square(sigma);
    // Impute Group Size
    array[N] real G_arr;
    G_arr[ii_G_obs] = G_obs;
    G_arr[ii_G_mis] = G_mis;
    vector[N] G = to_vector(G_arr);
    // Impute Body Mass
    array[N] real M_arr;
    M_arr[ii_M_obs] = M_obs;
    M_arr[ii_M_mis] = M_mis;
    vector[N] M = to_vector(M_arr);
}
model { 
    vector[N] mu; 
    matrix[N, N] K;
    // Priors
    eta_sq ~ normal(1, 0.25);
    rho ~ normal(3, 0.25);
    bM ~ normal(0, 0.5);
    bG ~ normal(0, 0.5);
    a ~ std_normal();
    sigma ~ exponential(1);
    // Linear model
    for(i in 1:N) {
        mu[i] = a + bG * G[i] + bM * M[i];       
    }
    // Kernel matrix
    K = cov_GPL1(Dmat, eta_sq, rho, 0.01);
    // Likelihood or residual prior 
    B ~ multi_normal(mu, K);
}
