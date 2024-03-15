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
// Impute M and G ignoring models for both
data {
    int<lower=0> N;
    // Mass (partially obserevd)
    int<lower=0> N_M_obs;
    int<lower=0> N_M_mis;
    array[N_M_obs] int<lower=1, upper=N> ii_M_obs;
    array[N_M_mis] int<lower=1, upper=N> ii_M_mis;
    // Brain size (outcome)
    vector[N] B; // Only complete cases of B are studied (outcome)
    vector[N] G; // Only complete cases of B are studied (outcome)
    matrix[N, N] Dmat;
}
parameters {
    real a;
    real bG;
    real bM;
    real<lower=0> sigma;
    real<lower=0> eta_sq;
    real<lower=0> rho;
    // Imputation
    // Body mass
    array[N_M_mis] real M_mis; 
    array[N_M_obs] real M_obs;
}
transformed parameters {
    real<lower=0> sigma_sq;
    sigma_sq = square(sigma);

    // Imputation
    // Body mass: Merge missing and observed values
    array[N] real M;
    M[ii_M_obs] = M_obs; 
    M[ii_M_mis] = M_mis; 
}
model { 
    vector[N] mu; 
    matrix[N, N] K;
    // Priors
    eta_sq ~ normal(1, 0.25);
    rho ~ normal(3, 0.25);
    bM ~ normal(0, 0.5);
    bG ~ normal(0, 0.5);
    a ~ normal(0,1);
    sigma ~ exponential(1);
    // Linear model
    mu = a + bG * G + bM * M;
    // Kernel matrix
    K = cov_GPL1(Dmat, eta_sq, rho, 0.01);
    // Likelihood or resiuadl prior 
    B ~ multi_normal(mu, K);
}
