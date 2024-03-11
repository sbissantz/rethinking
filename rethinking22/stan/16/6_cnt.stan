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
data {
    int  N_spp;
    vector[N_spp] B;
    vector[N_spp] M;
    vector[N_spp] G;
    matrix[N_spp, N_spp] Dmat;
}
parameters {
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
    vector[N_spp] mu; 
    matrix[N_spp, N_spp] K;
    // Linear model
    mu = a + 0 * M;
    // Kernel matrix
    K = cov_GPL1(Dmat, eta_sq, rho, 0.01);
}
model { 
    eta_sq ~ normal(1, 0.25);
    rho ~ normal(3, 0.25);
    bM ~ normal(0, 0.5);
    bG ~ normal(0, 0.5);
    a ~ normal(0,1);
    sigma ~ exponential(1);
    B ~ multi_normal(mu, K);
}

