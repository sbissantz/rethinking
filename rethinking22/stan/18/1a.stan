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
// Complete case analysis
data {
    int<lower=0> N;
    vector[N] B; // Only complete cases of B are studied (outcome)
    vector[N] M; // Only complete cases of M are studied (outcome)
    vector[N] G; // Only complete cases of M are studied (outcome)
    matrix[N, N] Dmat;
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
    mu = a + bG * G + bM * M;
    // Kernel matrix
    K = cov_GPL1(Dmat, eta_sq, rho, 0.01);
    // Likelihood or residual prior 
    B ~ multi_normal(mu, K);
}
