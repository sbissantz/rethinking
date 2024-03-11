functions {
    matrix cov_GPL2( matrix x, real alpha_sq, real rho_sq, real delta){
        int N = rows(x); // Number of rows
        matrix[N, N] K; // Covariance Kernel Matrix
        for ( i in 1:(N-1) ) {
            // Set the variances on the main diagonal, despite the last one
            // Note: alpha_sq: maximum covariance; reached with itself
            K[i,i] = alpha_sq + delta;
            for ( j in (i + 1):N ) {
                // Set the covairances according to the Kernel function
                // eta_sq and rho_sq control the functions shape
                K[i, j] = alpha_sq * exp(-rho_sq * square(x[i,j]));
                 // Covariance matrix is symmetric! Thus, "ji" is "ij"
                K[j,i] = K[i,j];
            }
        }
        // Set the last variance on the main diagonal 
        K[N, N] = alpha_sq + delta;
        return K;    
    }
}
data{
    int N;
    array[N] int P;
    array[N] int T;
    array[N] int S;
     matrix[N,N] D;
}
parameters{
     vector[N] z;
     real abar;
     real<lower=0> eta_sq;
     real<lower=0> rho_sq;
}
transformed parameters{
     matrix[10,10] K;
     vector[10] lambda;
     vector[10] a;
     matrix[10,10] L_K;
    K = cov_GPL2(D, eta_sq, rho_sq, 0.01);
    L_K = cholesky_decompose(K);
    a = L_K * z;
    // Linear model
    for ( i in 1:10 ) {
        // Raw assessment of spatial confounding: Space -> Tools
        // "How similar in tool counts are those given islands given their locations"
        lambda[i] = abar + a[S[i]];
    }
    

}
model{
    rho_sq ~ exponential( 0.5 );
    eta_sq ~ exponential( 2 );
    abar ~ normal( 3 , 0.5 );
    z ~ normal(0,1);
    T ~ poisson_log( lambda );
}
generated quantities{
    vector[10] log_lik;
    for ( i in 1:10 ) log_lik[i] = poisson_lpmf( T[i] | lambda[i] );
}

