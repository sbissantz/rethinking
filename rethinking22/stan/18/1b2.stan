functions{
    matrix cov_GPL1(matrix x, real sq_alpha, real sq_rho, real delta) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho * x[i,j] );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + delta;
        return K;
    }
    vector merge_missing( array[] int miss_indexes , vector x_obs , vector x_miss ) {
        int N = dims(x_obs)[1];
        int N_miss = dims(x_miss)[1];
        vector[N] merged;
        merged = x_obs;
        for ( i in 1:N_miss )
            merged[ miss_indexes[i] ] = x_miss[i];
        return merged;
    }
}
data{
     matrix[184,184] Imat;
     int N_spp;
     vector[184] B;
     vector[184] M;
     array[2] int M_missidx;
     vector[184] G;
     array[33] int G_missidx;
     matrix[184,184] Dmat;
}
parameters{
     real a;
     real bG;
     real bM;
     real<lower=0> etasq;
     real<lower=0> rho;
     vector[2] M_impute;
     vector[33] G_impute;
}
model{
     vector[184] mu;
     vector[184] M_merge;
     vector[184] G_merge;
     matrix[N_spp,N_spp] K;
    rho ~ normal( 3 , 0.25 );
    etasq ~ normal( 1 , 0.25 );
    bM ~ normal( 0 , 0.5 );
    bG ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 1 );
    K = cov_GPL1(Dmat, etasq, rho, 0.01);
    G_merge = merge_missing(G_missidx, to_vector(G), G_impute);
    G_merge ~ normal( 0 , 1 );
    M_merge = merge_missing(M_missidx, to_vector(M), M_impute);
    M_merge ~ normal( 0 , 1 );
    for ( i in 1:184 ) {
        mu[i] = a + bM * M_merge[i] + bG * G_merge[i];
    }
    B ~ multi_normal( mu , K );
}