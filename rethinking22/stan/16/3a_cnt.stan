data{
     int N_spp;
     vector[N_spp] B;
     vector[N_spp] G;
     vector[N_spp] M;
}
parameters{
     real a;
     real bG;
     real bM;
     real<lower=0> sigma;
}
transformed parameters {
     vector[N_spp] mu;
    for ( i in 1:N_spp ) {
        mu[i] = a + bM * M[i] + bG * G[i];
    }
}
model{
    sigma ~ exponential( 1 );
    bM ~ normal( 0 , 0.5 );
    bG ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 1 );
    B ~ normal( mu , sigma );
}