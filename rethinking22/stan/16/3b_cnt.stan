data{
     int N_spp;
     vector[N_spp] B;
     matrix[N_spp, 2] X;
}
parameters{
     real a;
     vector[2] b;
     real<lower=0> sigma;
}
model{
    sigma ~ exponential( 1 );
    to_vector(b) ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 1 );
    B ~ normal_id_glm( X , a, b, sigma );
}
