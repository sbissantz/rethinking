data{
     int N;
    array[100] int X;
    array[100] int dyad_id;
    array[100] int mom_id;
}
parameters{
     real a;
     vector[100] z;
     real<lower=0> sigma;
     vector[100] x;
     real<lower=0> tau;
}
model{
     vector[100] p;
    tau ~ normal( 0 , 1 );
    x ~ normal( 0 , 1 );
    sigma ~ normal( 0 , 1 );
    z ~ normal( 0 , 1 );
    a ~ normal( 0 , 1.5 );
    // Mixture
    for ( i in 1:100 ) {
        p[i] = a + z[mom_id[i]] * sigma + x[dyad_id[i]] * tau;
        p[i] = inv_logit(p[i]);
    }
    X ~ bernoulli( p );
}
