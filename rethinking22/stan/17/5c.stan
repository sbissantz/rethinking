data{
     int N;
     real f;
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
     vector[100] log_p;
    tau ~ normal( 0 , 1 );
    x ~ normal( 0 , 1 );
    sigma ~ normal( 0 , 1 );
    z ~ normal( 0 , 1 );
    a ~ normal( 0 , 1.5 );
    for ( i in 1:100 ) {
        // Different loss function!
        log_p[i] = log_inv_logit(a + z[mom_id[i]] * sigma + x[dyad_id[i]] * tau);
    }
    for ( i in 1:100 ) 
        if ( X[i] == 0 ) target += log1m_exp(log_p[i]) + log1m(f);
    for ( i in 1:100 ) 
        if ( X[i] == 1 ) target += log_sum_exp(log_p[i], log1m_exp(log_p[i]) + log(f));
}
