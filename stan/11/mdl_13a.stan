data{
    int<lower=0> N;
    int<lower=0> cno;
    array[N] int A;
    vector[N] D;
    array[N] int CID;
}
parameters{
    vector[cno] a;
}
transformed parameters {
    vector[N] lambda;
    vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = a[CID[i]];
        mu[i] = exp(mu[i]);
    }
    for ( i in 1:N ) {
        lambda[i] = 1/mu[i];
    }
}
model{
    a ~ normal( 0 , 1 );
    for ( i in 1:N )
        if ( A[i] == 0 ) target += exponential_lccdf(D[i] | lambda[i]);
    for ( i in 1:N )
        if ( A[i] == 1 ) D[i] ~ exponential( lambda[i] );
}
