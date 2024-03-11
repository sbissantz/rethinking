data{
    int<lower=1> N;
    int<lower=1> K;
    array [N] int R;
    array [N] int I;
    array [N] int A;
    array [N] int E;
    array [N] int C;
    vector[K] alpha;
}
parameters{
    ordered[K-1] c;
    real bE;
    real bC;
    real bI;
    real bA;
    simplex[K] delta;
}
transformed parameters{
    vector[N] eta;
    vector[K+1] delta_j;
    delta_j = append_row(0, delta);
    for ( i in 1:N ) {
        eta[i] = bE * sum(delta_j[1:E[i]]) + bA * A[i] + bI * I[i] + bC * C[i];
    }
}
model{
    delta ~ dirichlet( alpha );
    bA ~ normal( 0 , 1 );
    bI ~ normal( 0 , 1 );
    bC ~ normal( 0 , 1 );
    bE ~ normal( 0 , 1 );
    c ~ normal( 0 , 1 );
    R ~ ordered_logistic( eta , c );
}
