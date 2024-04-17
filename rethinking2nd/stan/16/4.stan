// Boxes model with age as covariate
data{
    int N; // number of observations
    int S; // number of strategies
    array[N] int y;
    array[N] int majority_first;
    array[N] real age;
}
parameters{
    vector[S] alpha;
    vector[S] beta;
}
model{
    vector[S] phi;
    vector[S] p;
    
    // prior
    alpha ~ normal(0,1);
    beta ~ normal(0,0.5);
    
    // probability of data
    for ( i in 1:N ) {
        if ( y[i]==2 ) phi[1]=1; else phi[1]=0; // majority
        if ( y[i]==3 ) phi[2]=1; else phi[2]=0; // minority
        if ( y[i]==1 ) phi[3]=1; else phi[3]=0; // maverick
        phi[4]=1.0/3.0;                         // random
        if ( majority_first[i]==1 )             // follow first
            if ( y[i]==2 ) phi[5]=1; else phi[5]=0;
        else
            if ( y[i]==3 ) phi[5]=1; else phi[5]=0;

        // calculate p vector for this case
        p = softmax( alpha + beta*age[i] );
        // compute log( p_s * Pr(y_i|s) )
        for ( s in 1:S ) phi[s] = log(p[s]) + log(phi[s]);
        // compute average log-probability of y_i
        target += log_sum_exp( phi );
    }
}