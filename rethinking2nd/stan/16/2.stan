data{
    int N; // Number of observations
    int S; // Number of strategies
    array[N] int y; // Behvior shown by each subject
    array[N] int majority_first; 
}
parameters{
    // Probability of each strategy
    simplex[S] p;
}
model{
     
    vector[S] phi;
    // prior
    p ~ dirichlet( rep_vector(4,S) );
    // probability of data – likelihood
    for ( i in 1:N ) {
        // If subject i chooses strategy 1, then phi[1] = 1, else 0
        if ( y[i]==2 ) phi[1]=1; else phi[1]=0; // majority
        if ( y[i]==3 ) phi[2]=1; else phi[2]=0; // minority
        if ( y[i]==1 ) phi[3]=1; else phi[3]=0; // maverick
        phi[4]=1.0/3.0;                         // random
        if ( majority_first[i]==1 )             // follow first
            if ( y[i]==2 ) phi[S]=1; else phi[5]=0;
        else
            if ( y[i]==3 ) phi[S]=1; else phi[5]=0;
        
        // compute log( p_s * Pr(y_i|s )
        for ( j in 1:5 ) phi[j] = log(p[j]) + log(phi[j]);
        // compute average log-probability of y_i
        target += log_sum_exp( phi );
    }
}
