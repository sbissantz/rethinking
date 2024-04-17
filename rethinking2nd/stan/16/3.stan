// Boxes model with gender as covariate
data{
    int N; // number of observations
    int S; // number of strategies
    int G; // number of gender categories 
    array[N] int y;
    array[N] int majority_first;
    array[N] int gender;
}
parameters{
    // Important for the outcome array of two simplexes
    array[G] simplex[S] p;
}
model{
    vector[S] phi;
    
    // prior
    for ( g in 1:G ) p[g] ~ dirichlet( rep_vector(4,5) );
    
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
        
        // compute log( p_s * Pr(y_i|s) )
        for ( s in 1:S ) phi[s] = log(p[gender[i],s]) + log(phi[s]);
        // compute average log-probability of y_i
        target += log_sum_exp( phi );
    }
}
