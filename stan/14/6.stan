data{
     int N;
     vector[1000] W;
     vector[1000] E;
     vector[1000] Q;
}
parameters{
     real aE;
     real aW;
     real bQE;
     real bEW;
     corr_matrix[2] Rho;
     vector<lower=0>[2] sigma;
}
transformed parameters { 
    vector[1000] muW;
    vector[1000] muE;
    // MVNormal requires list or array of vectors
    array[1000] vector[2] WE;
    array[1000] vector[2] MU; 
    /////////////////////////////////////////////
    // Linear model: (direct) effect of Q on E
    muE = aE + bQE * Q;
    // Linear model: (direct) effect of E on W
    muW = aW + bEW * E; 
    // Note: Have to loop because of the array structure
    for ( j in 1:1000 ) { 
        // Build the mean vector (transposed) 
        MU[j] = [ muW[j] , muE[j] ]';
        // Build the outcome vector (transposed)
        WE[j] = [ W[j] , E[j] ]';
    }
}
model{
    sigma ~ exponential( 1 );
    Rho ~ lkj_corr( 2 );
    bEW ~ normal( 0 , 0.5 );
    bQE ~ normal( 0 , 0.5 );
    aW ~ normal( 0 , 0.2 );
    aE ~ normal( 0 , 0.2 );
    // Multivatiate outcome EW – Note: Sigma = SRS
    WE ~ multi_normal( MU , quad_form_diag(Rho , sigma) );
}
