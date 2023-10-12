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
     vector<lower=0>[2] Sigma;
}
model{
     vector[1000] muW;
     vector[1000] muE;
    Sigma ~ exponential( 1 );
    Rho ~ lkj_corr( 2 );
    bEW ~ normal( 0 , 0.5 );
    bQE ~ normal( 0 , 0.5 );
    aW ~ normal( 0 , 0.2 );
    aE ~ normal( 0 , 0.2 );
    for ( i in 1:1000 ) {
        muE[i] = aE + bQE * Q[i];
    }
    for ( i in 1:1000 ) {
        muW[i] = aW + bEW * E[i];
    }
    {
    array[1000] vector[2] W;
    array[1000] vector[2] MU;
    for ( j in 1:1000 ) MU[j] = [ muW[j] , muE[j] ]';
    for ( j in 1:1000 ) W[j] = [ W[j] , E[j] ]';
    YY ~ multi_normal( MU , quad_form_diag(Rho , Sigma) );
    }
}
