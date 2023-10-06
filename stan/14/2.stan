data{
    array[504] int L;
    array[504] int block_id;
    array[504] int actor_id;
    array[504] int treatment_id;
}
parameters{
    corr_matrix[4] Rho_actor;
    corr_matrix[4] Rho_block;
    vector<lower=0>[4] sigma_actor;
    vector<lower=0>[4] sigma_block;
    array[6] vector[4] beta;
    array[7] vector[4] alpha;
    vector[4] gamma;
}
model{
    vector[504] p;
    Rho_block ~ lkj_corr( 4 );
    Rho_actor ~ lkj_corr( 4 );
    sigma_block ~ exponential( 1 );
    sigma_actor ~ exponential( 1 );
    gamma ~ normal( 0 , 1 );
    beta ~ multi_normal( rep_vector(0,4) , quad_form_diag(Rho_block , sigma_block) );
    alpha ~ multi_normal( rep_vector(0,4) , quad_form_diag(Rho_actor , sigma_actor) );
    for ( i in 1:504 ) {
        p[i] = gamma[treatment_id[i]] + alpha[actor_id[i], treatment_id[i]] + beta[block_id[i], treatment_id[i]];
    }
    L ~ bernoulli_logit( p );
}