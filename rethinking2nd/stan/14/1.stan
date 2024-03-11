data{
    int n_i; // number of observations
    int n_j; // number of clusters 
    vector[n_i] wait; // outcome
    array[n_i] int<lower=0, upper=1> afternoon; // predictor
    array[n_i] int<lower=1, upper=20> cafe; // cluster
}
parameters{
    vector[n_j] a_j; // varying intercept
    vector[n_j] b_j; // varying slope
    real a; // population intercept
    real b; // population slope
    vector<lower=0>[2] sigma_j; //between-cluster stdev
    // Add: ...for varying effects (i.e., sigma_aj, sigma_bj) 
    real<lower=0> sigma; //stdev within clusters
    // Add: ...for waiting times within cafes 
    corr_matrix[2] Rho; // varying effects correlation matrix
}
model{
    vector[200] mu; // mean vector
    Rho ~ lkj_corr( 2 ); // prior for correlation matrix
    sigma_j ~ exponential( 1 ); // prior for between-cluster stdev
    sigma ~ exponential( 1 ); // prior for within-cluster stdev
    b ~ normal( -1 , 0.5 ); // prior for population slope
    a ~ normal( 5 , 2 ); // prior for population intercept
    { //local scope
    array[20] row_vector[2] P; // Varying effects population 
    // Note: Array of vectors required for P ~ MVN
    row_vector[2] MU; // Varying effects population mean
    MU = [ a , b ];
    // Build the population
    for ( j in 1:n_j ) P[j] = [ a_j[j] , b_j[j] ];
    // Population of varying effects MVN(MU, SIGMA)
    P ~ multi_normal( MU , quad_form_diag(Rho , sigma_j) );
    }
    for ( i in 1:n_i ) {
        // Linear model 
        mu[i] = a_j[cafe[i]] + b_j[cafe[i]] * afternoon[i]; 
    }
    wait ~ normal( mu , sigma ); // Likelihood
}
