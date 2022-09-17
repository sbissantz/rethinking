data{
    int<lower=0> N_obs;
    int<lower=0> N_cens;
    int<lower=0> cno;
    int<lower=0> U;
    // real<lower=max(D_obs)> U;
    array[N_obs] real D_obs;
    array[N_obs] int CID_obs;
}
parameters{
    vector[cno] a;
}
transformed parameters {
    vector[N_obs] lambda;
    vector[N_obs] mu;
    for ( i in 1:N_obs ) {
        mu[i] = a[CID_obs[i]];
        mu[i] = exp(mu[i]);
    }
    for ( i in 1:N_obs ) {
        lambda[i] = 1/mu[i];
    }
}
model{
    a ~ normal( 0 , 1 );
    D_obs ~ exponential( lambda );
    target += N_cens * exponential_lccdf(U | lambda);
}
