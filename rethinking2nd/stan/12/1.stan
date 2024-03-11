data{
    int<lower=0> N; 
    int<lower=0> gno; 
    array[N] int N_bern;
    array[N] int A;
    array[N] int gid;
}
parameters{
    vector[gno] a;
    real<lower=0> chi;
}
transformed parameters{
    real kappa;
    kappa = chi + 2;
    vector[N] phi;
    for ( i in 1:N ) {
        phi[i] = a[gid[i]];
        phi[i] = inv_logit(phi[i]);
    }
}
model{
    chi ~ exponential(1);
    a ~ normal(0 , 1.5);
    A ~ beta_binomial(N_bern , phi*kappa , (1-phi)*kappa);
}
generated quantities {
    array[N] int A_tilde;
    A_tilde = beta_binomial_rng(N_bern, phi*kappa , (1-phi)*kappa);
}
