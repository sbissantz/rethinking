data{ 
    int N;
    array[100] int RC;
    array[100] int notes;
    array[100] int cat;
}
parameters{ 
    real a;
    real b;
    real<lower=0,upper=1> k;
}
model{ 
    vector[100] lambda;
    k ~ beta( 2 , 2 );
    b ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 1 );
    for ( i in 1:100 ) {
        if ( RC[i] == 1 ) { // missing
            target += log_sum_exp(
                // a + b * C if C=1, then a + b*1 = a + b
                // since log_lpmf, not exp(a+b)
                log(k) + poisson_log_lpmf(notes[i] | a + b), 
                // a + b * C if C=1, then a + b*0 = a
                // since log_lpmf, not exp(a)
                log(1 - k) + poisson_log_lpmf(notes[i] | a));                  
        } 
        // Note since poisson_log no exp(lambda)
        lambda[i] = a + b * cat[i];
        if ( RC[i] == 0 ) { // observed
            // ------------------------
            // Sneaking cat model 
            // If cat is known, then update the probability that cat is present: k
            cat[i] ~ bernoulli( k );
            // ------------------------
            // Normal Poisson model
            notes[i] ~ poisson_log( lambda[i] );
        }
    }
}
generated quantities{
     vector[N] PrC1; // Posterior conditional on cat present
     vector[N] lpC1; // Likelihood conditional on cat present
     vector[N] lpC0; // Likelihood conditional on cat absend 
    for ( i in 1:N ) {
        // Imputed values
        // since log_lpmf, not exp(a+b) or exp(a)
        lpC0[i] = log(1 - k) + poisson_log_lpmf(notes[i] | a);
        lpC1[i] = log(k) + poisson_log_lpmf(notes[i] | a + b);
        // Posterior: Pr(C_i = 1 | N_i)
        PrC1[i] = exp(lpC1[i])/(exp(lpC1[i]) + exp(lpC0[i]));
    }
}
