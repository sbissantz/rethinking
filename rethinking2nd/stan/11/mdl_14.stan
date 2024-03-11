data{
    int<lower=0> N;
    array[N] int y;
}
parameters{
    real a_p;
    real a_l;
}
transformed parameters {
    real logit_p;
    real p;
    real log_lambda;
    real lambda;
    log_lambda = a_l;
    lambda = exp(log_lambda);
    logit_p = a_p;
    p = inv_logit(logit_p);
}
model{
    a_l ~ normal( 1 , 0.5 );
    a_p ~ normal( -1.5 , 1 );
    for ( i in 1:N )
      if ( y[i] == 0 ) { 
      target += log_mix(p, 0, poisson_lpmf(0 | lambda)); 
      } else { 
      // Check that all y_i > 0: check
      target += log1m(p) + poisson_lpmf(y[i] | lambda); 
      }
}
