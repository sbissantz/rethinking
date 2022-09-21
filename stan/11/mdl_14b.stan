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
    lambda = exp(a_l)
    p = inv_logit(a_p);
}
model{
//Fix
    a_l ~ normal( 1 , 0.5 );
    a_p ~ normal( -1.5 , 1 );
    for(n in 1:N) {
      if (y[n] == 0) { 
      1 ~ bernoulli(p); 
      } else { 
      0 ~ bernoulli(p); 
      y[n] ~ poisson(lambda) T[1, ];
    }
  }
}

//how is the fast implementation of stan_lm stan_glm called




