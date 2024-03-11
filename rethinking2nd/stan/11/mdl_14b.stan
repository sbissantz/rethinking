data{
    int<lower=0> N;
    array[N] int<lower=0> y;
}
parameters{
    // Remeber: You can make p & l a linear model
    // p = logistic(X_i*beta+alpha)
    // lambda = exp(X_i*beta+alpha)
    // Here: p = alpha_p_ the average probability to drink 
    // Here: lambda = alpha_l the average n° manuscripts per day
    real<lower=0, upper=1> p; //alpha_p
    real<lower=0> lambda; //alpha_l
}
model {
    p ~ normal( 1 , 0.5 );
    lambda ~ normal( -1.5 , 1 );
    for (n in 1:N) {
      if (y[n] == 0) {
        target += log_sum_exp(bernoulli_lpmf(1 | p),
                              bernoulli_lpmf(0 | p)
                                + poisson_lpmf(y[n] | lambda));
      } else {
        target += bernoulli_lpmf(0 | p)
                    + poisson_lpmf(y[n] | lambda);
    }
  }
}
