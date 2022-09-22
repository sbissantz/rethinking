functions {
  int num_zeros(array[] int y) {
    int sum = 0;
    for (n in 1:size(y)) {
      sum += (y[n] == 0);
    }
    return sum;
  }
}
data{
    int<lower=0> N;
    array[N] int<lower=0> y;
}
transformed data {
  int<lower=0> N_zero = num_zeros(y);
  array[N - N_zero] int<lower=1> y_nonzero;
  int N_nonzero = 0;
  for (n in 1:N) {
    if (y[n] == 0) continue;
    N_nonzero += 1;
    y_nonzero[N_nonzero] = y[n];
  }
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
       target
     += N_zero
          * log_sum_exp(bernoulli_lpmf(1 | p),
                        bernoulli_lpmf(0 | p)
                          + poisson_lpmf(0 | lambda));
   target += N_nonzero * bernoulli_lpmf(0 | p);
   target += poisson_lpmf(y_nonzero | lambda);
}

