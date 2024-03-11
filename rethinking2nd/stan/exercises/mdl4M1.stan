data {
  //
}
parameters {
  //
}
model {
  //
}
generated quantities{
  real mu = normal_rng(0,10) ;
  real<lower=0> sigma = exponential_rng(1) ;
  real y_sim = normal_rng(mu, sigma) ;
}
