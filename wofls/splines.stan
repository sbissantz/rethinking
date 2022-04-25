//
// Basis Splines
//
data{
  int<lower=0> N; // data items
  int<lower=0> K; // parameters
  matrix[N, K] B; // predictor matrix
  vector[N] D; // outcome vector
}
parameters{
  vector[K] w; // coefficients for predictors 
  real alpha; // intercept
  real<lower=0> sigma; // error scale 
}
model{
  D ~ normal(B*w + alpha, sigma); //likelihood
  alpha ~ normal(100,10); // prior
  w ~ normal(0,1000); // prior
  sigma ~ exponential(1); // prior 
}
generated quantities{
  vector[N] mu = 
  B*w + alpha ;
}
