data{
  int<lower=0> N;
  vector[N] C;
  matrix[N,1] X;
}
parameters {
  real a; 
  vector[1] b; 
  real<lower=0> sigma; 
}
model {
  a ~ normal(0,1);
  b ~ normal(0,1);
  sigma ~ exponential(1);
  C ~ normal_id_glm(X, a, b, sigma);
}
