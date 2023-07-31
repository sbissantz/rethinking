data {
   int<lower=0> N; 
}
parameters {
  real v;
  real z;
}
transformed parameters {
  real x;
  x = z * exp(v);
}
model {
  v ~ normal(0, 3); 
  z ~ std_normal(); 
}
