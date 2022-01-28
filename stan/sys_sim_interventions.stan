data {
  int<lower=1> N;
  int<lower=1, upper=2> K;
  vector[N] W;
  vector[N] H;
  int<lower=1, upper=K> S[N];
}
transformed data {
 real Hbar;
 Hbar = mean(H);
}
parameters {
  vector[K] a;  
  vector<lower=0>[K] b;  
  real<lower=0> sigma;
  real h;  
  real<lower=0> tau;
}
model {
  // weight 
  W ~ normal(a[S]+b[S]*(H-Hbar), sigma);
  a ~ normal(60,10)
  b ~ lognormal(0,1)
  sigma ~ uniform(0,10)
  // height 
  H ~ normal(h[S], sigma) 
  h ~ normal(0, 10) 
# H_i ~ normal(nu_i, tau)
# nu_i = h_S[i] 
# tau ~ uniform(0,10)
}




