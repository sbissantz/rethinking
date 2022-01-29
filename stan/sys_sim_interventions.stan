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
  vector[K] h;  
  real<lower=0> tau;
}
model {
  // weight 
  vector[N] mu;
  sigma ~ uniform(0,10);
  a ~ normal(60,10);
  b ~ lognormal(0,1);
  for(i in 1:N) {
    mu[i] = a[S[i]]+b[S[i]] * (H[i]-Hbar);
  }
  W ~ normal(mu, sigma);
  // height 
  vector[N] nu;
  tau ~ uniform(0,10);
  h ~ normal(0, 10);
  for(i in 1:N) {
    nu[i] = h[S[i]];
  }
  H ~ normal(nu, tau);
}
