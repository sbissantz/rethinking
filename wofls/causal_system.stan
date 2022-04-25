//
// Simulating a causal system
//

data {
  int<lower=0> N;
  vector[N] D;
  vector[N] M;
  vector[N] A;
  vector[N] M_seq;
  vector[N] A_seq;
}
parameters {
  real a;
  real b_AD;
  real b_MD;
  real<lower=0> sigma;
  real k;
  real b_AM;
  real<lower=0> tau;
}
model {
  // Data distribution I
  M ~ normal(k + b_AM*A, tau);
  // Priors
  k ~ normal(0,0.2);
  b_AM ~ normal(0,0.5);
  tau ~ exponential(1);
  // Data distribution II
  D ~ normal(a + b_AD*A + b_MD*M, sigma);
  // Priors
  a ~ normal(0,0.2);
  b_AD ~ normal(0,0.5);
  b_MD ~ normal(0,0.5);
  sigma ~ exponential(1);
}
generated quantities {
  // Simulating nu 
  vector[N] nu = 
  k + b_AM*A_seq;
  // Simulating mu for A=1
  vector[N] mu =  
  a + b_AD*A_seq + b_MD*M_seq;
  // mu | A=0
  vector[N] mu_A0 =  
  a + b_AD*0 + b_MD*M_seq;
  real M_tilde[N] =
  normal_rng(nu, tau);
  // D | A = 1
  real D_tilde[N] =
  normal_rng(mu, sigma);
  // D | A = 0
  real D_tilde_A0[N] =
  normal_rng(mu_A0, sigma);
}

