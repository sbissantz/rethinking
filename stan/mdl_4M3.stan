data {
int<lower=1> n;
vector[n] M;
vector[n] D;
vector[n] A;
}
parameters {
real alpha, eta;
real beta_D, beta_A;
real theta_A; 
real<lower=0> sigma, tau;
}
model {
// D <- A ->M
M ~ normal(alpha + beta_D*D + beta_A*A, sigma);
alpha ~ normal(0, 0.2); 
beta_D ~ normal(0, 0.2);
beta_A ~ normal(0, 0.2);
sigma ~ exponential(1);
// A -> D
D ~ normal(eta + theta_A*A, tau);
eta ~ normal(0, 0.2); 
theta_A ~ normal(0, 0.5);
tau ~ exponential(1);
}

