//
// Simulation based calibration
//

data {
    int<lower=0> N ;
    int Hbar;
    vector[N] H;
    vector[N] W;
}
parameters {
   real alpha; 
   real<lower=0> beta; 
   real<lower=0> sigma; 
}
model {
   W ~ normal(alpha + beta * (H - Hbar), sigma);
   // Determining the values for... 
   alpha ~ normal(178, 20); 
   beta ~ lognormal(0, 1);
   sigma ~ exponential(1);
}
