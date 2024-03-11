data {
  int<lower=1> N;
  int<lower=1, upper=2> K;
  vector[N] H;
  int<lower=1, upper=2> S[N];
}

parameters {
 vector[K] alpha;
 real<lower=0> sigma;
}

model {
 vector[N] mu;
 alpha ~ normal(0, 0.5);
 sigma ~ exponential(1);
 for( i in 1:N) {
   mu[i] = alpha[S[i]];
 }
  H ~ normal(mu, sigma);
}
