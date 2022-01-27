data { 
    int<lower=1> N;
    int<lower=1, upper=2> K;
    vector[N] W;
    int<lower=1, upper=K> S[N]; 
}
parameters {
   vector[K] alpha; 
   real<lower=0> sigma; 
}
model { 
    W ~ normal(alpha[S], sigma);
    alpha ~ normal(0,0.5);
    sigma ~ exponential(1);
}
generated quantities {
  // real W_tilde_1[N] = 
  //  normal_rng(alpha[S], sigma);
}
