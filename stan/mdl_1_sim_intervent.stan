data { 
    int<lower=1> N;
    int<lower=1, upper=2> K;
    vector[N] W;
    // vector[N] X;
    int<lower=1, upper=K> S[N]; 
}
parameters {
   vector[K] alpha_S; 
   real<lower=0> sigma; 
}
model { 
    W ~ normal(alpha_S[S], sigma);
    alpha_S ~ normal(0,0.5);
    sigma ~ exponential(1);
}
generated quantities {
  // real W_tilde_1[N] = 
  //  normal_rng(alpha[S], sigma);
  // real beta = 0 ; 
  // real W_tilde_1[N] = 
  //   normal_rng(X*beta + alpha[1], sigma);
  // real W_tilde_2[N] = 
  //   normal_rng(X*beta + alpha[2], sigma);
}
