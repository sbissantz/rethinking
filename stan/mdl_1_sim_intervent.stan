data { 
    int<lower=1> N;
    int<lower=1, upper=2> K;
    vector[N] W;
    int S[N]; 
}
parameters {
   vector[K] alpha; 
   real<lower=0> sigma; 
}
model { 
    W ~ normal( alpha[S], sigma );
    alpha[S] ~ uniform(60,10) ;
    sigma ~ uniform(0,10);
}
generated quantities {
    real alpha_tilde[N] =
    normal_rng(alpha[S], sigma);
}
