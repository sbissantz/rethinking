data {
 int<lower=0> N;
 int<lower=0> K;
 array[N] int career;
 array[K] int income;
}
parameters {
  vector[K-1] alpha;
  real beta;
}
transformed parameters {
  //vector[K] p_soft;
  //p_soft[1] = alpha[1] + beta*income[1];
  //p_soft[2] = alpha[2] + beta*income[2];
  //p_soft[3] = 0; //pivot
  vector[K] p;
  vector[K] s;
  s[1] = alpha[1] + beta*income[1];
  s[2] = alpha[2] + beta*income[2];
  s[3] = 0;
  p=softmax(s);
}
model {
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  //career ~ categorical_logit(p_soft);
  career ~ categorical(p);
}
