data {
 int<lower=0> N;
 int<lower=0> K;
 array[N] int career;
 array[K] int career_income;
}
parameters {
  vector[K-1] alpha;
  real<lower=0> beta;
}
transformed parameters {
  vector[K] soft_p;
  invsoft_p[1] = alpha[1] + beta*career_income[1];
  invsoft_p[2] = alpha[2] + beta*career_income[2];
  invsoft_p[3] = 0; //pivot
}
model {
  alpha ~ normal(0,1);
  beta ~ normal(0,0.5);
  career ~ categorical_logit(invsoft_p);
}
