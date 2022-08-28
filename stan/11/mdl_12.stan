data{
  int<lower=0> N;
  array[N] int admit;
  array[N] int rejct;
}

parameters{
  real alpha_1;
  real alpha_2;
}

transformed parameters {
 real log_lambda_1 = alpha_1;
 real log_lambda_2 = alpha_2;
}

model{
  
  admit ~ poisson_log(log_lambda_1);
  rejct ~ poisson_log(log_lambda_2);
  alpha_1 ~ normal(0, 1.5);
  alpha_2 ~ normal(0, 1.5);
}




