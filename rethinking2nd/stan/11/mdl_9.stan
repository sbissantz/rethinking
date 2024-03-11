data {
  int<lower=0> N;
  array[N] int t;
  array[N] int M;
  array[N] int y;
}

transformed data {
  array[N] real log_t = log(t);
}

parameters {
 real beta_M;
 real alpha;
} 

transformed parameters {
  vector[N] log_mu;
  for( i in 1:N ) {
    // Add the logarithm of the exposre (offset)
    log_mu[i] = log_t[i] + alpha + beta_M * M[i];
  }
}

model {
  y ~ poisson_log(log_mu);
  alpha ~ std_normal();
  beta_M ~ std_normal();
}
