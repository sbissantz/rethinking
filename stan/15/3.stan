data {
    int N; 
    array[N] int H;
    vector[N] S;
}
parameters {
   real b_SH;
   real a;
}
model {
  vector[N] p;
  b_SH ~ normal(0, 0.5);
  a ~ normal(0, 1);
  p = a + b_SH * S;
  H ~ binomial_logit(10, p); //10 is the maximum number of trials
} 
