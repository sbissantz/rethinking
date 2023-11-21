data{
    int N;
    array[10] int T;
    array[10] int society;
    array[10] int P;
     matrix[10,10] Dmat;
}
transformed data {
  matrix[N, N] K;
  vector[N] mu = rep_vector(0, N);
  for (i in 1:(N - 1)) {
    K[i, i] = 1 + 0.1;
    for (j in (i + 1):N) {
      K[i, j] = exp(-0.5 * square(Dmat[i,j]));
      K[j, i] = K[i, j];
    }
  K[N, N] = 1 + 0.1;
 }
}
parameters{
     vector[10] k;
     real<lower=0> g;
     real<lower=0> b;
     real<lower=0> a;
}
model{
     vector[10] lambda;
     matrix[10,10] SIGMA;
    a ~ exponential( 1 );
    b ~ exponential( 1 );
    g ~ exponential( 1 );
    k ~ multi_normal( rep_vector(0,10) , K );
    for ( i in 1:10 ) {
        lambda[i] = (a * P[i]^b/g) * exp(k[society[i]]);
    }
    T ~ poisson( lambda );
}
