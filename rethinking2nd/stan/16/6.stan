data{
     int<lower=0> N;
     array[N] int n; //outcome
     vector[N] age;
     vector[N] seconds;
}
parameters{
     real<lower=0> phi;
     real<lower=0> k;
     real<lower=0> theta;
}
model{
     vector[N] lambda;
    theta ~ lognormal( log(5) , 0.25 );
    k ~ lognormal( log(2) , 0.25 );
    phi ~ lognormal( log(1) , 0.1 );
    for ( i in 1:84 ) {
        lambda[i] = seconds[i] * phi * (1 - exp(-k * age[i]))^theta;
    }
    n ~ poisson( lambda );
}
