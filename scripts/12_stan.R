#
# Ch.12: Monsters and Mixtures
#

library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$gid <- ifelse(d$applicant.gender=="male",1L, 2L)
dat <- list(A=d$admit, N=d$applications, gid=d$gid)
m12.1 <- ulam( 
              alist(
                    A ~ dbetabinom(N, pbar, theta),
                    logit(pbar) <- a[gid],
                    a[gid] ~ dnorm(0, 1.5),
                    transpars> theta <<- phi + 2.0,
                    phi ~ dexp(1)
              ), data=dat, chains=4)

stancode(m12.1)
# Phi-Kappa Reparameterized Beta-Binomial
#
# data{
#     int N[12];
#     int A[12];
#     int gid[12];
# }
# parameters{
#     vector[2] a;
#     real<lower=0> phi;
# }
# transformed parameters{
#     vector[12] pbar;
#     for ( i in 1:12 ) {
#         pbar[i] = a[gid[i]];
#         pbar[i] = inv_logit(pbar[i]);
#     }
#     real theta = phi + 2;
#     real alpha = pbar*theta;
#     real beta = (1-pbar)*theta;
# }
# model{
#     phi ~ exponential( 1 );
#     a ~ normal( 0 , 1.5 );
#     A ~ beta_binomial( N , alpha, beta );
# }

# Phi-Kappa Reparameterized Beta-Binomial
# ..-consistent with the distribution explorer website
#
# data{
#     int N[12];
#     int A[12];
#     int gid[12];
# }
# parameters{
#     vector[2] a;
#     real<lower=0> phi;
# }
# transformed parameters{
#     vector[12] phi;
#     for ( i in 1:12 ) {
#         phi[i] = a[gid[i]];
#         phi[i] = inv_logit(phi[i]);
#     }
#     real kappa = phi + 2;
#     real alpha = phi*kappa;
#     real beta = (1-phi)*kappa;
# }
# model{
#     phi ~ exponential( 1 );
#     a ~ normal( 0 , 1.5 );
#     A ~ beta_binomial( N , alpha, beta );
# }
