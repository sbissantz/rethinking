#
# Ch.12: Monsters and Mixtures
#
library(rethinking)
#
# Data
#
data(UCBadmit)
d <- UCBadmit
#
# Data Wrangling
d$gid <- ifelse(d$applicant.gender=="male",1L, 2L)
#
# Data list 
dat <- list(A=d$admit, N=d$applications, gid=d$gid)
#
# Fit the model
m12.1 <- ulam( 
              alist(
                    A ~ dbetabinom(N, pbar, theta),
                    logit(pbar) <- a[gid],
                    a[gid] ~ dnorm(0, 1.5),
                    transpars> theta <<- phi + 2.0,
                    phi ~ dexp(1)
              ), data=dat, chains=4)
# 
# Show stancode
stancode(m12.1)
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
#     real theta;
#     theta = phi + 2;
# }
# model{
#     vector[12] pbar;
#     phi ~ exponential( 1 );
#     a ~ normal( 0 , 1.5 );
#     for ( i in 1:12 ) {
#         pbar[i] = a[gid[i]];
#         pbar[i] = inv_logit(pbar[i]);
#     }
#     A ~ beta_binomial( N , pbar*theta , (1-pbar)*theta );
# }
# 

# Reduced data list
dat_ls <- list("N"=nrow(d),"A"=d$admit, "N_bern"=d$applications, "gid"=d$gid,
               "gno"=length(unique(dat$gid)))

#
# Fit the model
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "12", "1.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(data=dat_ls)


