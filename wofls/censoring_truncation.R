#
# Survival analysis
#
library(rethinking)
data(AustinCats)
d <- AustinCats

# Goal: Adoption rates between black & non-blakc cats
# H: Black cats are adopted at lower rates 
#
# Crux: Pr(Adopted) + Censoring-Pr(!Adopted)
# Pr of being adopted in time interval x ("normal probability")
# + Pr of waiting time span x and not being adopted! ("censoring probability")

# Model sketch
#
# D_i = 1 ~ Exponential(lambda_i)
# D_i = 0 ~ Exponential-CCDF(lambda_i)
# lamdbda_i = 1/mu_i
# mu_i = alpha_CID[i]

# Data list 
#
CID <- ifelse(d$color=="Black", 1, 2)
A <- ifelse(d$out_event=="Adoption", 1, 0)
dat_ls <- list("N"=nrow(d), "cno"=length(unique(CID)), "D"=d$days_to_event,
               "A"=A, "CID"= CID)


# Fit with ulam
#
m11.14 <- ulam(alist(
               D|A==1 ~ exponential(lambda),
               D|A==0 ~ custom(exponential_lccdf(!Y | lambda)),
               lambda <- 1.0/mu,
               log(mu) <- a[CID],
               a[CID] ~ normal(0,1)
), data=dat_ls, chains=4, cores=4)
stancode(m11.14)

# Fit with Stan
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_13a.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Diagnostics
#
fit$cmdstan_diagnose()
fit$summary()

# Stan user guide version of the model
# integrating out the censored values
#
CID <- ifelse(d$color=="Black", 1, 2)
CID_obs <- CID[A==1]
 <- ifelse(d$out_event=="Adoption", 1, 0)
N_obs <- length(d$days_to_event[A==1])
N_cens <- nrow(d) - N_obs
D_obs <- d$days_to_event[A==1]
cno <- length(unique(CID))
U <- max(D_obs)
dat_ls <- list("N_obs"=N_obs, "N_cens"=N_cens, "cno"=cno, "U"=U, "D_obs"=D_obs,
               "CID_obs" = CID_obs)

# Fit the model
#
path <- "~/projects/stanmisc"
file <- file.path(path, "stan", "11", "mdl_13b.stan") 
mdl <- cmdstanr::cmdstan_model(file, pedantic=TRUE)
fit <- mdl$sample(dat=dat_ls)

# Check the model
#
fit$cmdstan_diagnose()
fit$print()

# Note: did receive different values!
# If you should ever do a survival analysis, make sure, you understand the
# deviations before making inferences from your models.
