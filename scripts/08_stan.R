#
# categorical interatctions
#
options(mc.cores = parallel::detectCores())

# Data
#
library(rethinking)
data(rugged)
d <- rugged

# Tranformations 
#
d$log_gdp <- log(d$rgdppc_2000)
cond <- complete.cases(d$rgdppc_2000) 
dcc <- d[cond,]

# Rescaling
# 
# Mean scaler 
dcc$log_gdp_std <- with(dcc, log_gdp/mean(log_gdp))
# Maximum scaler 
dcc$rugged <- with(dcc, rugged/max(rugged)) 





