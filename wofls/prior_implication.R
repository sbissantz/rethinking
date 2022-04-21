# 
# Prior implications
#

# Before setting up a joint prior model for the analysis, I inspect the prior
# for each parameter independently. 

# alpha prior
curve(dnorm(x, 178, 20), from=100, to=250)
# standardized: 0

# beta prior
curve(dnorm(x, 0, 10), from=-50, to=50)
curve(dnorm(x, 0, 0.5), from=-50, to=50)  # standardized 
curve(dlnorm(x, 0, 1), from=0, to=10, xlab="weight", ylab="height")

# sigma prior
curve(dexp(x, 1), from=-1, to=5)
curve(dunif(x, 0, 50), from=-10, to=60)

#
# Additional: 95% interval 
#

curve(dlnorm(x, 0, 1), from=0, to=10, xlab="weight", ylab="height")
# Quantile values for the 95% percentile interval
qv <- qlnorm(c(.05, .50, .95), meanlog=0, sdlog=1)
abline(v=qv, lty=2)
