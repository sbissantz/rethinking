#
# Stan
#

y <- c(1,1,1)
y <- c(1,1,1,0)
y <- c(0,1,1,0,1,1)
y <- rbinom(100, 1, 0.73)
N <- length(y)

dat_list <- list(
                 y = y,
                 N = N
)
mdl.stan <- "
data{
    int<lower=0> N;
    int<lower=0,upper=1> y[N];
}
parameters{
    real<lower=0,upper=1> p;
}
model{
    y ~ bernoulli(p);
    p ~ uniform(0,1);
}
"
fit <- rstan::stan(model_code=mdl.stan, data=dat_list)
smpl <- rstan::extract(fit)
hist(smpl$p, xlab="Probability of Water", 
     ylab="number of ways the data can happen")


# Generative process
#
N <- 1e8
globe <- rbinom(N, 1, 0.5)
earth <- vector(length = N/2)
mars <- vector(length = N/2)
for(i in 1:N){
    if (globe[i] == 1){
        earth[i] <- rbinom(1, 1, 0.3)
    } else {
        mars[i] <- 1
    }
}
sum(earth, na.rm=TRUE) / length(earth[!is.na(earth)])



gen_process <- function(N){
    earth <- vector(length = N/2)
    mars <- vector(length = N/2)
    for(i in seq(N)){
        globe <- rbinom(1,1, 0.5) 
        if(globe==1){
            earth[i] <- rbinom(1, 1, 0.3)
        } else {
            mars[i] <- 1
        }
    }
    return(list(mars=mars,earth=earth))
}
gen_process(100)


#
# generative process
#
N <- 1e4
(card <- rbinom(N, 2, 1/3))
(ifelse(card==2, 1, rbinom(1, 1, 1/2)))

# Probability of Earth|Land
(Pr_EL <- (Pr_LE * Pr_E) / Pr_L)
# Probability of Water on Earth
Pr_LE <- 0.3 
# Probability of Earth
Pr_E <- 0.5
# Probability of L 
Pr_L <- 0.15 + 0.5


# water earth
#
sample_combi <- function(){
    earth <- sample(1:2, 1)
    if(earth==1){
        c(earth, rbinom(1, 1, .3))
    } else {
        c(earth, 1)
    }
}
N <- 1e5 
combis <- replicate(N, sample_combi())
planet <- combis[1,]
land <- combis[2,]
sum(planet == 1 & land==1) / sum(land==1)

# Black 1
#

sim_card <- function(){
    deck <- 1:3
    card <- sample(deck, 1)
    sides <- matrix(c(1,1,1,0,0,0),2,3)[,card]
    sample(sides)
}
N <- 1e4
cards <- replicate(N, sim_card())
up <- cards[1,]
down <- cards[2,]
sum(up == 1 & down == 1) / sum(up==1)

# Black 2
#
sim_card <- function(){
    deck <- 1:4
    card <- sample(deck, 1)
    sides <- matrix(c(1,1,1,0,0,0,1,1),2,4)[,card]
    sample(sides)
}
N <- 1e4
cards <- replicate(N, sim_card())
up <- cards[1,]
down <- cards[2,]
sum(up == 1 & down == 1) / sum(up==1)

# Black 3
#
sim_card <- function(){
    deck <- 1:6
    card <- sample(deck, 1)
    sides <- matrix(c(1,1,1,0,1,0,0,0,0,0,0,0),2,6)[,card]
    sample(sides)
}
N <- 1e4
cards <- replicate(N, sim_card())
up <- cards[1,]
down <- cards[2,]
sum(up == 1 & down == 1) / sum(up==1)

#
# Summarizing the Posterior
#

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Generative process
N <- 1e3
y <- rbinom(N, 1, prob=.73)

# data list
dat_ls <- list(
               y = y,
               N = N
               )

model.stan <- "
data{
    int<lower=0> N;
    int<lower=0, upper=1> y[N];
}
parameters{
    real<lower=0, upper=1> p;
}
model{
    y ~ bernoulli(p);
    p ~ normal(.6, 0.1);
}"
fit <- rstan::stan(model_code = model.stan, data = dat_ls)
print(fit)
summary(fit)
plot(fit)
pairs(fit)
rstan::traceplot(fit)

samples <- rstan::extract(fit)

#abline(v=mean(samples))                # Posterior mean
#abline(v=median(samples), lty=2)       # Posterior median 
#abline(v=mode(samples), lty=3)         # Posterior mode 

# Visualization
#
plot(density(samples$p), main=NA)
boundaries <- c(0.71, 0.73)
abline(v = boundaries)

# Intervals of defined boundariesj
#
N_samples <- length(samples$p)
sum(samples$p < 0.722)/N_samples
sum(samples$p > 0.71 & samples$p < 0.73 )/N_samples

# Intervals of defined mass
#
N_samples <- length(samples$p)
quantile( samples$p, 0.5 )

# Pereentile Interval
quantile( samples$p, c(0.7, 0.8) )

# Highest posterior denisty interval
#
# For S3 class objects corece
rethinking::HPDI( as.vector(samples$p), prob=.9 )







