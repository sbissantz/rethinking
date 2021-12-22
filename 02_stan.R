#
# First stan trials
#
# https://www.r-bloggers.com/2019/01/an-introduction-to-stan-with-r/

# Generate some data
#
N <- 100
pr_W <- 0.65 
y <- rbinom(N, 1, pr_W)

# Data list
#
dat_list <- list(
  y = y,
  N = N 
)

# Model & fit
#
mdl.stan <-
  "
data {
  int<lower=0> N;               // number of trials
  int<lower=0, upper=1> y[N];   // success on trial n
}
parameters {
  real<lower=0, upper=1> p; // chance of success
}
model {
  p ~ uniform(0, 1);        // prior
  y ~ bernoulli(p);         // likelihood
}
"
fit <- rstan::stan(model_code=mdl.stan, data=dat_list) 
print(fit)

# extract samples
#
smpl <- rstan::extract(fit)
hist(smpl$p)

# Generate some data
#
(y <- rbinom(5, 1, .65))
N <- length(y)

# Generate a data list
#
dat_list <- list(
  y = y, 
  N = N
)
mdl.stan <- "
data{
int<lower=0> N; // number of trials 
int<lower=0, upper=1> y[N]; //data vector 
}
parameters{
real<lower=0, upper=1> p; //parameter
}
model{
y ~ bernoulli(p); // likelihood
p ~ uniform(0,1); // prior
}
"
fit <- rstan::stan(model_code = mdl.stan, data = dat_list)
smpl <- rstan::extract(fit)
hist(smpl$p)

# PPD
#
hist(rnorm(1e5, mean = 0.6, sd = .1))

# Produce some data
#
y <- rbinom(10, 1, .65)
N <- length(y)

# Data List
#
dat_list <- list(
  y = y,
  N = N
)

# Model
#
mdl.stan <- "
data{
 int<lower=0, upper=10> N;
 int<lower=0, upper=1> y[N]; 
} 
parameters{
  real<lower=0, upper=1> p; 
 }
model{
  y ~ binomial(1,p);
  p ~ normal(0.6, 0.1);
}
"
(fit2 <- rstan::stan(model_code = mdl.stan, data = dat_list))
smpl2 <- rstan::extract(fit2)
hist(smpl2$p)


# Data generating process
#
y <- rbinom(10, 1, prob = .73)

# data list
#
dat_list <- list(
  y = y,
  N = length(y)
)

# model
#
mdl.stan <- "
data{
int<lower=0> N; 
int<lower=0, upper=1> y[N];
}
parameters{
real<lower=0, upper=1> p;
}
model{
y ~ bernoulli(p); 
if (p < 0.5)  
  p ~ uniform(0,0.5);
else 
  p ~ uniform(0.5,1);
}
"
fit <- rstan::stan(model_code=mdl.stan, data=dat_list)

# inspect the model

#
# Sim cards
# 

sim_cards <- function() {
  deck <- 1:3
  card <- sample(deck, 1)
  sides <- matrix(c(1,1,1,0,0,0),2,3)[,card]
  sides
  sample(sides) 
}

#
# generative process
# E.g.: globe 
#

#
# generative process
# E.g.: card deck
#

sim_card <- function(){
  deck <- 1:3
  card <- sample(deck, 1)
  sides <- matrix(c(1,1,1,0,0,0),2,3)[,card]
  sample(sides)
}
sim_card()
N <- 1e5
cards <- replicate(N, sim_card())
up <- cards[1,]
down <- cards[2,]
sum(up==1 & down==1) / sum(up==1)

#
# generative process 
# 
#
sim_pancake <- function(){
  pancake <- sample(1:3, 1)
  # 2x3 matrix
  # matrix(c(1,1,1,0,0,0),2,3)
  sides <- matrix(c(1,1,1,0,0,0),2,3)[,pancake]
  sample(sides)
}
N <- 1e5
pancakes <- replicate(N, sim_pancake())
up <- pancakes[1,]
down <- pancakes[2,]
sum(up==1 & down==1) / sum(up==1)






