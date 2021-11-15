
# 2M1 ---------------------------------------------------------------------

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

# 2M2 ---------------------------------------------------------------------




# 2M3 ---------------------------------------------------------------------

#
# Analytic solution
#

# Generative process
#
N <- 1e4
(card <- rbinom(N, 2, 1/3))
(ifelse(card==2, 1, rbinom(1, 1, 1/2)))

# Probability of Water on Earth
Pr_LE <- 0.3 
# Probability of Earth
Pr_E <- 0.5
# Probability of L 
Pr_L <- 0.15 + 0.5
# Probability of Earth|Land
(Pr_EL <- (Pr_LE * Pr_E) / Pr_L)

#
# Simulated version
#

# Generative process
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


# 2M4 ---------------------------------------------------------------------

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

# 2M5 ---------------------------------------------------------------------

# Generative process 
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

# 2M6 ---------------------------------------------------------------------

# Generative process 
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










