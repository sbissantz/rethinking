#
#
# Simulating the masking relationship
#
#

#
# DAG 1
#

# Draw: DAG
dag <- dagitty('dag {
    N [pos="0,0"]
    M [pos="2,0"]
    K [pos="1,1"]
    M -> K <- N
    M -> N
}')
drawdag( dag )

# Simulate: Fake Data
n <- 100
M <- rnorm( n )
N <- rnorm( n , M )
K <- rnorm( n , N - M )
d_sim <- data.frame(K=K,N=N,M=M)

# Formalize: model
f <- alist( 
           K ~ dnorm( mu, sigma ),
           mu <- a + bN * N,
            a ~ dnorm( 0,1 ),
            bN ~ dnorm( 0, 0.5 ),
           sigma <- dexp(1)
)
# Fit: mdl to dta
m <- quap( f, data = d_sim ) 

#
# DAG  2
# 

dag2 <- dagitty('dag {
    N [pos="0,0"]
    M [pos="2,0"]
    K [pos="1,1"]
    M -> K <- N
    M -> N
}')
drawdag( dag2 )

# Simulate: fake data
n <- 100
M <- rnorm( n )
N <- rnorm( n , M )
K <- rnorm( n , N - M )
d_sim2 <- data.frame(K=K,N=N,M=M)

# Formalize: model
f2 <- alist( 
           K ~ dnorm( mu, sigma ),
           mu <- a + bM * M,
            a ~ dnorm( 0,1 ),
            bM ~ dnorm( 0, 0.5 ),
           sigma <- dexp(1)
)
# Fit: mdl to dta
m2 <- quap( f2, data = d_sim2 ) 

#
# DAG 3
#

dag3 <- dagitty('dag {
    N [pos="0,0"]
    U [pos="1,0"]
    M [pos="2,0"]
    K [pos="1,1"]
    M <- U -> N
    M -> K <- N
}')
drawdag( dag3 )

# Simulate: fake data
n <- 100
U <- rnorm( n )
N <- rnorm( n , U )
M <- rnorm( n , U )
K <- rnorm( n , N - M )
d_sim3<- data.frame(K=K,N=N,M=M)

# Formalize: model
f3 <- alist( 
           K ~ dnorm( mu, sigma ),
           mu <- a + bM * M + bN * N,
            a ~ dnorm( 0,1 ),
            bM ~ dnorm( 0, 0.5 ),
            bN ~ dnorm( 0, 0.5 ),
           sigma <- dexp(1)
)
# Fit: mdl to dta
m3 <- quap( f3, data = d_sim3 ) 

#
# Summarize 
# 

# Markov equivalent DAGs?

par( mfrow=c(3,2) )
MElist <- equivalentDAGs( dag2 )
lapply( MElist, plot )

# Coeftab: Masking effect?

plot( coeftab( m, m2, m3 ), pars=c("bN","bM") )
