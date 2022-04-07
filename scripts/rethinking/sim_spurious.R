#
#
# Simulating spurious associations
#
#

N <- 100                               # number of cases
x_real <- rnorm( N )                   # x_real as Gaussian with mean 0 and stddev 1
x_spur <- rnorm( N , x_real )          # x_spur as Gaussian with mean=x_real
y <- rnorm( N , x_real )               # y as Gaussian with mean=x_real
d <- data.frame(y,x_real,x_spur)       # bind all together in data frame

pairs(d)

f1 <- alist( y ~ dnorm( mu, sigma ),
             mu <- a + br * x_real,
             a ~ dnorm( 0, 0.1 ),
             br ~ dnorm( 0, 0.5 ),
             sigma ~ dexp( 1 )
)
m1 <- quap( f1, data=d )

f2 <- alist( y ~ dnorm( mu, sigma ),
             mu <- a + bs * x_spur,
             a ~ dnorm( 0, 0.1 ),
             bs ~ dnorm( 0, 0.5 ),
             sigma ~ dexp( 1 )
)
m2 <- quap( f2, data=d )

f3 <- alist( y ~ dnorm( mu, sigma ),
             mu <- a + br * x_real + bs* x_spur,
             a ~ dnorm( 0, 0.1 ),
             br ~ dnorm( 0, 0.5 ),
             bs ~ dnorm( 0, 0.5 ),
             sigma ~ dexp( 1 )
)
m3 <- quap( f3, data=d )

plot( coeftab( m1, m2, m3 ), par=c("br", "bs") )
