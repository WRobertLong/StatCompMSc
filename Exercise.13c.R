#####################
#
#  Exercise 13 part c
#
#####################

N <- 1000
X <- rexp(N,1)

B <- X * exp( 4*sqrt(2) - (8/X) - X ) / sqrt(pi * X)

S <- 0

for ( i in 1:N) {

	S <- S + X[i]*exp( 4*sqrt(2) - (8/X[i]) - X[i] ) / sqrt(pi * X[i])
}


# Expectation
(E1 <- sum(B)/N)

# Variance
(V <- sum(B^2)/N - (sum(B)^2) /N )