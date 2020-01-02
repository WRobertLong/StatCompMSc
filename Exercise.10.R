#####################
#
# Fucntion generates N(0,sigma) samples
# from exponential proposals using
# rejection sampling
#
#####################

rnorm.rs <- function ( N , sigma ) {

	#proposal density vector
	X <- vector(length=N, mode = "numeric")
	X <- rexp(N,1/sigma)

	#uniform samples
	U <- vector(length=N, mode = "numeric")
	U <- runif(N,0,1)

	#accept/reject criterion
	a.r <- exp(-((X/sigma-1)^2)/2)

	# The accepted samples (these are half normal)
	samp <- X[U <= a.r]

	# convert to full-normal
	V <- runif(length(samp),0,1)
	return ( samp * ifelse(V<0.5, 1, -1) )

}

sigma.test <- 4
n.test <- 500
dt <- rnorm.rs ( n.test, sigma.test )

# plot a histogram with overlaid normal density
hist(dt,freq=F)
xx <- seq(-4*sigma.test , 4*sigma.test ,length=100)
lines(xx,dnorm(xx,0,sigma.test ))
