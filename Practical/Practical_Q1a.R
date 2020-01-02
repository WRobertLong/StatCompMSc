Q1.a <- function(N, c) {

	validate.pars(N,c)

	X <- rnorm(N,0,1)
	Y <- rnorm(N,0,1)

	Z <- X * Y

	foo <- Z >= c & X >=0

	return(list(estimate=mean(foo), MSE=var(foo)/N))
}
