Q1.b <- function(N, c, mu.X=0, mu.Y=0 ) {

	validate.pars(N,c)

	X.IS <- rnorm(N,mu.X,1)
	Y.IS <- rnorm(N,mu.Y,1)

	Z.IS <- X.IS * Y.IS

	bar <- dnorm(X.IS,0,1)*dnorm(Y.IS,0,1) / ( dnorm(X.IS,mu.X,1)*dnorm(Y.IS,mu.Y,1) )

	foo <- Z.IS >= c & X.IS >=0

	return(list(estimate=mean(foo*bar), MSE=var(foo*bar)/N))
}
