####################################################################
#  Statistical Computing Exercise Sheet 5
#  Exercise 19 part b

target.density <- function (x) {

	if ( abs(x) <= ( 3*pi ) ) return( 0.3293733954 * ( sin(x)^2 ) / ( x^2 ) ) else return(0)
}

MH.Ex19 <- function(N, X0, sig) {

	X <- numeric(N) # vector to store the chain
	X[1] <- X0		# starting value

	for (i in 1:( N-1) ) {

		# generate proposal
		prop <- rnorm(1, X[i], sig )

		# calculate value for acceptance condition
		acc.val <- target.density(prop) / target.density(X[i])

		# apply the accept / reject condition
		if  ( runif(1) <= acc.val ) X[i+1] <- prop else X[i+1] <- X[i]
	}
	return (X)
}