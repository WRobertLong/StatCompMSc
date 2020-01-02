####################################################################
#  Statistical Computing Exercise Sheet 5
#  Exercise 18 part b

Stationary.Distribution.MC <- function( M ) {

	# No need to check for square matrix - eigen() will do that.
	# We use eigen(t(P)) to compute the eigenvalues and eigenvectors
	# However, Some care is needed here, as this is a numerical 
	# operation  so to find the eigenvalue=1 is not as straight 
	# forward as eigen(t(P))[[1]] == 1 which will only work in trivial 
	# or simple situations due to numerical error. So we use round() 
	# to 4 decimal places

	location.of.eigenvalue.1 <- which(round(eigen(t(M))[[1]],4) == 1)

	if ( is.na(location.of.eigenvalue.1[1]) )
		stop("Unable to find an eigenvalue=1")

	# Can the matrix have more than 1 eigenvalue=1 ? I think not.
	# Now extract the eigenvector. Note that eigen() returns 
	# a list with the 2nd component a vector of the eigenvectors 
	# (ie a matrix)

	pi.sd <- (eigen(t(P))[[2]])[,location.of.eigenvalue.1[1]]

	for ( i in 1:length(pi.sd) ) {
		if ( Im(z <- zapsmall(pi.sd[i])) != 0 ) stop("complex eigenvector found.")
	}

	# get rid of those annoying zero imaginary parts
	storage.mode(pi.sd) <- "numeric"
	
	# Now scale the eigenvector so that it's elements sum to 1
	return ( pi.sd/sum(pi.sd) )
}