validate.pars <- function (N,c) {

	if (missing(N)) stop("Need to specify N.")
	if (missing(c)) stop("Need to specify c.")
	if (N<1) stop("N must be greater than 1.")
	if (c<0) stop("c must be greater than 0.")

}