MC.MC.MSE <- function (n, N, c) {

	vc <- vector (mode="numeric", length=n)

	for (i in 1:n)  {
		vc[i] <- Q1.a(N,c)$MSE
	}
	
	return(mean(vc) )
}