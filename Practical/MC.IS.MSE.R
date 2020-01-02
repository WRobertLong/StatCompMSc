MC.IS.MSE <- function (n, N, c, mu.X, mu.Y) {

	vc <- vector (mode="numeric", length=n)

	for (i in 1:n)  {

		vc[i] <- Q1.b(N,c, mu.X, mu.Y)$MSE

	}
	return(mean(vc) )
}