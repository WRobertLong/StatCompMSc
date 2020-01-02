
vs <- vector(mode="numeric")

#for (k in c(3,4,10,20,100,500)) {
for (k in c(3,10)) {

	vc <- vector(length=10,mode="numeric")

	for ( j in 1:100 ) {

		ls <- list()

#		the.one <- 1

		for (i in 1:1000) {

			M<-matrix(runif(3*k,0,1), nrow=k, ncol=3)
			cov.XY <- cov(M[,1],M[,2]) 
			cov.XZ <- cov(M[,1],M[,3]) 
			cov.YZ <- cov(M[,2],M[,3]) 

			if (cov.XY<0 & cov.XZ<0 & cov.YZ<0) {
#				ls[[the.one]] <- M
#				the.one <- the.one + 1
				ls <- append(ls,M)
			}
		}
		vc[j] <- length(ls)
	}
vs <- append(vs, mean(vc) )
}
