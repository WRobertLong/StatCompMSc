####################################################################
#  Statistical Computing Exercise Sheet 5
#  Exercise 20 part a

require(ggplot2)
if (!exists("MH.Ex19")) source("Exercise_Sheet_5_Ex19.R")

# number of lags
n.k <- 100

# vector of sigma values
sig.vec <- c(1,5,10)

# vector to store the autocorrelations
x.acf <- numeric(n.k * length(sig.vec))

# loop counter
i <- 0

for (sig in sig.vec )   {

	N <- 100000
	X <- MH.Ex19(N, 0.5, sig)

	# create sub chain, discarting the first 10% of the chain
	X  <- X[ (0.1*N) :N]
	N <- length(X)

	#loop to compute the autocorrelations
	for (k in 1:n.k) {
		x.acf[(i * n.k) + k] <-	cor(X[-(N:(N-k+1))],X[-(1:k)] )
	}
	# OK, we could have used the built-in acf() function, but
	# the above was more fun :-)  

	i <- i+1
}
dtf <- data.frame(ACF=x.acf, lag=rep(1:n.k,3), 
	sigma=c(rep(sig.vec[1],n.k),rep(sig.vec[2],n.k),rep(sig.vec[3],n.k)) )

ggplot(dtf,aes(x=lag,y=ACF,group=factor(sigma),colour=factor(sigma))) +
	geom_line() + scale_colour_discrete(name ="sigma")
# ggplot is also a bit more fun that the acf() plots
