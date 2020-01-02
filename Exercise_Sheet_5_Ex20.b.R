####################################################################
#  Statistical Computing Exercise Sheet 5
#  Exercise 20 part b

if (!exists("dtf") | !exists("sig.vec") source("Exercise_Sheet_5_Ex20.a.R")
if (!exists("MH.Ex19")) source("Exercise_Sheet_5_Ex19.R")
N <- 100000
for (sig in sig.vec) ) {

	X <- MH.Ex19(N, 0.5, sig)	
	error <- abs(0.01 * mean(X^4))
	MSE_100000 <- var(X^4) *(1/N) * (1 + 2*sum(dtf$ACF[dtf$sigma==sig]))
	print(paste("sigma=",sig, ", required N=", round(N * MSE_100000/error)))
}

