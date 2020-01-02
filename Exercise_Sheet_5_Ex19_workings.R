####################################################################
#  Statistical Computing Exercise Sheet 5
#  Exercise 19 part b

require(ggplot2)

target.density <- function (x) {

	if ( abs(x) <= ( 3*pi ) ) return( 0.3293733954 * ( sin(x)^2 ) / ( x^2 ) ) else return(0)
}

generate.proposal <- function(x,sig)  {

	return ( rnorm(1,x,sig ) )

}

N <- 1000000
X <- numeric(N)
X[1] <- -1
sig <- 6

for (i in 1:( N-1) ) {

	# generate proposal
	prop <- generate.proposal(X[i],sig)

	# calculate acceptance prob
	acc.prob <- min(target.density(prop) / target.density(X[i]) , 1)

	U <- runif(1)

	if  ( U <= acc.prob ) X[i+1] <- prop else X[i+1] <- X[i]

#	print(paste("i=",i, "  Proposal:", round(prop,4), 
#		"  alpha:", round(alpha,4), "  U:", round(U,4), "  X[i+1]:", round(X[i+1],4) ) )
}

mean(X)
mean(X^2)
mean(X^3)
mean(X^4)
sum( abs(X) <= 3 ) / N

hist(X,breaks=30)
plot(X,type="b")

dt <- data.frame(X)
dt$iteration <- rownames(dt)
ggplot(data=dt,aes(x=iteration,y=X)) + geom_point() + geom_line()

XX.X <- numeric(1000)
XX <- runif(1000,-3*pi,3*pi)
for (i in 1:1000) {
	XX.X[i] <- target.density(XX[i])
}




MHsimple <- function (X,acc) {

burnin = 0 # number of burn-in iterations

lag = 1 # iterations between successive samples


% storage
X = zeros(nsamp,1) # samples drawn from the Markov chain
acc = [0 0]  #vector to track the acceptance rate
# MH routine


for (i in 1:burnin) {
#	[x,a] = MHstep(x,sig); # iterate chain one time step
#	 acc = acc + [a 1]; # track accept-reject status
}

x = 0.5  # start point
sig = 1 # standard deviation of Gaussian proposal
nsamp = 50000  #number of samples to draw

X <- numeric(nsamp)

for (i in 1:nsamp) {
	for (j in 1:lag) {
		x <- MHstep(x,sig)[1]
#		[x,a] = MHstep(x,sig) # iterate chain one time step
#		acc = acc + [a 1] # track accept-reject status
	}
	X[i] = x  # store the i-th sample
}

hist(X,breaks=20)


MHstep <- function (x0,sig)  {

	xp = rnorm(1,x0,sig) # generate candidate from Gaussian
	accprob = targetdist(xp) / targetdist(x0) # acceptance probability
	u = runif(1) # uniform random number
	if (u <= accprob) { # if accepted 
		x1 = xp # new point is the candidate
		a = 1 # note the acceptance
	} else {  # if rejected

		x1 = x0 # new point is the same as the old one
		a = 0 # note the rejection
	}

	return(c(x1,a))
}

targetdist <- function (x) {
#	return (exp(-(x^2)) * (2 + sin(x*5) + sin(x*2)) )

	if ( abs(x) <= ( 3*pi ) ) return( ( sin(x)^2 ) / ( x^2 ) ) else return(0)

}

 
XX <- runif(100000,-3,3)
XX.new <- exp(-(XX^2)) * (2 + sin(XX*5) + sin(XX*2))
plot(density(XX.new))

