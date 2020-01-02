######################
#
# Exercise 12
#
######################
require(ggplot2)
require(reshape2)

X <- rnorm(1000,0,1)
Y <- (X^2-1)/2
dt <- melt(as.data.frame(cbind(X,Y)))

ggplot(dt, aes(x=value, fill=variable, colour=variable)) + 
	geom_histogram(aes(y = ..density..), 
		alpha=0.3,binwidth=.2, position="identity") +
	xlab("")


