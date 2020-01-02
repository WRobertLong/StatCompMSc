################
#
# Exercise 8
#
#

U <- runif(1000,0,1)
X <- 1/(U^(3/2))

hist(X, freq=F, breaks=seq(0, max(X)+1, 0.1),xlim=c(0,10), ylim=c(0,1) )

# now use the rule for the density of a function g of a random variable X:
# f.Y = f.X / | g'| where both function arguments are the inverse of g
# Here g'(x) = -3/(2x^(5/2)) and the inverse of g is 1/x^(2/3)

x <- seq(0, 10, 0.01)
#g <- (1/2)/(3/(2*(1/(x^(2/3))))^(5/2))
g <- ifelse(x <= 1, 0,(1/2)/(3/(2*(1/(x^(2/3))))^(5/2)))
lines(x, g, lw=1)