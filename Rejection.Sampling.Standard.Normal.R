
N <- 3000
lambda <- 1

X <- vector(length=N, mode = "numeric")
U <- vector(length=N, mode = "numeric")

#proposal density vector
X <- rexp(N,lambda)

U <- runif(N,0,1)

#accept/reject criterion
a.r <- exp(-((X-lambda)^2)/2)

# The accepted samples (these are half normal)
samp <- X[U <= a.r]

# convert to full-normal
V <- runif(N,0,1)
s1 <- samp * ifelse(V<0.5, 1, -1)

hist(s1,freq=F)
shapiro.test(s1)

# overlay a plot of the actual density
xx <- seq(-4, 4,length=100)
lines(xx,dnorm(xx,0,1))


#####################
#
# Code generates N(0,sigma) samples
# from exponential proposals using
# rejection sampling
#
#####################

sigma <- 10
N <- 3000

#parameter for the exponential distribution
lambda <- 1/sigma

#proposal density vector
X <- vector(length=N, mode = "numeric")
X <- rexp(N,lambda)

#uniform samples
U <- vector(length=N, mode = "numeric")
U <- runif(N,0,1)

#accept/reject criterion
a.r <- exp(-((X/sigma-1)^2)/2)

# The accepted samples (these are half normal)
samp <- X[U <= a.r]
hist(samp)

# convert to full-normal
V <- runif(length(samp),0,1)
out <- samp * ifelse(V<0.5, 1, -1)

#formal test of normality
shapiro.test(out)

x11()
# plot a histogram with overlaid normal density
hist(out,freq=F)
xx <- seq(-4*sigma, 4*sigma,length=100)
lines(xx,dnorm(xx,0,sigma))