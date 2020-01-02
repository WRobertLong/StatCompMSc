#####################################
##  
##  MATH5835M Statistical Computing
##         Practical
##
#####################################

setwd("C:/Users/rob/Documents/PhD/MATH5835M Statistical Computing/Practical")
#setwd("M:/MATH5835M - Statistical Computing/Practical")

# Question 1.a
source("validate.pars.R")
source("Practical_Q1a.R")

Q1.a(1000000,10)



n <- 6
vc <- vector(mode="numeric", length=n)

for ( i in 1:n) {

	vc[i] <- Q1.a(10^i,3)


}
plot(vc)



# Question 1.b
#
# Importance sampling

source("Practical_Q1b.R")

Q1.a(10000,10)
Q1.b(10000,10,10,10)


#  Question 1.c
#



*********************************

source("MC.MC.MSE.R")

MC.MC.MSE(100,10000,3)


# want N >= 1.96^2 * var(sample) / (error^2)
# where error = RMSE
# so

n.base <- 10000000
var.fx <- MC.MC.MSE(10,n.base,3) * n.base

N.MC <-  ( 1.96^2 ) * var.fx / ((0.01*Q1.a(n.base,3)$estimate)^2)
N.MC
submitted <- ( 1.96^2 ) * 0.0049 / (0.01*0.0049)^2
submitted





source("MC.IS.MSE.R")

n.base <- 500000

#mu.x=mu.y=mu for importance sampling
seq.length <- 30
mu.seq <- seq(1.7,2.1,length=seq.length)
N.vec <- numeric(seq.length)
i <- 1
for (mu in mu.seq) {
	var.fx <- MC.IS.MSE(20,n.base,3,mu,mu) * n.base

	N.IC <-  ( 1.96^2 ) * var.fx / ((0.01*Q1.b(n.base,3,mu,mu)$estimate)^2)
	N.vec[i] <-	N.IC
	print(i)
	i <- i+1
}


calc.samp.size.IS <- function ( c, offset ) {

	n.base <- 1000000

	#mu.x=mu.y= sqrt(c) for importance sampling
	mu = sqrt(c)+offset

	var.fx <- MC.IS.MSE(20,n.base,3,mu,mu) * n.base

	N.IC <-  ( 1.96^2 ) * var.fx / ((0.01*Q1.b(n.base,3,mu,mu)$estimate)^2)

	return( N.IC)		

}

n.use <- calc.samp.size.IS (10,-1.4)
Q1.b(n.use,0)


vec.c <-seq(1,10,by=1)
vec.best.offset <- numeric(length(vec.c))
j <- 1
for (c in vec.c ) {

	vec.offset <- seq(-2,1,length=30) 
	vec.N <- numeric(length(offset))
	i <- 1
	for ( offset in vec.offset) {

		n.base <- 100000

		#mu.x=mu.y= sqrt(c) for importance sampling
		mu = sqrt(c)+offset 

		var.fx <- MC.IS.MSE(20,n.base,3,mu,mu) * n.base

		N.IC <-  ( 1.96^2 ) * var.fx / ((0.01*Q1.b(n.base,3,mu,mu)$estimate)^2)

		vec.N[i] <- N.IC
		i = i + 1
	}

	vec.best.offset[j] <- vec.offset[which(vec.N ==min(vec.N))]
	print(j)
	j=j+1
}
vec.best.offset




# c=0, offset = 1.5
# c=0.5, offset = 1.3
# c=1, offset = 0.9
# c=1.5, offset = 0.7
# c=2, offset = 0.5
# c=2.5, offset = 0.33
# c=3, offset = 0.2




require(ggplot2)
require(splines)
x11()
ggplot(data.frame(mu = mu.seq,N=N.vec), aes(x=mu,y=N) ) +
	geom_point(shape=1) +
	geom_smooth(aes(), se=F,
                method="glm",
                formula=y~ns(x,4),
                family=gaussian(link="log"),
                show_guide = FALSE,lwd=0.7) 



MC.IS.MSE(100,10000,3,sqrt(3),sqrt(3))

MC.IS.MSE(100,10000,3,1,3)

MC.IS.MSE(100,10000,3,3,1)

MC.IS.MSE(100,10000,3,2,1.5)

MC.IS.MSE(100,10000,3,1.9,1.6)
MC.IS.MSE(100,10000,3,1.9,1.7)
MC.IS.MSE(100,10000,3,1.9,1.8)
MC.IS.MSE(100,10000,3,1.9,1.9)
MC.IS.MSE(100,10000,3,1.9,2.0)


require(rgl)



x <- seq(1.6, 2.3, length=15)
y <- seq(1.6, 2.3, length=15)


z <- matrix(nrow=length(x),ncol=length(y))
for (i in 1:length(x) ) {

	for (j in 1:length(y) ) {

		z[i,j] <- MC.IS.MSE(50,5000,3,x[i],y[j])

	}
	print(i)
	
} 

palette <- colorRampPalette(c("red", "yellow", "green", "blue")) 
col.table <- palette(256)
col.ind <- cut(z, 256)
open3d(windowRect=c(50,50,800,800))
persp3d(x, y, z, col=col.table[col.ind])



open3d(windowRect=c(50,50,800,800))x <- seq(-3, 3, length=50)
y <- seq(-3, 3, length=50)
z <- outer(x,y, function(x,y) dnorm(x, 0, 1)*dnorm(y, 0, 1))

palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(z, 256)
persp3d(x, y, z, col=col.table[col.ind])






#####################################################


#  Question 1.d
#

c.seq <- seq(5,7,length=20)
vec.p <- vector(mode="numeric", length=length(c.seq) )
vec.MSE <- vector(mode="numeric", length=length(c.seq) )


i <- 1

for ( c in c.seq ) {

	vec.p[i] <- Q1.b( 1000000, c, c, c)$estimate
	vec.MSE[i] <- Q1.b( 1000000, c, c, c)$MSE

	i <- i + 1

}
par(mfrow=c(2,1))
plot(c.seq, vec.p)
plot(c.seq, vec.MSE)




c.seq <- seq(0,10,length=20)
vec.p <- vector(mode="numeric", length=length(c.seq) )
vec.MSE <- vector(mode="numeric", length=length(c.seq) )

i <- 1
for ( c in c.seq ) {

	vec.p[i] <- Q1.a( 1000000, c)$estimate
	vec.MSE[i] <- Q1.a( 1000000, c)$MSE

	i <- i + 1

}
x11()
par(mfrow=c(2,1))
plot(c.seq, vec.p)
plot(c.seq, vec.MSE)





N <- 10000
X <- rnorm(N,1.7,1)
Y <- rnorm(N,1.7,1)

Z <- X * Y
mean(Z)


hist(Z)
hist(Z[X>0])

mean(Z[X>0])








