lambda <- 2
n <- 10

N <- 100000

vec.estimate <- numeric(N)

for (i in 1:N ) {

	samples <- rexp(n,lambda)
	vec.estimate[i] <- mean(( samples - mean(samples) ) ^2 ) - (lambda^(-2) )
}
print(list(estimate=mean(vec.estimate),MSE=var(vec.estimate)/N ) )

#for 1% accuracy,  assume we want sqrt(MSE(estimate)) = 0.01 x estimate

# we know the estimate is -0.025 so we need to find N such that 
#MSE(estimate) = (0.0025 x 0.01)^2 =  6.25e-10

# WE already estimated the MSE for N=100000 above, so we use the relation:

# (100000 / N ) * MSE(100000) = MSE(estimate)=6.25e-10 so this gives

N.needed <- N*4.200028e-07/(6.25e-10)
