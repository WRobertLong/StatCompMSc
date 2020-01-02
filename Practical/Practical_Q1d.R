require(ggplot2)

seq.c <- seq(0,10, by=0.1)
vec.pr <- numeric(length(seq.c))
i <- 1

for (c in seq.c ) {
	vec.pr[i] <- Q1.b(1000000,c,sqrt(c),sqrt(c))$estimate
	print(i)
	i <- i+1
}
ggplot(data.frame(c = seq.c,p3=vec.pr), aes(x=c,y=p3) ) +
	geom_point(shape=1) +
	geom_line(color="red")



	