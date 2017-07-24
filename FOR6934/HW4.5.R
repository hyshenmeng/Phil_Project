set.seed(1234)
#generate random beta from the normal distribution
mu1=0
sd1=10
n=10000
beta=rnorm(n,mu1,sd1)

#calculate pi from the random beta
pi=exp(beta)/(1+exp(beta))

#plot the distribution of pi
plot(density(pi),xlim=c(0,1),xlab=expression(pi),main='')
abline(v=mean(pi),col='red')