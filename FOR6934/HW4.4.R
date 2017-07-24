set.seed(1111)
#generate normal random data
mu1=10
sd1=10
n=1000
r=rnorm(n,mu1,sd1)
rmean=mean(r)
rvar=var(r)
rsum=sum((r-mu1)^2)

#prior parameters
a1=0.1
b1=0.1

#posterior parameters
a2=a1+n/2
b2=b1+rsum/2
a2
b2

#generate random precisions from the posterior distribution
g=rgamma(1000,a2,b2)

#calculate variances from the random precisions
ig=1/g

#plot the distribution of variances
plot(density(ig),main='variance')
abline(v=mean(ig),col='red')
abline(v=quantile(ig,c(0.025,0.975)),col='blue')
mean(ig)
quantile(ig,c(0.025,0.975))

#analytical values for inverse gamma distribution
igmean=b2/(a2-1)
ivar=(b2^2)/(((a2-1)^2)*(a2-2))
igCI=c(igmean-1.96*sqrt(ivar),igmean+1.96*sqrt(ivar))
igmean
igCI
