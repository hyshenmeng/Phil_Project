set.seed(1111)
library(rjags)

#load data
hw6<-read.table(file="F:/HW6.txt",header=TRUE)
x=hw6$x
n=length(x)
dataList=list('x'=x,'n'=n)

#specify model
modelString="
model{
for (i in 1:n){
x[i]~dnorm(mu,prec)
}
mu~dnorm(-5,1/6.25)
prec~dgamma(1,1)
}
"
writeLines(modelString, con="TEMPmodel.txt")

#generate chains
jagSModel=jags.model(file="TEMPmodel.txt",data=dataList,n.chain=4,n.adapt=100)
nchain=4

#burn-in period
update(jagSModel,n.iter=1000)

#geberate posterior samples
jagsSamples=jags.samples(jagSModel,c('mu','prec'),n.iter=1000)
niter=1000

#trace plot for mu
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$mu))
for (i in 1:nchain) lines(1:niter,jagsSamples$mu[1,,i],col=i)

#trace plot for prec
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$prec))
for (i in 1:nchain) lines(1:niter,jagsSamples$prec[1,,i],col=i)

#trace plot for sig2
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(1/jagsSamples$prec))
for (i in 1:nchain) lines(1:niter,1/jagsSamples$prec[1,,i],col=i)

#density plot function
densityplot<-function(x){
  plot(density(x),type='l',xlab='',main='')
  x_ci=quantile(x,c(0.025,0.975))
  print(x_ci)
  abline(v=x_ci,col='red',lty=3)
  x_mean=mean(x)
  print(x_mean)
  x_var=var(x)
  print(x_var)
  abline(v=x_mean)
} 

#density plot for mu
densityplot(jagsSamples$mu)
y=seq(from=-15,to=5,length.out=100)
lines(y,dnorm(y,mean=-5,sd=2.5),col='blue',lwd=3)
abline(v=-5)

#density plot for prec
densityplot(jagsSamples$prec)

#density plot for sig2
densityplot(1/jagsSamples$prec)

