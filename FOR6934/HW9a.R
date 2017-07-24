set.seed(1111)
library(rjags)

#load data
hw9<-read.csv(file.choose())
attach(hw9)
y=log(N.species)
x1=log(area)
x2=Latit
x3=Latit^2
n=length(Island)
dataList=list(y=y,x1=x1,x2=x2,x3=x3)
initsList=list(b0=mean(y),b1=0,b2=0,b3=0,prec=10)

#specify model
modelString="
model{
for (i in 1:141){
media[i]<-b0+b1*x1[i]+b2*x2[i]+b3*x3[i]
y[i]~dnorm(media[i],prec)
}
b0~dnorm(0,1/100)
b1~dnorm(0,1/100)
b2~dnorm(0,1/100)
b3~dnorm(0,1/100)
prec~dgamma(1,1)
}
"
writeLines(modelString, con="TEMPmodel.txt")

#generate chains
jagSModel=jags.model(file="TEMPmodel.txt",data=dataList,inits=initsList,n.chain=4,n.adapt=100)
nchain=4

#burn-in period
update(jagSModel,n.iter=1000)

#generate posterior samples
jagsSamples=jags.samples(jagSModel,c('b0','b1','b2','b3','prec'),n.iter=1000)
niter=1000

#trace plot for b0
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$b0))
for (i in 1:nchain) lines(1:niter,jagsSamples$b0[1,,i],col=i)

#trace plot for b1
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$b1))
for (i in 1:nchain) lines(1:niter,jagsSamples$b1[1,,i],col=i)

#trace plot for b2
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$b2))
for (i in 1:nchain) lines(1:niter,jagsSamples$b2[1,,i],col=i)

#trace plot for b3
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$b3))
for (i in 1:nchain) lines(1:niter,jagsSamples$b3[1,,i],col=i)

#trace plot for sig2
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(1/jagsSamples$prec))
for (i in 1:nchain) lines(1:niter,1/jagsSamples$prec[1,,i],col=i)

#density plot function
densityplot<-function(x){
  plot(density(x),type='l',xlab='',main='')
  x_ci=quantile(x,c(0.025,0.975))
  abline(v=x_ci,col='red',lty=3)
  x_mean=mean(x)
  abline(v=x_mean)
  print(c(x_mean,x_ci))
} 

#density plot for b0
densityplot(jagsSamples$b0)

#density plot for b1
densityplot(jagsSamples$b1)

#density plot for b2
densityplot(jagsSamples$b2)

#density plot for b3
densityplot(jagsSamples$b3)

#density plot for sig2
densityplot(1/jagsSamples$prec)

