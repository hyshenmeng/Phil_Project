set.seed(1111)
library(rjags)

#load data
hw6<-read.table(file="F:/HW6.txt",header=TRUE)
x=hw6$x
dataList=list('x'=x)

#specify model
modelString_f="
model{
for (i in 1:3){
x[i]~dnorm(mu_f,prec)
}
for (i in 4:15){
x[i]~dnorm(mu_m,prec)
}
mu_f~dnorm(0,0.01)
mu_m~dnorm(0,0.01)
prec~dgamma(1,1)
}
"
writeLines(modelString_f, con="TEMPmodel.txt")

#generate chains
jagSModel=jags.model(file="TEMPmodel.txt",data=dataList,n.chain=4,n.adapt=100)
nchain=4

#burn-in period
update(jagSModel,n.iter=1000)

#geberate posterior samples
jagsSamples=jags.samples(jagSModel,c('mu_f','mu_m','prec'),n.iter=1000)
niter=1000

#trace plot for mu_f
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$mu_f))
for (i in 1:nchain) lines(1:niter,jagsSamples$mu_f[1,,i],col=i)

#trace plot for mu_m
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$mu_m))
for (i in 1:nchain) lines(1:niter,jagsSamples$mu_m[1,,i],col=i)

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

#density plot for mu_f
densityplot(jagsSamples$mu_f)

#density plot for mu_m
densityplot(jagsSamples$mu_m)

#density plot for prec
densityplot(jagsSamples$prec)

#density plot for sig2
densityplot(1/jagsSamples$prec)

#mu_f v.s. mu_m
diff=abs(jagsSamples$mu_f)-abs(jagsSamples$mu_m)
mean(diff<0)
