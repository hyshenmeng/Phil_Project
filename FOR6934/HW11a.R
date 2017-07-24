set.seed(1111)
library(rjags)

#load data
hw11<-read.csv(file="F:/radon data gelman_hill.csv")
attach(hw11)
y=hw11$log.radon
x1=hw11$floor
x2=hw11$log.u
c.id=hw11$county
N=nrow(hw11)
K=length(unique(c.id))
dataList=list(y=y,x1=x1,x2=x2,c.id=c.id,N=N,K=K)

#specify model
modelString="
model{
for (i in 1:N){
media[i]<-b0[c.id[i]]+b1*x1[i]+b2*x2[i]
y[i]~dnorm(media[i],prec.sig2)
}
for (k in 1:K){
b0[k]~dnorm(gamma,prec.tau2)
}
b1~dnorm(0,1/100)
b2~dnorm(0,1/100)
gamma~dnorm(0,1/100)
prec.sig2~dgamma(0.1,0.1)
prec.tau2~dgamma(0.1,0.1)
}
"
writeLines(modelString, con="TEMPmodel.txt")

#generate chains
jagSModel=jags.model(file="TEMPmodel.txt",data=dataList,n.chain=4,n.adapt=100)
nchain=4

#burn-in period
update(jagSModel,n.iter=10000)

#generate posterior samples
jagsSamples=jags.samples(jagSModel,c('b0','b1','b2','gamma','prec.sig2','prec.tau2'),n.iter=10000)
niter=10000

#trace plot function
traceplot<-function(num,nome){
  k1=jagsSamples[[num]][]
  plot(NA,NA,xlim=c(0,niter),ylim=range(k1),main=nome)
  for (i in 1:nchain) lines(1:niter,jagsSamples[[num]][1,,i],col=i)
} 

#density plot function
densityplot<-function(x,nome){
  plot(density(x),type='l',xlab='',main=nome)
  x_ci=quantile(x,c(0.025,0.975))
  abline(v=x_ci,col='red',lty=3)
  x_mean=mean(x)
  abline(v=x_mean)
  print(c(x_mean,x_ci))
} 

#b1
traceplot(2,'b1')
densityplot(jagsSamples$b1,'b1')

#b2
traceplot(3,'b2')
densityplot(jagsSamples$b2,'b2')

#gamma
traceplot(4,'gamma')
densityplot(jagsSamples$gamma,'gamma')

#prec.sig2
traceplot(5,'prec.sig2')
densityplot(1/jagsSamples$prec.sig2,'sig2')

#prec.tau2
traceplot(6,'prec.tau2')
densityplot(1/jagsSamples$prec.tau2,'tau2')

detach(hw11)
