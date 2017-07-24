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
hw11$denis=1
nk=aggregate(denis~county,data=hw11,sum)$denis

#prior parameters
a.sig2=b.sig2=a.tau2=b.tau2=0.1

#initial values
b0=rep(mean(y),K)
b1=b2=0
gamma=mean(y)
sig2=1
tau2=1
param=list(b0=b0,b1=b1,b2=b2,gamma=gamma,sig2=sig2,tau2=tau2,c.id=c.id)

#sample b0
samp.b0=function(param){
  prec0=(nk/param$sig2)+(1/param$tau2)
  var0=1/prec0
  tmp0a=data.frame(c.id=c.id,err0=y-param$b1*x1-param$b2*x2)
  tmp0b=aggregate(err0~c.id,data=tmp0a,sum)$err0
  pmedia0=(tmp0b/param$sig2)+(param$gamma/param$tau2)
  rnorm(K,mean=var0*pmedia0,sd=sqrt(var0))
}

#sample b1
samp.b1=function(param){
  prec1=(sum(x1^2)/param$sig2)+(1/100)
  var1=1/prec1
  pmedia1=sum((y-param$b0[c.id]-param$b2*x2)*x1)/param$sig2
  rnorm(1,mean=var1*pmedia1,sd=sqrt(var1))
}

#sample b2
samp.b2=function(param){
  prec2=(sum(x2^2)/param$sig2)+(1/100)
  var2=1/prec2
  pmedia2=sum((y-param$b0[c.id]-param$b1*x1)*x2)/param$sig2
  rnorm(1,mean=var2*pmedia2,sd=sqrt(var2))
}

#sample sig2
samp.sig2=function(param){
  a0=(N+2*a.sig2)/2
  err=(y-param$b0[c.id]-param$b1*x1-param$b2*x2)^2
  b0=b.sig2+sum(err)/2
  1/rgamma(1,a0,b0)
}

#sample tau2
samp.tau2=function(param){
  a1=(K+2*a.tau2)/2
  b1=(sum((param$b0-param$gamma)^2)/2)+b.tau2
  1/rgamma(1,a1,b1)
}

#sample gamma
samp.gamma=function(param){
  prec3=(K/param$tau2)+(1/100)
  var3=1/prec3
  pmedia3=sum(param$b0)/param$tau2
  rnorm(1,mean=var3*pmedia3,sd=sqrt(var3))
}

#gibbs sampler
ngibbs=10000
store.b0=matrix(NA,ngibbs,K)
store.others=matrix(NA,ngibbs,5)
for (i in 1:ngibbs){
  param$b0=samp.b0(param)
  param$b1=samp.b1(param)
  param$b2=samp.b2(param)
  param$gamma=samp.gamma(param)
  param$sig2=samp.sig2(param)
  param$tau2=samp.tau2(param)
  store.b0[i,]=param$b0
  store.others[i,]=c(param$b1,param$b2,param$gamma,param$sig2,param$tau2)
}

#trace plot function
traceplot<-function(x,nome){
  plot(NA,NA,xlim=c(0,ngibbs),ylim=range(x),main=nome)
  lines(1:ngibbs,x)
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
traceplot(store.others[,1],'b1')
densityplot(store.others[,1],'b1')

#b2
traceplot(store.others[,2],'b2')
densityplot(store.others[,2],'b2')

#gamma
traceplot(store.others[,3],'gamma')
densityplot(store.others[,3],'gamma')

#prec.sig2
traceplot(1/store.others[,4],'prec.sig2')
densityplot(store.others[,4],'sig2')

#prec.tau2
traceplot(1/store.others[,5],'prec.tau2')
densityplot(store.others[,5],'tau2')

detach(hw11)
