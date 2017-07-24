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

#prior parameters
v=100
a=b=1

#initial values
b0=mean(y)
b1=b2=b3=0
sig2=0.1
param=list(b0=b0,b1=b1,b2=b2,b3=b3,sig2=sig2)

#sample b0
samp.b0=function(param){
  prec0=(n/param$sig2)+(1/v)
  var0=1/prec0
  pmedia0=sum(y-param$b1*x1-param$b2*x2-param$b3*x3)/param$sig2
  rnorm(1,mean=var0*pmedia0,sd=sqrt(var0))
}

#sample b1
samp.b1=function(param){
  prec1=(sum(x1^2)/param$sig2)+(1/v)
  var1=1/prec1
  pmedia1=sum((y-param$b0-param$b2*x2-param$b3*x3)*x1)/param$sig2
  rnorm(1,mean=var1*pmedia1,sd=sqrt(var1))
}

#sample b2
samp.b2=function(param){
  prec2=(sum(x2^2)/param$sig2)+(1/v)
  var2=1/prec2
  pmedia2=sum((y-param$b0-param$b1*x1-param$b3*x3)*x2)/param$sig2
  rnorm(1,mean=var2*pmedia2,sd=sqrt(var2))
}

#sample b3
samp.b3=function(param){
  prec3=(sum(x3^2)/param$sig2)+(1/v)
  var3=1/prec3
  pmedia3=sum((y-param$b0-param$b1*x1-param$b2*x2)*x3)/param$sig2
  rnorm(1,mean=var3*pmedia3,sd=sqrt(var3))
}

#sample sig2
samp.sig2=function(param){
  a.sig2=(n+2*a)/2
  err2=(y-param$b0-param$b1*x1-param$b2*x2-param$b3*x3)^2
  b.sig2=b+sum(err2)/2
  1/rgamma(1,a.sig2,b.sig2)
}

#gibbs sampler
ngibbs=10000
store.param=matrix(NA,ngibbs,5)
for (i in 1:ngibbs){
  param$b0=samp.b0(param)
  param$b1=samp.b1(param)
  param$b2=samp.b2(param)
  param$b3=samp.b3(param)
  param$sig2=samp.sig2(param)
  store.param[i,]=c(param$b0,param$b1,param$b2,param$b3,param$sig2)
}

#trace plot function
traceplot<-function(x){
  plot(NA,NA,xlim=c(0,ngibbs),ylim=range(x))
  lines(1:ngibbs,x)
}

#trace plot for b0
traceplot(store.param[,1])

#trace plot for b1
traceplot(store.param[,2])

#trace plot for b2
traceplot(store.param[,3])

#trace plot for b3
traceplot(store.param[,4])

#trace plot for sig2
traceplot(store.param[,5])

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
densityplot(store.param[,1])

#density plot for b1
densityplot(store.param[,2])

#density plot for b2
densityplot(store.param[,3])

#density plot for b3
densityplot(store.param[,4])

#density plot for sig2
densityplot(store.param[,5])



