set.seed(1111)
library(rjags)

#load data
hw8<-read.csv(file.choose())
attach(hw8)

#prior parameters
a1=b1=1
a2=b2=1

#initial values
pi=0.5
lamda=0.1
n=length(nid)
y1=fruits
A1=rep(0,n)
A1[1:(n/2)]=1
A1[fruits>0]=1
param=list(pi=pi,lamda=lamda,A1=A1)

#sample pi
samp.pi=function(param){
  sum.A1=sum(param$A1)
  a.pi=sum.A1+a2
  b.pi=n-sum.A1+b2
  rbeta(1,a.pi,b.pi)
}

#sample lamda
samp.lamda=function(param){
  sum.A1=sum(param$A1)
  sum.y_A1=sum(y1[param$A1==1])
  a.lamda=sum.y_A1+a1
  b.lamda=sum.A1+b1
  rgamma(1,a.lamda,b.lamda)
}

#sample A1
samp.A1=function(param){
  prob.tmp1=exp(-param$lamda)*param$pi
  prob.tmp2=1-param$pi
  prob=prob.tmp1/(prob.tmp1+prob.tmp2)
  k=rbinom(n,size=1,p=prob)
  k[y1>0]=1
  k
}

#gibbs sampler
ngibbs=10000
store.param=matrix(NA,ngibbs,2)
store.A1=matrix(NA,ngibbs,n)
for (i in 1:ngibbs){
  param$pi=samp.pi(param)
  param$lamda=samp.lamda(param)
  param$A1=samp.A1(param)
  store.param[i,]=c(param$pi,param$lamda)
  store.A1[i,]=param$A1
}

#trace plot function
traceplot<-function(x){
  plot(NA,NA,xlim=c(0,ngibbs),ylim=range(x))
  lines(1:ngibbs,x)
} 

#trace plot for pi
traceplot(store.param[,1])

#trace plot for lamda
traceplot(store.param[,2]) 

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

#density plot for pi
densityplot(store.param[,1])

#density plot for lamda
densityplot(store.param[,2])


