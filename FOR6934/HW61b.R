set.seed(1111)
library(rjags)

#load data
hw6<-read.table(file="F:/HW6.txt",header=TRUE)
x=hw6$x
n=length(x)

#prior parameters
w=-5
tau2=2.5^2
a=1
b=1

#initial values
sig2=1
mu=-4

#gibbs sampler
ngibbs=1000
store.para=matrix(NA,ngibbs,2)
for (i in 1:ngibbs){
  
  #draw mu parameter
  prec=(n/sig2)+(1/tau2)
  var1=1/prec
  media=var1*((sum(x)/sig2)+(w/tau2))
  mu=rnorm(1,mean=media,sd=sqrt(var1))
  
  #draw sig2 parameter
  a1=a+(n/2)
  ssq=sum((x-mu)^2)
  b1=b+(ssq/2)
  sig2=1/rgamma(1,a1,b1)
  
  #store results
  store.para[i,]=c(mu,sig2)
}

#trace plot function
traceplot<-function(x){
  plot(NA,NA,xlim=c(0,ngibbs),ylim=range(x))
  lines(1:ngibbs,x)
} 

#trace plot for mu
traceplot(store.para[,1])

#trace plot for sig2
traceplot(store.para[,2])

#trace plot for prec
traceplot(1/store.para[,2])

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
densityplot(store.para[,1])
y=seq(from=-15,to=5,length.out=100)
lines(y,dnorm(y,mean=-5,sd=2.5),col='blue',lwd=3)
abline(v=-5)

#density plot for sig2
densityplot(store.para[,2])

#density plot for prec
densityplot(1/store.para[,2])
