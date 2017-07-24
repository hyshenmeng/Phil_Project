set.seed(1111)
library(rjags)

#load data
hw6<-read.table(file="F:/HW6.txt",header=TRUE)
x=hw6$x
n=length(x)
x_f=hw6$x[1:3]
n_f=length(x_f)
x_m=hw6$x[4:15]
n_m=length(x_m)

#prior parameters
w=0
tau2=100
a=1
b=1

#initial values
sig2=1
mu_f=-4
mu_m=-4

#gibbs sampler
ngibbs=1000
store.para=matrix(NA,ngibbs,3)
for (i in 1:ngibbs){
  
  #draw mu_f parameter
  prec=(n_f/sig2)+(1/tau2)
  var1=1/prec
  media1=var1*((sum(x_f)/sig2)+(w/tau2))
  mu_f=rnorm(1,mean=media1,sd=sqrt(var1))
  
  #draw mu_m parameter
  prec=(n_m/sig2)+(1/tau2)
  var1=1/prec
  media2=var1*((sum(x_m)/sig2)+(w/tau2))
  mu_m=rnorm(1,mean=media2,sd=sqrt(var1))
  
  #draw sig2 parameter
  a1=a+(n/2)
  ssq=sum((x_f-mu_f)^2)+sum((x_m-mu_m)^2)
  b1=b+(ssq/2)
  sig2=1/rgamma(1,a1,b1)
  
  #store results
  store.para[i,]=c(mu_f,mu_m,sig2)
}

#trace plot function
traceplot<-function(x){
  plot(NA,NA,xlim=c(0,ngibbs),ylim=range(x))
  lines(1:ngibbs,x)
} 

#trace plot for mu_f
traceplot(store.para[,1])

#trace plot for mu_m
traceplot(store.para[,2])

#trace plot for sig2
traceplot(store.para[,3])

#trace plot for prec
traceplot(1/store.para[,3])

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
densityplot(store.para[,1])

#density plot for mu_m
densityplot(store.para[,2])

#density plot for sig2
densityplot(store.para[,3])

#density plot for prec
densityplot(1/store.para[,3])

#mu_f v.s. mu_m
diff=abs(store.para[,1])-abs(store.para[,2])
mean(diff<0)
