set.seed(1111)

#tnorm function
tnorm<-function(lo,hi,mu,sig){
  q1<-pnorm(lo,mu,sig)
  q2<-pnorm(hi,mu,sig)
  z<-runif(1,q1,q2)
  z<-qnorm(z,mu,sig)
  #z[z==-Inf]<-lo[z==-Inf]
  #z[z==Inf]<-hi[z==Inf]
}

#acceptMH function
acceptMH<-function(p.old,p.new,param.old,param.new){
  a<-exp(p.new-p.old)
  z<-runif(1,0,1)
  if (z<a) param.old=param.new
  param.old
}

#load data
hw12<-read.csv(file="F:/bird data.csv")
attach(hw12)
y=log(N.species)
x1=log(area)
media=1.75+0.23*x1
n=length(Island)
a=(n+2)/2
b=sum((y-media)^2)/2+1

#M-H algorithm with jump=0.01
prec=2.5
nsim=10000
store.prec1=matrix(NA,nsim,1)
jump=0.01
for (i in 1:nsim){
  #old prec
  prec.old=prec
  #propose new prec based on truncated normal
  prec.new=tnorm(lo=0,hi=10000,mu=prec.old,sig=jump)
  #caculate target distribution based on old prec
  p.old=sum(dnorm(y,mean=media,sd=sqrt(1/prec.old),log=T))+dgamma(prec.old,shape=1,rate=1,log=T)
  #caculate target distribution based on new prec
  p.new=sum(dnorm(y,mean=media,sd=sqrt(1/prec.new),log=T))+dgamma(prec.new,shape=1,rate=1,log=T) 
  #accept or retain 
  prec=acceptMH(p.old,p.new,prec.old,prec.new)
  store.prec1[i]=prec
}
accept_rate1<-length(unique(store.prec1))/nsim
accept_rate1

#graphs with jump=0.01
plot(1:nsim,store.prec1,type='l',ylab='Sampled Values',xlab='Iterations',main='jump=0.01')
acf(store.prec1,main='Autocorrelation with jump=0.01')
hist(store.prec1,probability=T,main='jump=0.01')
z=seq(from=0,to=5,length.out=10000)
y1=dgamma(z,shape=a,rate=b)
lines(z,y1,col='red')

#M-H algorithm with jump=0.1
prec=2.5
nsim=10000
store.prec2=matrix(NA,nsim,1)
jump=0.1
for (i in 1:nsim){
  #old prec
  prec.old=prec
  #propose new prec based on truncated normal
  prec.new=tnorm(lo=0,hi=10000,mu=prec.old,sig=jump)
  #caculate target distribution based on old prec
  p.old=sum(dnorm(y,mean=media,sd=sqrt(1/prec.old),log=T))+dgamma(prec.old,shape=1,rate=1,log=T)
  #caculate target distribution based on new prec
  p.new=sum(dnorm(y,mean=media,sd=sqrt(1/prec.new),log=T))+dgamma(prec.new,shape=1,rate=1,log=T) 
  #accept or retain 
  prec=acceptMH(p.old,p.new,prec.old,prec.new)
  store.prec2[i]=prec
}
accept_rate2<-length(unique(store.prec2))/nsim
accept_rate2

#graphs with jump=0.1
plot(1:nsim,store.prec2,type='l',ylab='Sampled Values',xlab='Iterations',main='jump=0.1')
acf(store.prec2,main='Autocorrelation with jump=0.1')
hist(store.prec2,probability=T,main='jump=0.1')
z=seq(from=0,to=5,length.out=10000)
y1=dgamma(z,shape=a,rate=b)
lines(z,y1,col='red')

#M-H algorithm with jump=1
prec=2.5
nsim=10000
store.prec3=matrix(NA,nsim,1)
jump=1
for (i in 1:nsim){
  #old prec
  prec.old=prec
  #propose new prec based on truncated normal
  prec.new=tnorm(lo=0,hi=10000,mu=prec.old,sig=jump)
  #caculate target distribution based on old prec
  p.old=sum(dnorm(y,mean=media,sd=sqrt(1/prec.old),log=T))+dgamma(prec.old,shape=1,rate=1,log=T)
  #caculate target distribution based on new prec
  p.new=sum(dnorm(y,mean=media,sd=sqrt(1/prec.new),log=T))+dgamma(prec.new,shape=1,rate=1,log=T) 
  #accept or retain 
  prec=acceptMH(p.old,p.new,prec.old,prec.new)
  store.prec3[i]=prec
}
accept_rate3<-length(unique(store.prec3))/nsim
accept_rate3

#graphs with jump=1
plot(1:nsim,store.prec3,type='l',ylab='Sampled Values',xlab='Iterations',main='jump=1')
acf(store.prec3,main='Autocorrelation with jump=1')
hist(store.prec3,probability=T,main='jump=1')
z=seq(from=0,to=5,length.out=10000)
y1=dgamma(z,shape=a,rate=b)
lines(z,y1,col='red')

#M-H algorithm with jump=10
prec=2.5
nsim=10000
store.prec4=matrix(NA,nsim,1)
jump=10
for (i in 1:nsim){
  #old prec
  prec.old=prec
  #propose new prec based on truncated normal
  prec.new=tnorm(lo=0,hi=10000,mu=prec.old,sig=jump)
  #caculate target distribution based on old prec
  p.old=sum(dnorm(y,mean=media,sd=sqrt(1/prec.old),log=T))+dgamma(prec.old,shape=1,rate=1,log=T)
  #caculate target distribution based on new prec
  p.new=sum(dnorm(y,mean=media,sd=sqrt(1/prec.new),log=T))+dgamma(prec.new,shape=1,rate=1,log=T) 
  #accept or retain 
  prec=acceptMH(p.old,p.new,prec.old,prec.new)
  store.prec4[i]=prec
}
accept_rate4<-length(unique(store.prec4))/nsim
accept_rate4

#graphs with jump=10
plot(1:nsim,store.prec4,type='l',ylab='Sampled Values',xlab='Iterations',main='jump=10')
acf(store.prec4,main='Autocorrelation with jump=10')
hist(store.prec4,probability=T,main='jump=10')
z=seq(from=0,to=5,length.out=10000)
y1=dgamma(z,shape=a,rate=b)
lines(z,y1,col='red')