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

#calc.log function
calc.log=function(delta,pi){
  part1=dbinom(d1,size=T1,prob=delta)*pi
  part2=dbinom(d1,size=T1,prob=0)*(1-pi)
  sum(log(part1+part2))
}

#sample delta
samp.delta=function(param,jump){
  delta.old=param$delta
  delta.new=tnorm(lo=0,hi=1,mu=delta.old,sig=jump$delta)
  p.old1=calc.log(delta.old,param$pi)+dbeta(delta.old,1,1,log=T)
  p.new1=calc.log(delta.new,param$pi)+dbeta(delta.new,1,1,log=T)
  acceptMH(p.old1,p.new1,delta.old,delta.new)
}

#sample pi
samp.pi=function(param,jump){
  pi.old=param$pi
  pi.new=tnorm(lo=0,hi=1,mu=pi.old,sig=jump$pi)
  p.old2=calc.log(param$delta,pi.old)+dbeta(pi.old,1,1,log=T)
  p.new2=calc.log(param$delta,pi.new)+dbeta(pi.new,1,1,log=T)
  acceptMH(p.old2,p.new2,pi.old,pi.new)
}

#load data
hw13<-read.csv(file="F:/fake data occupancy.csv")
attach(hw13)
n=nrow(hw13)

#initial values
param=list(delta=0.5,pi=0.5)
jump=list(delta=0.1,pi=0.1)
accept=list(delta=0,pi=0)

#gibbs sampler using M-H algorithm
nsim=10000
store.param=matrix(NA,nsim,2)
for (i in 1:nsim){
  delta=samp.delta(param,jump)
  if (delta!=param$delta) accept$delta=accept$delta+1
  param$delta=delta
  
  pi=samp.pi(param,jump)
  if (pi!=param$pi) accept$pi=accept$pi+1
  param$pi=pi
  
  if (i<1000 & i%%50==0){
    if (accept$delta/50>0.4) jump$delta=jump$delta*2
    if (accept$delta/50<0.1) jump$delta=jump$delta*0.5
    if (accept$pi/50>0.4) jump$pi=jump$pi*2
    if (accept$pi/50<0.1) jump$pi=jump$pi*0.5
    accept$delta=0
    accept$pi=0
  }
  
  store.param[i,]=c(param$delta,param$pi)
}

#plots for delta
seq1=seq(from=2000,to=nsim,by=10)
plot(seq1,store.param[seq1,1],type='l',ylab='Sampled Values',xlab='Iterations',main='Delta')
acf(store.param[seq1,1],main='Delta')

#plots for pi
seq1=seq(from=2000,to=nsim,by=10)
plot(seq1,store.param[seq1,2],type='l',ylab='Sampled Values',xlab='Iterations',main='Pi')
acf(store.param[seq1,2],main='Pi')

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

#density plot for delta
densityplot(store.param[seq1,1])

#density plot for pi
densityplot(store.param[seq1,2])