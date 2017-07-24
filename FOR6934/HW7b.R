set.seed(1111)
library(rjags)

#load data
hw7<-read.csv(file.choose())
attach(hw7)

#prior parameters
a.delta=b.delta=1
a.pi=b.pi=1

#initial values
pi_m=pi_f=0.4
delta=0.6
n=length(D1)
n_m=length(D1[sex==0])
n_f=length(D1[sex==1])
S1=rep(0,n)
S1[1:(n/2)]=1
S1[D1>0]=1
param=list(pi_m=pi_m,pi_f=pi_f,delta=delta,S1=S1,sex=sex)

#sample pi_m
samp.pi_m=function(param){
  sum.S1_m=sum(param$S1[sex==0])
  a1=sum.S1_m+a.pi
  b1=n_m-sum.S1_m+b.pi
  rbeta(1,a1,b1)
}

#sample pi_f
samp.pi_f=function(param){
  sum.S1_f=sum(param$S1[sex==1])
  a2=sum.S1_f+a.pi
  b2=n_f-sum.S1_f+b.pi
  rbeta(1,a2,b2)
}

#sample delta
samp.delta=function(param){
  sum.D1=sum(D1[param$S1==1])
  sum.T1=sum(T1[param$S1==1])
  a3=sum.D1+a.delta
  b3=sum.T1-sum.D1+b.delta
  rbeta(1,a3,b3)
}

#sample S1
samp.S1=function(param){
  prob1.tmp_m=dbinom(D1[sex==0],size=T1[sex==0],p=param$delta)*param$pi_m
  prob0.tmp_m=(1-param$pi_m)
  prob_m=prob1.tmp_m/(prob1.tmp_m+prob0.tmp_m)
  prob1.tmp_f=dbinom(D1[sex==1],size=T1[sex==1],p=param$delta)*param$pi_f
  prob0.tmp_f=(1-param$pi_f)
  prob_f=prob1.tmp_f/(prob1.tmp_f+prob0.tmp_f)
  k=c(rbinom(n_m,size=1,p=prob_m),rbinom(n_f,size=1,p=prob_f))
  k[D1>0]=1
  k
}

#gibbs sampler
ngibbs=10000
store.param=matrix(NA,ngibbs,3)
store.S1=matrix(NA,ngibbs,n)
for (i in 1:ngibbs){
  param$pi_m=samp.pi_m(param)
  param$pi_f=samp.pi_f(param)
  param$delta=samp.delta(param)
  param$S1=samp.S1(param)
  store.param[i,]=c(param$pi_m,param$pi_f,param$delta)
  store.S1[i,]=param$S1
}

#trace plot function
traceplot<-function(x){
  plot(NA,NA,xlim=c(0,ngibbs),ylim=range(x))
  lines(1:ngibbs,x)
} 

#trace plot for pi_m
traceplot(store.param[,1])

#trace plot for pi_f
traceplot(store.param[,2])

#trace plot for delta
traceplot(store.param[,3]) 

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

#density plot for pi_m
densityplot(store.param[,1])

#density plot for pi_f
densityplot(store.param[,2])

#density plot for delta
densityplot(store.param[,3])

#pi_m v.s. pi_f
diff=store.param[,1]-store.param[,2]
mean(diff>0)
