set.seed(1111)
library(rjags)

#load data
hw7<-read.csv(file.choose())
attach(hw7)
dataList=list(D1=D1,T1=T1)
S1=rep(0,100)
S1[1:50]=1
S1[D1>0]=1
initsList=list(pi_m=0.4,pi_f=0.4,delta=0.6,S1=S1)

#specify model
modelString="
model{
for (i in 1:100){
D1[i]~dbin(ifelse(S1[i]==0,0,delta),T1[i])
}
for (i in 1:50){
S1[i]~dbern(pi_m)
}
for (i in 51:100){
S1[i]~dbern(pi_f)
}
pi_m~dbeta(1,1)
pi_f~dbeta(1,1)
delta~dbeta(1,1)
}
"
writeLines(modelString, con="TEMPmodel.txt")

#generate chains
jagSModel=jags.model(file="TEMPmodel.txt",data=dataList,inits=initsList,n.chain=4,n.adapt=100)
nchain=4

#burn-in period
update(jagSModel,n.iter=1000)

#geberate posterior samples
jagsSamples=jags.samples(jagSModel,c('pi_m','pi_f','delta'),n.iter=1000)
niter=1000

#trace plot for pi_m
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$pi_m))
for (i in 1:nchain) lines(1:niter,jagsSamples$pi_m[1,,i],col=i)

#trace plot for pi_f
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$pi_f))
for (i in 1:nchain) lines(1:niter,jagsSamples$pi_f[1,,i],col=i)

#trace plot for delta
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$delta))
for (i in 1:nchain) lines(1:niter,jagsSamples$delta[1,,i],col=i)

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
densityplot(jagsSamples$pi_m)

#density plot for pi_f
densityplot(jagsSamples$pi_f)

#density plot for delta
densityplot(jagsSamples$delta)

#pi_m v.s. pi_f
diff=jagsSamples$pi_m-jagsSamples$pi_f
mean(diff>0)
