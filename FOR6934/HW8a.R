set.seed(1111)
library(rjags)

#load data
hw8<-read.csv(file.choose())
attach(hw8)
n=length(nid)
dataList=list(y1=fruits)
A1=rep(0,n)
A1[1:(n/2)]=1
A1[fruits>0]=1
initsList=list(pi=0.5,lamda=1,A1=A1)

#specify model
modelString="
model{
for (i in 1:1000){
y1[i]~dpois(ifelse(A1[i]==1,lamda,0))
}
for (i in 1:1000){
A1[i]~dbern(pi)
}
pi~dbeta(1,1)
lamda~dgamma(1,1)
}
"
writeLines(modelString, con="TEMPmodel.txt")

#generate chains
jagSModel=jags.model(file="TEMPmodel.txt",data=dataList,inits=initsList,n.chain=4,n.adapt=100)
nchain=4

#burn-in period
update(jagSModel,n.iter=1000)

#generate posterior samples
jagsSamples=jags.samples(jagSModel,c('pi','lamda'),n.iter=1000)
niter=1000

#trace plot for pi
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$pi))
for (i in 1:nchain) lines(1:niter,jagsSamples$pi[1,,i],col=i)

#trace plot for lamda
par(mfrow=c(1,1),mar=rep(4,4))
plot(NA,NA,xlim=c(0,niter),ylim=range(jagsSamples$lamda))
for (i in 1:nchain) lines(1:niter,jagsSamples$lamda[1,,i],col=i)

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
densityplot(jagsSamples$pi)

#density plot for lamda
densityplot(jagsSamples$lamda)