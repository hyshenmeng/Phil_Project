set.seed(1111)

#load data
data<-read.csv(file="F:/roaches.csv")
param<-read.csv(file="F:/roaches posterior after burnin.csv")

#prepare data
estim<-apply(param,2,mean)
range<-range(data$distance)
seq1<-seq(from=range[1],to=range[2],length.out=1000)
nsim<-nrow(param)

#without uncertainty
yhat=exp(estim[1]+estim[2]*seq1)

#with parameter uncertainty
yhat1=matrix(NA,1000,3)
for(i in 1:1000){
  tmp1=exp(param[,1]+param[,2]*seq1[i])
  yhat1[i,]=quantile(tmp1,c(0.025,0.5,0.975))
}

#with parameter & sampling uncertainty
yhat2=matrix(NA,1000,3)
for(i in 1:1000){
  tmp2=rpois(nsim,lambda=exp(param[,1]+param[,2]*seq1[i]))
  yhat2[i,]=quantile(tmp2,c(0.025,0.5,0.975))
}

#plot graph
plot(data$distance,data$roaches)
lines(seq1,yhat,col='red')
lines(seq1,yhat1[,1],col='blue',lty=3)
lines(seq1,yhat1[,3],col='blue',lty=3)
lines(seq1,yhat2[,1],col='orange',lty=3)
lines(seq1,yhat2[,3],col='orange',lty=3)

#proportion of zeros from original data
prop0<-mean(data$roaches==0)
prop0

#proportion of zeros from predictive distribution
res<-rep(NA,nsim)
for(i in 1:nsim){
  param1<-as.numeric(param[i,])
  dat.new=rpois(1000,lambda=exp(param1[1]+param1[2]*data$distance))
  res[i]=mean(dat.new==0)
}

#plot graph
hist(res,main='proportion of zeros',xlim=c(0,0.3),xlab='')
abline(v=prop0,col='red')

