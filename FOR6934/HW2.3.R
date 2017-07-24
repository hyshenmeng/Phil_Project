set.seed(1)

#input data
tree<-read.csv(file.choose())
attach(tree)

#maximum likelihood estimation
library(stats4)
nloglik<-function(a,b) {-sum(dgamma(tree$diam, a, b, log=TRUE))}
mle(nloglik, start=list(a=0.1, b=0.1))

#plot the log-likelihood graph
a.seq=seq(from=0.01,to=1.5,length.out=100)
b.seq=seq(from=0.01,to=0.5,length.out=100)
res=matrix(NA,100,100)
for (i in 1:100){
  for (j in 1:100){
    res[i,j]=-nloglik(a.seq[i],b.seq[j])
  }
}
image(a.seq,b.seq,res,zlim=c(min(res),max(res)))
contour(a.seq,b.seq,res,add=TRUE,levels=seq(from=min(res),to=max(res),length.out=10))
abline(h=0.1720328,lty=3)
abline(v=1.0365145,lty=3)