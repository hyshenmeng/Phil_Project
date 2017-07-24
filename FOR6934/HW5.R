set.seed(1234)

#generative model's parameters
pi1=0.5
lambda1=2

#generate data from the generative model
n=1000
ai<-rbinom(n,size=1,prob=pi1)
tab_ai<-table(ai)
tab_ai
fia1<-rpois(sum(ai==1),lambda=lambda1)
tab_fia1<-table(fia1)
tab_fia1
fia0<-rep(0,sum(ai==0))
length(fia0)

#p(ai=0|fi=0)
prop<-length(fia0)/(length(fia0)+tab_fia1[1])
prop

#plot graph
plot(tab_fia1,type='h',ylab="Number of trees",xlab="Number of fruits",col='black',lwd=2,ylim=c(0,600))
lines(rep(0,2),c(tab_fia1[1],length(fia0)),col='red',lwd=2)
legend("topright",c("Adults","No adults"),text.col=c("black","red"))
