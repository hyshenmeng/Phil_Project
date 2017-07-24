#alpha, power, null proportion
alpha=0.05
power=0.8
p0=0.22

#critical values for alpha & power
cv_alpha<-qnorm(1-alpha)
cv_power<-qnorm(power)

#one-sample size
p1_L<-seq(from=0,to=0.21,by=0.01)
size_L<-seq(from=0,to=21,by=1)
for (i in 1:22){
  size_L[i]=((cv_alpha*sqrt(p0*(1-p0))+cv_power*sqrt(p1_L[i]*(1-p1_L[i])))/(p1_L[i]-p0))^2
}
tab1<-cbind(p1_L,size_L)
plot(p1_L,size_L,type='l',ylab='sample size',xlab='p1',col='red')
write.csv(tab1, file = "F:/tab1.csv")
p1_U<-seq(from=0.23,to=1,by=0.01)
size_U<-seq(from=23,to=100,by=78)
for (i in 1:78){
  size_U[i]=((cv_alpha*sqrt(p0*(1-p0))+cv_power*sqrt(p1_U[i]*(1-p1_U[i])))/(p1_U[i]-p0))^2
}
tab2<-cbind(p1_U,size_U)
plot(p1_U,size_U,type='l',ylab='sample size',xlab='p1',col='red')
write.csv(tab2, file = "F:/tab2.csv")

#two-sample sample size
delta<-seq(from=0.01,to=1,by=0.01)
size<-seq(from=1,to=100,by=1)
for (i in 1:100){
  size[i]=((cv_alpha+cv_power)^2)/(2*delta[i]^2)
}
tab3<-cbind(delta,size)
plot(delta,size,type='l',ylab='sample size',xlab='delta',col='red')
write.csv(tab3, file = "F:/tab3.csv")

