#alpha range from 0.01 to 0.1
alpha<-seq(from=0.01,to=0.1,by=0.01)

#critical values for alpha
cv_alpha<-qnorm(1-alpha)

#critical values for power
cv_power<-3.2732-1.3273*cv_alpha

#power
power<-pnorm(cv_power)

#table
tab<-cbind(alpha,power)
write.csv(tab, file = "F:/tab.csv")

#graph
plot(alpha,power,type='l',xlab='alpha',ylab='power',col='red')




