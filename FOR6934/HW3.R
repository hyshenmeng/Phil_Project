set.seed(1)

#data input
hw3<-read.table(file="F:/HW3.txt",header=TRUE)
n=nrow(hw3)
xmean=mean(hw3$x)
xvar=8
xprec=n/xvar

#prior parameters
mu_0=-5
sigma2_0=2.5^2
sigma_0=2.5
prec_0=1/sigma2_0

#posterior parameters
prec_1=xprec+prec_0
sigma2_1=1/prec_1
sigma_1=sqrt(sigma2_1)
mu_1=(prec_0*mu_0+xprec*xmean)/prec_1

#plot posterior distribution
xseq<-seq(from=-25,to=25,length.out=1000)
plot(xseq,dnorm(xseq,mu_1,sigma_1),type='l',ylab='Density',xlab='Outcomes',col='red')

#plot prior distribution
lines(xseq,dnorm(xseq,mu_0,sigma_0),type="l")

