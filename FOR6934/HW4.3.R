set.seed(1)

#true values for gamma distribution
a1=2
b1=2
L1=1
U1=3
true1=c(a1/b1,a1/(b1^2),pgamma(U1,a1,b1)-pgamma(L1,a1,b1))

#simulation for gamma distribution
n=10000
x1=matrix(NA,n,3)
gamma=rgamma(n,a1,b1)
for (i in 1:n){
  gamma1=gamma[1:i]
  cond=L1<gamma1 & gamma1<U1
  x1[i,]=c(mean(gamma1),var(gamma1),mean(cond))
}

#plot the results for increasingly larger sample sizes
par(mfrow=c(1,3))
title=c('Mean','Variance','Prob of interval LU')
for (i in 1:3){
  plot(1:n,x1[,i],type='l',ylab='',xlab='Number of Simulations',main=title[i])
  abline(h=true1[i],col='red')
}

#true values for beta distribution
a2=1
b2=5
L2=0.5
U2=0.8
true2=c(a2/(a2+b2),(a2*b2)/(((a2+b2)^2)*(a2+b2+1)),pbeta(U2,a2,b2)-pbeta(L2,a2,b2))

#simulation for beta distribution
n=10000
x2=matrix(NA,n,3)
beta=rbeta(n,a2,b2)
for (i in 1:n){
  beta1=beta[1:i]
  cond1=L2<beta1 & beta1<U2
  x2[i,]=c(mean(beta1),var(beta1),mean(cond1))
}

#plot the results for increasingly larger sample sizes
par(mfrow=c(1,3))
title=c('Mean','Variance','Prob of interval LU')
for (i in 1:3){
  plot(1:n,x2[,i],type='l',ylab='',xlab='Number of Simulations',main=title[i])
  abline(h=true2[i],col='red')
}
