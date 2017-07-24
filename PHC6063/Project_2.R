#data input
article<-read.csv(file="F:/Article Data Clean.csv")

#cross-tabulation counts 
article$N_12<-article$N_1.-article$N_11
article$N_21<-article$N_.1-article$N_11
article$N_22<-article$N-article$N_.1-article$N_1.+article$N_11

#odds ratios
article$od<-((article$N_11+0.5)*(article$N_22+0.5))/((article$N_21+0.5)*(article$N_12+0.5))

#log odds ratios
article$logod<-log(article$od)

#variances for log odds ratios
article$vlogod<-1/(article$N_11+0.5)+1/(article$N_12+0.5)+1/(article$N_21+0.5)+1/(article$N_22+0.5)
article$selogod<-sqrt(article$vlogod)

#CI for log odds ratios
article$logod_LO<-article$logod-1.96*sqrt(article$vlogod)
article$logod_UP<-article$logod+1.96*sqrt(article$vlogod)

#CI for odds ratios
article$od_Lo<-exp(article$logod_LO)
article$od_UP<-exp(article$logod_UP)

#fixed effect models
article$fixweight<-1/article$vlogod
mu_f<-sum(article$fixweight[1:38]*article$logod[1:38])/sum(article$fixweight[1:38])
sigma2_f<-1/sum(article$fixweight[1:38])
sigma_f<-sqrt(sigma2_f)
mu_f_LO<-mu_f-1.96*sqrt(sigma2_f)
mu_f_UP<-mu_f+1.96*sqrt(sigma2_f)

#random effect models
q<-sum(article$fixweight[1:38]*(article$logod[1:38])^2)-(sum(article$fixweight[1:38]*article$logod[1:38]))^2/sum(article$fixweight[1:38])
c<-sum(article$fixweight[1:38])-sum(article$fixweight[1:38]^2)/sum(article$fixweight[1:38])
zeta2<-(q-38+1)/c
article$vlogod2<-article$vlogod+zeta2
article$selogod2<-sqrt(article$vlogod2)
article$randomweight<-1/article$vlogod2
mu_r<-sum(article$randomweight[1:38]*article$logod[1:38])/sum(article$randomweight[1:38])
sigma2_r<-1/sum(article$randomweight[1:38])
sigma_r<-sqrt(sigma2_r)
mu_r_LO<-mu_r-1.96*sqrt(sigma2_r)
mu_r_UP<-mu_r+1.96*sqrt(sigma2_r)

#fixed effect models VS random effect models
qchisq(0.95,37,lower.tail=FALSE)
I2<-(q-38+1)/q

#data output
write.csv(article, file="F:/article output.csv")

#forest plots
forest<-read.csv(file="F:/article graph.csv")
library(metafor)
win.graph()
forest(forest$logod, forest$vlogod, slab=forest$Article)

