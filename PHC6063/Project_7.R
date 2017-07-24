#block function
block=function(n){
  store=matrix(NA,n,4)
  for (i in 1:n){
    u=runif(1)
    count=sum(store[,2],na.rm=T)
    c=((n/2)-count)/(n+1-i)
    if (u<c){a=1}
    else {a=0}
    store[i,]=c(u,a,count,c)
  }
  store
}

#generate blocks
b1<-block(10)
b2<-block(10)
b3<-block(10)
b4<-block(10)
b5<-block(10)
b6<-block(10)
b7<-block(10)
b8<-block(10)
b9<-block(6)

#stack matrices
m=rbind(b1,b2,b3,b4,b5,b6,b7,b8,b9)
ct<-data.frame(m)
colnames(ct)<-c('u','a','count','c')
ct$trt<-ifelse(ct$a==1,'A','B')
write.csv(ct,file="F:/clinical_trials.csv")

#############################################

size=seq(from=3,to=7,by=1)
#quantile k
k=qbinom(0.05,size,0.33)
k

#probablity p
p=1-0.8^(1/size)
p


