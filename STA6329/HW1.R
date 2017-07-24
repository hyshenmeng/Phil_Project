
#Page20_Exercise1
170166719 %% 31079

#Page20_Exercise3
e3<-function(r){
  result<-matrix(NA,4,3)
  for (i in 1:4){
  i2<-i*10
  result[i,1]=sum(r^(1:i2))
  result[i,2]=(1-r^(i2+1))/(1-r)
  result[i,3]=result[i,1]-result[i,2]
  }
  print(result)
}
e3(1.08)
e3(1.06)

#Page20_Exercise10
x<-c(0,7,8)
x[0.9999999999999999]
x[0.99999999999999999]

#Page20_Exercise11
rep(0:4,each=5)
rep(seq(1,5),5)

#Page20_Exercise13
more.colors<-c("red","yellow","blue","green","magenta","cyan")
number<-seq(1,6)
a<-rep(seq(1,3),4)
b<-rep(0:3,each=3)
c<-a+b
more.colors[c]

#Page25_Exercise1
solar.radiation<-c(11.1,10.6,6.3,8.8,10.7,11.2,8.9,12.2)
mean(solar.radiation)
median(solar.radiation)
var(solar.radiation)
sr10<-solar.radiation+10
mean(sr10)
median(sr10)
var(sr10)
srm2<-solar.radiation*(-2)
mean(srm2)
median(srm2)
var(srm2)
par(mfrow=c(1,3))
hist(solar.radiation)
hist(sr10)
hist(srm2)
sum((solar.radiation-mean(solar.radiation))^2)/length(solar.radiation)
sum((solar.radiation-mean(solar.radiation))^2)/(length(solar.radiation)-1)

#Page32_Exercise1
www="http://www.stats.uwo.ca/faculty/braun/data/rnf6080.dat"
rain.df<-read.table(www,header=FALSE,quote="\"")
rain.df[rain.df==-999]<-NA
rain.df[2,4]
names(rain.df)
rain.df[2,]
names(rain.df)<-c("year","month","day",seq(0, 23))
rain.df$daily<-rowSums(rain.df[,4:27])
par(mfrow=c(1,1))
hist(rain.df$daily)

#Page32_Exercise2
curve((x<=3)*(3*x+2)+(x>3)*(2*x-0.5*x^2),from=0,to=6)















