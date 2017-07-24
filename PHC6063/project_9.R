#9.1
pj9<-read.csv(file="F:/transplants.csv")
pj9$FOLLOWUP=seq(from=12,to=1,length.out=12)
time=sum(pj9$transplant*pj9$FOLLOWUP)
lambda=(1.6*time)/100000
pvalue=ppois(4,lambda,lower.tail=F)
pvalue

##################################################

#9.2
##NON-MALIGNANCY
###load data
pj9_NM<-read.csv(file="F:/pj9_NM.csv")

###subset data
table(pj9_NM$PREVIOUS_MALIGNANCY)
pj9_NM1<-subset(pj9_NM,pj9_NM$PREVIOUS_MALIGNANCY=="No")

###tabulate data
tabgender<-table(pj9_NM1$RECIPIENT_GENDER)
tabgender
tabrace<-table(pj9_NM1$RECIPIENT_ETHNICITY)
tabrace
taborgan_NM<-table(pj9_NM1$TRANSPLANTED_ORGAN)
taborgan_NM

###summarize data
summary(pj9_NM1$RECIPIENT_AGE)
sd(pj9_NM1$RECIPIENT_AGE)

###caculate follow-up time
library(lubridate)
pj9_NM1$TRANSPLANT_DATE<-as.character(pj9_NM1$TRANSPLANT_DATE)
pj9_NM1$LAST_FOLLOWUP<-as.character(pj9_NM1$LAST_FOLLOWUP)
pj9_NM1$FOLLOWUP=year(strptime(pj9_NM1$LAST_FOLLOWUP,format="%m/%d/%Y"))-year(strptime(pj9_NM1$TRANSPLANT_DATE,format="%m/%d/%Y"))
aggregate(pj9_NM1$FOLLOWUP,by=list(pj9_NM1$TRANSPLANTED_ORGAN),FUN=sum)
sum(pj9_NM1$FOLLOWUP)

##################################################

##MALIGNANCY
###load data
pj9_M<-read.csv(file="F:/pj9_M.csv")

###subset data
table(pj9_M$Previous_SCC)
pj9_M1<-subset(pj9_M,Previous_SCC=="No")

###tabulate data
taborgan_M<-table(pj9_M1$Transplant_type)
taborgan_M

###summarize data
pj9_M1$Transplant_date<-as.character(pj9_M1$Transplant_date)
pj9_M1$DOB<-as.character(pj9_M1$DOB)
pj9_M1$AGE=year(strptime(pj9_M1$Transplant_date,format="%m/%d/%Y"))-year(strptime(pj9_M1$DOB,format="%m/%d/%Y"))
summary(pj9_M1$AGE)
sd(pj9_M1$AGE)

###caculate follow-up time
pj9_M1$Malignancy_date<-as.character(pj9_M1$Malignancy_date)
pj9_M1$FOLLOWUP=year(strptime(pj9_M1$Malignancy_date,format="%m/%d/%Y"))-year(strptime(pj9_M1$Transplant_date,format="%m/%d/%Y"))
aggregate(pj9_M1$FOLLOWUP,by=list(pj9_M1$Transplant_type),FUN=sum)
sum(pj9_M1$FOLLOWUP)

##################################################

##MALIGNANCY WITHOUT SCC

###subset data
table(pj9_M$SCC)
pj9_M2<-subset(pj9_M1,SCC=="No")

###tabulate data
taborgan_M2<-table(pj9_M2$Transplant_type)
taborgan_M2

###caculate follow-up time
aggregate(pj9_M2$FOLLOWUP,by=list(pj9_M2$Transplant_type),FUN=sum)
sum(pj9_M2$FOLLOWUP)

##################################################

###Chi-squared test
organ<-matrix(c(473,439,1896,1218,195,71,272,157),ncol=2,dimnames=list(Transplant=c("Heart","Lung","Kidney","Liver"),Post=c("Non-malignancy","Malignancy")))
organ
chisq.test(organ)

###t-test
t.test(pj9_NM1$RECIPIENT_AGE,pj9_M1$AGE)

##################################################

##incidence rate function
incidencerate<-function(f,t1,t2){
  rate=(f*100000)/(t1+t2)
  z=(rate-471)/sqrt(471)
  pvalue=pnorm(z,lower.tail=F)
  print(c(rate,pvalue))
} 

###ir1 for heart
incidencerate(195,1316,2315)

###ir1 for lung
incidencerate(71,325,1396)

###ir1 for kidney
incidencerate(272,1677,10356)

###ir1 for liver
incidencerate(157,1018,6642)

###ir1 for all
incidencerate(692,4314,20975)

###ir2 for heart
incidencerate(75,544,2315)

###ir2 for lung
incidencerate(22,85,1396)

###ir2 for kidney
incidencerate(105,620,10356)

###ir2 for liver
incidencerate(98,588,6642)

###ir2 for all
incidencerate(299,1825,20975)

##################################################

write.csv(pj9_NM1, file="F:/pj9_NM1.csv")
write.csv(pj9_M1, file="F:/pj9_M1.csv")

