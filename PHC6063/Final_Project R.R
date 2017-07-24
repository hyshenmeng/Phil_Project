library(bestglm)
library(leaps)
library(reshape2)
library(ggplot2)
library(pROC)

#data input
d1<-read.csv(file="F:/Final_Project_Data.csv")

#data managment
attach(d1)
##transform
d1$T_VEGF=(VEGF/Creatinine)^(1/3)
d1$T_MMP.9=(MMP.9/Creatinine)^(1/3)
d1$T_Angiogenin=(Angiogenin/Creatinine)^(1/3)
d1$T_CA9=(CA9/Creatinine)^(1/3)
d1$T_PA1=(PA1/Creatinine)^(1/3)
d1$T_IL8=(IL8/Creatinine)^(1/3)
d1$T_APOE=(APOE/Creatinine)^(1/3)
d1$T_MMP.10=(MMP.10/Creatinine)^(1/3)
##categorize
d1$Y[d1$Group=="TUMOR"]<-1
d1$Y[d1$Group!="TUMOR"]<-0
##subset
cancer<-d1[,25:33]
cancer0<-cancer[which(cancer$Y==0),]
n0<-nrow(cancer0)
cancer1<-cancer[which(cancer$Y==1),]
n1<-nrow(cancer1)
##boxplot
cancer.m<-melt(cancer,id.var="Y")
ggplot(data=cancer.m,aes(x=variable,y=value))+geom_boxplot(aes(fill=as.factor(Y)))
ggplot(data=cancer.m,aes(x=as.factor(Y),y=value))+geom_boxplot()+facet_wrap(~variable,ncol=4)
detach(d1)

#export dataset to csv for data summary using SAS
write.csv(d1,file="F:/Final_Project_Data_for_SAS.csv")

#model selection
##best subset logistic regression with bestglm
nmodel=10
best<-bestglm(Xy=cancer,family=binomial,IC="BIC",TopModels=nmodel,method="exhaustive")
df<-best$BestModels
##bootstrap best subset logistic regression
nboot=1000
for (i in 1:nboot){
  print(i)
  ##stratified bootstrap
  cancer0_s<-cancer0[sample(n0,replace=T),]
  cancer1_s<-cancer1[sample(n1,replace=T),]
  cancer_s<-rbind(cancer0_s,cancer1_s)
  ##best subset glm
  best_s<-bestglm(Xy=cancer_s,family=binomial,IC="BIC",TopModels=nmodel,method="exhaustive")
  ##store best models
  df<-rbind(df,best_s$BestModels)
}
##best model
df[df=="TRUE"]<-1
df$MID<-paste(df$T_VEGF,df$T_MMP.9,df$T_Angiogenin,df$T_CA9,df$T_PA1,df$T_IL8,df$T_APOE,df$T_MMP.10,sep = "")
df$RANK<-rep(1:nmodel,nboot+1)
tab<-table(df$MID,df$RANK)
tab
margin.table(tab,1)
#####The most frequent model is 01011110#####
model<-df[df$MID=="01011110",]
head(model)
#####Best biomarker combination includes T_MMP.9, T_CA9, T_PA1, T_IL8, and T_APOE#####

#export model selection dataset to csv
write.csv(df,file="F:/Model_Selection.csv")


