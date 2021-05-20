
---------------------------------------------------------------------------------------
                              ###------LOADING THE DATA--------###
---------------------------------------------------------------------------------------
d=read.csv("C:/Jig17230/Capstone Project_Telecom/telecomfinal.csv",na.strings=c("",NA))
head(d)
str(d)

library(dplyr)
options(scipen = 999)

#Sanity Check
names(d)
summary(d)

--------------------------------------------------------------------------------------
                              ###-------DATA QUALITY REPORT-------###
--------------------------------------------------------------------------------------
#Variable Names
variables<-names(d)
QR<-as.data.frame(variables)
rm(variables)

#Variable datatype
QR$datatype<-sapply(d,class)
#No of records
QR$NoofRecords<-nrow(d)

#Unique records
for(i in 1:ncol(d))
{
  QR$Uniquerecords[i]<-length(unique(d[,i]))
    }
#Data availability _Variables
QR$available<-colSums(!is.na(d))
QR$availablepercent<-round(colMeans(!is.na(d)),4)

#Missing Values_Variables
QR$missing<-colSums(is.na(d))
QR$missingpercent<-round(colMeans(is.na(d)),4)

#Summary statistics
for(i in 1:ncol(d))
{
  QR$Minimum[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                              min(d[,i],na.rm = T),0),2)
  QR$Maximum[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                              max(d[,i],na.rm = T),0),2)
  QR$Mean[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                              mean(d[,i],na.rm = T),0),2)
  QR$fifthpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                              quantile(d[,i],p=0.05,na.rm = T),0),2)
  QR$tenthpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                                      quantile(d[,i],p=0.10,na.rm = T),0),2)
  QR$twentyfifthpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                                      quantile(d[,i],p=0.25,na.rm = T),0),2)
  QR$fiftythpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                                      quantile(d[,i],p=0.50,na.rm = T),0),2)
  QR$seventyfifthpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                                      quantile(d[,i],p=0.75,na.rm = T),0),2)
  QR$ninetythpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                                      quantile(d[,i],p=0.90,na.rm = T),0),2)
  QR$ninetyfifthpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                                      quantile(d[,i],p=0.95,na.rm = T),0),2)
  QR$ninetyninethpercentile[i]<-round(ifelse(class(d[,i])=="integer"|class(d[,i])=="numeric",
                                            quantile(d[,i],p=0.99,na.rm = T),0),2)
}

write.csv(QR,"Data Quality Report",row.names = T)

---------------------------------------------------------------------------------------------------
                          ###-------DATA PREPARATION-------###
---------------------------------------------------------------------------------------------------
#14 Variables with more than 15% missing values. We are removing 13 variables except retdays
#As retdays is an important variable we are treating this variable and including in the analysis
#New dataset creation by omitting the variables with more than 15% missing values

names(QR)
variables_missing<-as.numeric(sum(QR$missingpercent>=0.15))
QR$variables[(QR$missingpercent>=0.15)]

#Missing Value treatment for retdays
#More than 96% values are missing. Creating dummy variable
summary(d$retdays)
sort(unique(d$retdays),na.last = F)
d$retdays_n<-ifelse(is.na(d$retdays)==TRUE,0,1)
summary(d$retdays_n)

#Omitting variables with more than 15% missing values
d_n<-d[,colMeans(is.na(d))<=0.15]
names(d_n)

#DROP_BLK_MEAN=BLCK_DAT_MEAN + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN.
#Omitting blck_dat_mean from the dataset
d_n<-d_n[-50]
names(d_n)


----------------------------------------------------------------------------------------------
                         ###-------DATA EXPLORATION_PROFILING--------### 
----------------------------------------------------------------------------------------------

#Deciling - Continuous Variables
--------------------------------
library(ggplot2)
names(d_n)  
str(d_n)
  
#mou_Mean
summary(d_n$mou_Mean)
d_n%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(d_n%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-round(dat1$n/dat1$N,2)
dat1$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(d_n%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("moumean",nrow(dat1))
dat1
plot(dat1$churn_perc)

#totmrc_Mean
summary(d_n$totmrc_Mean)
d_n%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(d_n%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(d_n%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrcmean",nrow(dat2))
dat2
plot(dat2$churn_perc)

#rev_range
summary(d_n$rev_Range)
d_n%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(d_n%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(d_n%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("revrange",nrow(dat3))
dat3
plot(dat3$churn_perc)

#mou_Range
summary(d_n$mou_Range)
d_n%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(d_n%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(d_n%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mourange",nrow(dat4))
dat4
plot(dat4$churn_perc)

#change_mou
summary(d_n$change_mou)
d_n%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(d_n%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(d_n%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("changemou",nrow(dat5))
dat5
plot(dat5$churn_perc)

#drop_blk_Mean
summary(d_n$drop_blk_Mean)
d_n%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(d_n%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$LessThan<-unclass(d_n%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("dropblkmean",nrow(dat6))
dat6
plot(dat6$churn_perc)

#drop_vce_Range
summary(d_n$drop_vce_Range)
d_n%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(d_n%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$LessThan<-unclass(d_n%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("dropvcerange",nrow(dat7))
dat7
plot(dat7$churn_perc)

#owylis_vce_Range
summary(d_n$owylis_vce_Range)
d_n%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(d_n%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$LessThan<-unclass(d_n%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis",nrow(dat8))
dat8
plot(dat8$churn_perc)

#mou_opkv_Range
summary(d_n$mou_opkv_Range)
d_n%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(d_n%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$LessThan<-unclass(d_n%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mouopkvrange",nrow(dat9))
dat9
plot(dat9$churn_perc)

#months
summary(d_n$months)
d_n%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(d_n%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(d_n%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))
dat10
plot(dat10$churn_perc)

#totcalls
summary(d_n$totcalls)
d_n%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(d_n%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$LessThan<-unclass(d_n%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))
dat11
plot(dat11$churn_perc)

#eqpdays
summary(d_n$eqpdays)
#Only one missing value.Will remove
index<-which(is.na(d_n$eqpdays))
d_n<-d_n[-index,]
d_n%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(d_n%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat12$LessThan<-unclass(d_n%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))
dat12
plot(dat12$churn_perc)

#custcare_Mean
#Getting less number of deciles. Omit
summary(d_n$custcare_Mean)
d_n%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$varname<-rep("custcaremean",nrow(dat13))
dat13
plot(d_n$churn,d_n$custcare_Mean)


#callwait_Mean
summary(d_n$callwait_Mean)
d_n%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(d_n%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$LessThan<-unclass(d_n%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwaitmean",nrow(dat14))
dat14
plot(dat14$churn_perc)

#iwylis_vce_Mean
summary(d_n$iwylis_vce_Mean)
d_n%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(d_n%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$LessThan<-unclass(d_n%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylisvcemean",nrow(dat15))
dat15
plot(dat15$churn_perc)

#callwait_Range
#Less number of deciles. Omit
summary(d_n$callwait_Range)
d_n%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$varname<-rep("callwaitrange",nrow(dat16))
dat16
plot(d_n$churn,d_n$callwait_Range)

#ccrndmou_Range
#Less number of deciles. Omit
summary(d_n$ccrndmou_Range)
d_n%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$varname<-rep("ccrndmou",nrow(dat17))
dat17

#adjqty
summary(d_n$adjqty)
d_n%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(d_n%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$LessThan<-unclass(d_n%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))
dat18
plot(dat18$churn_perc)

#ovrrev_Mean
summary(d_n$ovrrev_Mean)
d_n%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(d_n%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$LessThan<-unclass(d_n%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrevmean",nrow(dat19))
dat19
plot(dat19$churn_perc)

#rev_Mean
summary(d_n$rev_Mean)
d_n%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(d_n%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$LessThan<-unclass(d_n%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("revmean",nrow(dat20))
dat20
plot(dat20$churn_perc)

#ovrmou_Mean
summary(d_n$ovrmou_Mean)
d_n%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(d_n%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$LessThan<-unclass(d_n%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmoumean",nrow(dat21))
dat21
plot(dat21$churn_perc)

#comp_vce_Mean
#considered in derived variables-Delete
summary(d_n$comp_vce_Mean)
d_n%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(d_n%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$LessThan<-unclass(d_n%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("compvcemean",nrow(dat22))
dat22
plot(dat22$churn_perc)

#plcdd_vce_Mean
#considered in derived variables-Delete
summary(d_n$plcd_vce_Mean)
d_n%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(d_n%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$LessThan<-unclass(d_n%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcdvcemean",nrow(dat23))
dat23
plot(dat23$churn_perc)

#avg3mou
summary(d_n$avg3mou)
d_n%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(d_n%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$LessThan<-unclass(d_n%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))
dat24
plot(dat24$churn_perc)

#avgmou
summary(d_n$avgmou)
d_n%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(d_n%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$LessThan<-unclass(d_n%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))
dat25
plot(dat25$churn_perc)


#avg3qty
summary(d_n$avg3qty)
d_n%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(d_n%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$LessThan<-unclass(d_n%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))
dat26
plot(dat26$churn_perc)

#avgqty
summary(d_n$avgqty)
d_n%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(d_n%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$LessThan<-unclass(d_n%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))
dat27
plot(dat27$churn_perc)

#avg6mou
summary(d_n$avg6mou)
d_n%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(d_n%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$LessThan<-unclass(d_n%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))
dat28
plot(dat28$churn_perc)

#avg6qty
summary(d_n$avg6qty)
d_n%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(d_n%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$LessThan<-unclass(d_n%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))
dat29
plot(dat29$churn_perc)

#opk_dat_Mean
#Less deciles-omit
summary(d_n$opk_dat_Mean)
d_n%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$varname<-rep("opkdatmean",nrow(dat30))
dat30

#roam_Mean
#less deciles-omit
summary(d_n$roam_Mean)
d_n%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$varname<-rep("roammean",nrow(dat31))
dat31


#recv_sms_Mean
#less deciles-omit
summary(d_n$recv_sms_Mean)
d_n%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$varname<-rep("recvsmsmean",nrow(dat32))
dat32


#mou_pead_Mean
#less deciles-omit
summary(d_n$mou_pead_Mean)
d_n%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$varname<-rep("moupeadmean",nrow(dat33))
dat33

#da_Mean
summary(d_n$da_Mean)
d_n%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$N<-unclass(d_n%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat34$LessThan<-unclass(d_n%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat34$varname<-rep("damean",nrow(dat34))
dat34
plot(dat34$churn_perc)

#da_Range
summary(d_n$da_Range)
d_n%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$N<-unclass(d_n%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat35$LessThan<-unclass(d_n%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat35$varname<-rep("darange",nrow(dat35))
dat35
plot(dat35$churn_perc)

#datovr_Mean
#less deciles-omit.
summary(d_n$datovr_Mean)
d_n%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$varname<-rep("datovrmean",nrow(dat36))
dat36

#datovr_Range
#less deciles.omit
summary(d_n$datovr_Range)
d_n%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$varname<-rep("datovrrange",nrow(dat37))
dat37


#drop_dat_Mean
#less deciles. omit
summary(d_n$drop_dat_Mean)
d_n%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$varname<-rep("dropdatmean",nrow(dat38))
dat38


#drop_vce_Mean
summary(d_n$drop_vce_Mean)
d_n%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$N<-unclass(d_n%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat39$churn_perc<-dat39$n/dat39$N
dat39$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat39$LessThan<-unclass(d_n%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat39$varname<-rep("dropvcemean",nrow(dat39))
dat39
plot(dat39$churn_perc)

#adjmou
summary(d_n$adjmou)
d_n%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$N<-unclass(d_n%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat40$churn_perc<-dat40$n/dat40$N
dat40$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat40$LessThan<-unclass(d_n%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat40$varname<-rep("adjmou",nrow(dat40))
dat40
plot(dat40$churn_perc)

#totrev
summary(d_n$totrev)
d_n%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$N<-unclass(d_n%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat41$churn_perc<-dat41$n/dat41$N
dat41$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat41$LessThan<-unclass(d_n%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat41$varname<-rep("totrev",nrow(dat41))
dat41
plot(dat41$churn_perc)

#adjrev
summary(d_n$adjrev)
d_n%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$N<-unclass(d_n%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat42$churn_perc<-dat42$n/dat42$N
dat42$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat42$LessThan<-unclass(d_n%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat42$varname<-rep("adjrev",nrow(dat42))
dat42
plot(dat42$churn_perc)

#avgrev
summary(d_n$avgrev)
d_n%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(d_n%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat43$LessThan<-unclass(d_n%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat43$varname<-rep("avgrev",nrow(dat43))
dat43
plot(dat43$churn_perc)

#comp_dat_Mean
#considered in derived variables. omit
summary(d_n$comp_dat_Mean)
d_n%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$varname<-rep("compdatmean",nrow(dat44))
dat44


#plcd_dat_Mean
#considered in derived variables-omit
summary(d_n$plcd_dat_Mean)
d_n%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$varname<-rep("plcddatmean",nrow(dat45))
dat45


#Derived Variables_creating dummy variables

#plcd_Attempt_Mean
d_n$plcd_attempt_Mean<-d_n$plcd_dat_Mean+d_n$plcd_vce_Mean
d_n%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$N<-unclass(d_n%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat46$churn_perc<-dat46$n/dat46$N
dat46$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_attempt_Mean)))[[2]]
dat46$LessThan<-unclass(d_n%>%mutate(dec=ntile(plcd_attempt_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_attempt_Mean)))[[2]]
dat46$varname<-rep("plcdattemptmean",nrow(dat46))
dat46
plot(dat46$churn_perc)


#complete mean
d_n$complete_Mean<-d_n$comp_dat_Mean+d_n$comp_vce_Mean
d_n%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$N<-unclass(d_n%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat47$churn_perc<-dat47$n/dat47$N
dat47$GreaterThan<-unclass(d_n%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(min(complete_Mean)))[[2]]
dat47$LessThan<-unclass(d_n%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(max(complete_Mean)))[[2]]
dat47$varname<-rep("completemean",nrow(dat47))
dat47
plot(dat47$churn_perc)

#continuous variables list
csv_cont_var=rbind(dat1,dat3,dat4,dat5,dat6,dat7,dat9,
                   dat11,dat14,dat15,dat18,dat19,dat20,dat21,dat24,
                   dat25,dat26,dat27,dat28,dat29,dat34,dat35,dat39,dat40,
                   dat41,dat42,dat43,dat46,dat47)

write.csv(csv_cont_var,"continuous variables.csv",row.names = T)
names(d_n)

#Removing variables with less deciles
#plcd_dat_Mean,plcd_vce_Mean,comp_dat-Mean,comp_vce_Mean
d_n<-d_n[,-c(13,16,17,45,48,49,50,56,57,58,22,23,65,66)]

names(d_n)



------------------------------------------------------------------------------
#Event Rate_Categorical variables
----------------------------------  

#crclscod
#some levels show less churn rate
summary(d_n$crclscod)
str(d_n$crclscod)
d_n%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC1
datC1$N<-unclass(d_n%>%filter(crclscod%in%datC1$levels)%>%count(crclscod))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("crclscod",nrow(datC1))
datC1
datC1$Group<-cut(datC1$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)

#asl_flag
summary(d_n$asl_flag)
d_n%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC2
datC2$N<-unclass(d_n%>%filter(asl_flag%in%datC2$levels)%>%count(asl_flag))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("aslflag",nrow(datC2))
datC2$Group<-cut(datC2$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC2

#prizm_social_one
summary(d_n$prizm_social_one)
d_n%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC3
datC3$N<-unclass(d_n%>%filter(prizm_social_one%in%datC3$levels)%>%count(prizm_social_one))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("prizmsocialone",nrow(datC3))
datC3
datC3$Group<-cut(datC3$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)

#area
summary(d_n$area)
d_n%>%count(churn,levels=area)%>%filter(churn==1)->datC4
datC4$N<-unclass(d_n%>%filter(area%in%datC4$levels)%>%count(area))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("area",nrow(datC4))
datC4$Group<-cut(datC4$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)
datC4

#refurb_new
summary(d_n$refurb_new)
d_n%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC5
datC5$N<-unclass(d_n%>%filter(refurb_new%in%datC5$levels)%>%count(refurb_new))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("refrbnew",nrow(datC5))
datC5$Group<-cut(datC5$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC5

#hnd_webcap
summary(d_n$hnd_webcap)
d_n%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC6
datC6$N<-unclass(d_n%>%filter(hnd_webcap%in%datC6$levels)%>%count(hnd_webcap))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N
datC6$Var.Name<-rep("hndwebcap",nrow(datC6))
datC6$Group<-cut(datC6$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)
plot(datC6$ChurnPerc)

#marital
summary(d_n$marital)
d_n%>%count(churn,levels=marital)%>%filter(churn==1)->datC7
datC7$N<-unclass(d_n%>%filter(marital%in%datC7$levels)%>%count(marital))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("marital",nrow(datC7))
datC7$Group<-cut(datC7$ChurnPerc,breaks = 2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC7

#ethnic
summary(d_n$ethnic)
d_n%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC8
datC8$N<-unclass(d_n%>%filter(ethnic%in%datC8$levels)%>%count(ethnic))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("ethnic",nrow(datC8))
datC8$Group<-cut(datC8$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)
datC8

#car_buy
summary(d_n$car_buy)
d_n%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC9
datC9$N<-unclass(d_n%>%filter(car_buy%in%datC9$levels)%>%count(car_buy))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("carbuy",nrow(datC9))
datC9$Group<-cut(datC9$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC9

#csa
#some levels with less churn rate
summary(d_n$csa)
d_n%>%count(churn,levels=csa)%>%filter(churn==1)->datC10
datC10$N<-unclass(d_n%>%filter(csa%in%datC10$levels)%>%count(csa))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("csa",nrow(datC10))
datC10$Group<-cut(datC10$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)


#retdays_n
summary(d_n$retdays_n)
d_n$retdays_n<-as.factor(d_n$retdays_n)
d_n%>%count(churn,levels=retdays_n)%>%filter(churn==1)->datC11
datC11$N<-unclass(d_n%>%filter(retdays_n%in%datC11$levels)%>%count(retdays_n))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("retdays_n",nrow(datC11))
datC11$Group<-cut(datC11$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)


#Use variables as factors
#age1
summary(d_n$age1)
d_n%>%count(churn,levels=age1)%>%filter(churn==1)->datC20
datC20$N<-unclass(d_n%>%filter(age1%in%datC20$levels)%>%count(age1))[[2]]
datC20$ChurnPerc<-datC20$n/datC20$N
datC20$Var.Name<-rep("age1",nrow(datC20))
datC20
datC20$Group<-cut(datC20$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)

#age2
summary(d_n$age2)
d_n%>%count(churn,levels=age2)%>%filter(churn==1)->datC12
datC12$N<-unclass(d_n%>%filter(age2%in%datC12$levels)%>%count(age2))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("age2",nrow(datC12))
datC12$Group<-cut(datC12$ChurnPerc,breaks=c(0,0.25,0.50,1),labels=c("Group1","Group2","Group3"),include.lowest = TRUE)

#models
summary(d_n$models)
d_n%>%count(churn,levels=models)%>%filter(churn==1)->datC13
datC13$N<-unclass(d_n%>%filter(models%in%datC13$levels)%>%count(models))[[2]]
datC13$ChurnPerc<-datC13$n/datC13$N
datC13$Var.Name<-rep("models",nrow(datC13))
datC13$Group<-cut(datC13$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)

#actvsubs
summary(d_n$actvsubs)
d_n%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC14
datC14$N<-unclass(d_n%>%filter(actvsubs%in%datC14$levels)%>%count(actvsubs))[[2]]
datC14$ChurnPerc<-datC14$n/datC14$N
datC14$Var.Name<-rep("actvsubs",nrow(datC14))
datC14$Group<-cut(datC14$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC14


#uniqsubs
summary(d_n$uniqsubs)
d_n%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC15
datC15$N<-unclass(d_n%>%filter(uniqsubs%in%datC15$levels)%>%count(uniqsubs))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("uniqsubs",nrow(datC15))
datC15$Group<-cut(datC15$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)
datC15

#forgntvl
summary(d_n$forgntvl)
d_n%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC16
datC16$N<-unclass(d_n%>%filter(forgntvl%in%datC16$levels)%>%count(forgntvl))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("forgntvl",nrow(datC16))
datC16$Group<-cut(datC16$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC16


#mtrcycle
summary(d_n$mtrcycle)
d_n%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC17
datC17$N<-unclass(d_n%>%filter(mtrcycle%in%datC17$levels)%>%count(mtrcycle))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("mtrcycle",nrow(datC17))
datC17$Group<-cut(datC17$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC17

#truck
summary(d_n$truck)
d_n%>%count(churn,levels=truck)%>%filter(churn==1)->datC18
datC18$N<-unclass(d_n%>%filter(truck%in%datC18$levels)%>%count(truck))[[2]]
datC18$ChurnPerc<-datC18$n/datC18$N
datC18$Var.Name<-rep("truck",nrow(datC18))
datC18$Group<-cut(datC18$ChurnPerc,2,labels=c("Group1","Group2"),include.lowest = TRUE)
datC18


#hnd_price
summary(d_n$hnd_price)
d_n%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC19
datC19$N<-unclass(d_n%>%filter(hnd_price%in%datC19$levels)%>%count(hnd_price))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("hndprice",nrow(datC19))
datC19$Group<-cut(datC19$ChurnPerc,3,labels=c("Group1","Group2","Group3"),include.lowest = TRUE)
datC19

csv_cat_var1=rbind(datC1,datC2,datC3,datC4,datC5,datC6,datC7,datC8,
                   datC9,datC10,datC11)
csv_cat_var2=rbind(datC12,datC13,datC14,datC15,datC16,datC17,datC18,
                   datC19)


write.csv(csv_cat_var1,"categorical variables1.csv",row.names = F)
write.csv(csv_cat_var2,"categorical variables2.csv",row.names = F)



#Removing variables with less churn rate
names(d_n)
d_n<-d_n[,-c(25,44)]

--------------------------------------------------------------------------------------------------
                            ###------DATA PREPARATION_OUTLIER TREATMENT--------###
--------------------------------------------------------------------------------------------------

###continuous variables_Boxplot###
----------------------------------
names(d_n)
summary(d_n)
str(d_n)

#Factor variables -asl_flag,prizm_social_one,area,refurb_new,hnd_webcap,
#marital,ethnic,age1,age2,models,hnd_price,actvsubs,uniqsubs,
#fogntvl,mtrcycle,trcuk,churn,car_buy,customer ID,retdays_n
names(d_n)
list<-names(d_n)

#Removing factor variables
list<-list[-c(25:42,50,51)]
list

#Outlier Plot
par(mar=rep(2,4))
par(mfrow=c(3,11))
for(i in 1:length(list))
{
  boxplot(d_n[,list[i]],main=list[i])
}
for(i in 1:length(list))
{
  plot(d_n[,list[i]],main=list[i])
}

#outlier treatment
for(i in 1:length(list)) 
{
  x=boxplot(d_n[,list[i]],main=list[i])
  out=x$out
  index=which(d_n[,list[i]]%in%x$out)
  d_n[index,list[i]]=mean(d_n[,list[i]],na.rm=T)
  rm(x)
  rm(out)
}

#After treatment
for(i in 1:length(list))
{
  boxplot(d_n[,list[i]],main=list[i])
}
for(i in 1:length(list))
{
  plot(d_n[,list[i]],main=list[i])
}
dev.off()

###Missing value treatment###
-----------------------------
summary(d_n)
names(d_n)

#Deletion_Missing Values
index1<-which(is.na(d_n[,c(1:5)]))
d_n<-d_n[-index1,]
summary(d_n)

index2<-which(is.na(d_n$change_mou))
d_n<-d_n[-index2,]

index3<-which(is.na(d_n$area))
d_n<-d_n[-index3,]

index4<-which(is.na(d_n$marital))
d_n<-d_n[-index4,]
summary(d_n)


#Mean imputation
d_n$avg6mou[is.na(d_n$avg6mou)]<-mean(d_n$avg6mou,na.rm=T)
d_n$avg6qty[is.na(d_n$avg6qty)]<-mean(d_n$avg6qty,na.rm=T)
d_n$hnd_price[is.na(d_n$hnd_price)]<-mean(d_n$hnd_price,na.rm=T)
summary(d_n)

#Creating a separate category "Missing"_Factor Variables
#prizm_social_one
summary(d_n$prizm_social_one)
str(d_n$prizm_social_one)
d_n$prizm_social_one_n<-ifelse(is.na(d_n$prizm_social_one),"Missing",ifelse(d_n$prizm_social_one=="R"|d_n$prizm_social_one=="T","Group3","Group1"))
d_n$prizm_social_one_n<-as.factor(d_n$prizm_social_one_n)
d_n$prizm_social_one_n<-factor(d_n$prizm_social_one_n,labels = c("Group1","Group3","Missing"))
summary(d_n$prizm_social_one_n)

names(d_n)
d_n<-d_n[,-26]

#hnd_webcap
#Creating separate category "Missing"
summary(d_n$hnd_webcap)
d_n$hnd_webcap_n<-ifelse(is.na(d_n$hnd_webcap),"Missing",ifelse(d_n$hnd_webcap=="WC","Group3",ifelse(d_n$hnd_webcap=="UNKW","Group1","Group2")))
d_n$hnd_webcap_n<-as.factor(d_n$hnd_webcap_n)
d_n$hnd_webcap_n<-factor(d_n$hnd_webcap_n,labels = c("Group1","Group2","Group3","Missing"))
summary(d_n$hnd_webcap_n)

names(d_n)
d_n<-d_n[,-28]

#Churn rate -after imputation
table(d$churn)/nrow(d)
table(d_n$churn)/nrow(d_n)


#Dummy variable creation_Factor Varibales
#age1
str(d_n$age1)
d_n$age1_n<-ifelse(d_n$age1==0,"Default",ifelse(d_n$age1<=30,"Young",
                                                ifelse(d_n$age1>30&d_n$age1<=55,"Middle","Old")))

str(d_n$age1_n)
d_n$age1_n<-as.factor(d_n$age1_n)
summary(d_n$age1_n)
names(d_n)
d_n<-d_n[,-30]


#age2
summary(d_n$age2)
d_n$age2_n<-ifelse(d_n$age2==0,"Default",ifelse(d_n$age2<=30,"Young",
                                                ifelse(d_n$age2>30&d_n$age2<=55,"Middle","Old")))
d_n$age2_n<-as.factor(d_n$age2_n)
summary(d_n$age2_n)
names(d_n)
d_n<-d_n[,-30]

#models
summary(d_n$models)
str(d_n$models)
d_n$models_n<-ifelse(d_n$models=="14","Group2","Group1")
d_n$models_n<-as.factor(d_n$models_n)
summary(d_n$models_n)
str(d_n$models_n)

names(d_n)
d_n<-d_n[,-30]


#hnd_price
summary(d_n$hnd_price)
str(d_n$hnd_price)
d_n$hnd_price_n<-ifelse(d_n$hnd_price%in%c("105.083038078331","199.9899902","299.9899902","249.9899902"),"Group1",
                        ifelse(d_n$hnd_price%in%c("9.989997864","39.98999023","179.9899902","239.9899902"),"Group3","Group2"))
d_n$hnd_price_n<-as.factor(d_n$hnd_price_n)
d_n$hnd_price_n<-factor(d_n$hnd_price_n,labels = c("Group1","Group2","Group3"))
summary(d_n$hnd_price_n)

names(d_n)
d_n<-d_n[,-30]

#actvsubs
summary(d_n$actvsubs)
str(d_n$actvsubs)
d_n$actvsubs_n<-ifelse(d_n$actvsubs%in%c("5","6"),"Group1","Group2")
d_n$actvsubs_n<-as.factor(d_n$actvsubs_n)
d_n$actvsubs_n<-factor(d_n$actvsubs_n,labels = c("Group1","Group2"))
str(d_n$actvsubs_n)
summary(d_n$actvsubs_n)

names(d_n)
d_n<-d_n[,-30]

#uniqsubs
summary(d_n$uniqsubs)
str(d_n$uniqsubs)
d_n$uniqsubs_n<-ifelse(d_n$uniqsubs=="9","Group2",ifelse(d_n$uniqsubs=="13","Group3","Group1"))
d_n$uniqsubs_n<-as.factor(d_n$uniqsubs_n)
d_n$uniqsubs_n<-factor(d_n$uniqsubs_n,labels=c("Group1","Group2","Group3"))
str(d_n$uniqsubs_n)
summary(d_n$uniqsubs_n)

names(d_n)
d_n<-d_n[,-30]


#forgntvl
summary(d_n$forgntvl)
str(d_n$forgntvl)
d_n$forgntvl_n<-ifelse(d_n$forgntvl=="1","Group1","Group2")
d_n$forgntvl_n<-as.factor(d_n$forgntvl_n)
d_n$forgntvl_n<-factor(d_n$forgntvl_n,labels = c("Group1","Group2"))
str(d_n$forgntvl_n)
summary(d_n$forgntvl_n)

names(d_n)
d_n<-d_n[,-30]

#mtrcycle
summary(d_n$mtrcycle)
str(d_n$mtrcycle)
d_n$mtrcycle_n<-ifelse(d_n$mtrcycle=="0","Group1","Group2")
d_n$mtrcycle_n<-as.factor(d_n$mtrcycle_n)
d_n$mtrcycle_n<-factor(d_n$mtrcycle_n,labels = c("Group1","Group2"))
summary(d_n$mtrcycle_n)

names(d_n)
d_n<-d_n[,-30]

#truck
summary(d_n$truck)
str(d_n$truck)
d_n$truck_n<-ifelse(d_n$truck=="1","Group1","Group2")
d_n$truck_n<-as.factor(d_n$truck_n)
d_n$truck_n<-factor(d_n$truck_n,labels = c("Group1","Group2"))
str(d_n$truck_n)
summary(d_n$truck_n)

names(d_n)
d_n<-d_n[,-30]

#area
summary(d_n$area)
str(d_n$area)
d_n$area_n<-ifelse(d_n$area%in%c("SOUTH FLORIDA AREA","NORTHWEST/ROCKY MOUNTAIN AREA"),"Group3",
                   ifelse(d_n$area%in%c("DALLAS AREA","LOS ANGELES AREA",
                                        "CHICAGO AREA","NEW YORK CITY AREA","SOUTHWEST AREA",
"PHILADELPHIA AREA","NORTH FLORIDA AREA","CALIFORNIA NORTH AREA","NEW ENGLAND AREA"),"Group2","Group1"))
d_n$area_n<-as.factor(d_n$area_n)
summary(d_n$area_n)
str(d_n$area_n)

names(d_n)
d_n<-d_n[,-26]

#refurb_new
summary(d_n$refurb_new)
str(d_n$refurb_new)
d_n$refurb_new_n<-ifelse(d_n$refurb_new=="N","Group1","Group2")
d_n$refurb_new_n<-as.factor(d_n$refurb_new_n)
d_n$refurb_new_n<-factor(d_n$refurb_new_n,labels=c("Group1","Group2"))
summary(d_n$refurb_new_n)
str(d_n$refurb_new_n)

names(d_n)
d_n<-d_n[,-26]


#marital
summary(d_n$marital)
str(d_n$marital)
d_n$marital_n<-ifelse(d_n$marital=="M"|d_n$marital=="S","Group1","Group2")
d_n$marital_n<-as.factor(d_n$marital_n)
d_n$marital_n<-factor(d_n$marital_n,labels = c("Group1","Group2"))
summary(d_n$marital_n)
str(d_n$marital_n)

names(d_n)
d_n<-d_n[,-26]

#ethnic
summary(d_n$ethnic)
str(d_n$ethnic)
d_n$ethnic_n<-ifelse(d_n$ethnic=="C","Group1",ifelse(d_n$ethnic%in%c("M","N","P","X","Z"),"Group2","Group3"))
d_n$ethnic_n<-as.factor(d_n$ethnic_n)
d_n$ethnic_n<-factor(d_n$ethnic_n,labels = c("Group1","Group2","Group3"))
summary(d_n$ethnic_n)
str(d_n$ethnic_n)

names(d_n)
d_n<-d_n[,-26]


#car_buy
summary(d_n$car_buy)
str(d_n$car_buy)
d_n$car_buy_n<-ifelse(d_n$car_buy=="New","Group1","Group2")
d_n$car_buy_n<-as.factor(d_n$car_buy_n)
d_n$car_buy_n<-factor(d_n$car_buy_n,labels = c("Group1","Group2"))
summary(d_n$car_buy_n)
str(d_n$car_buy_n)

names(d_n)
d_n<-d_n[,-27]

#summary(d_n$asl_flag)
summary(d_n$asl_flag)
str(d_n$asl_flag)
d_n$asl_flag_n<-ifelse(d_n$asl_flag=="N","Group2","Group1")
d_n$asl_flag_n<-as.factor(d_n$asl_flag_n)
d_n$asl_flag_n<-factor(d_n$asl_flag_n,labels = c("Group1","Group2"))
summary(d_n$asl_flag_n)
str(d_n$asl_flag_n)

names(d_n)
d_n<-d_n[,-25]


-------------------------------------------------------------------------------------------------
                       ###-----MODEL BUILDING_LOGISTIC REGRESSION-----###
-------------------------------------------------------------------------------------------------
#Spilitting the Dataset
set.seed(200)
index5<-sample(nrow(d_n),0.70*nrow(d_n),replace=F)
train<-d_n[index5,]
test<-d_n[-index5,]

#Churn rate checking
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)


names(train)

#Removing customer ID and building the logistic regression model
mod<-glm(churn~.,data=train[,-33],family="binomial")
summary(mod)
step(mod,direction = "both")

###Dummy creation for significant factor variables###
-----------------------------------------------------
#asl_flag
summary(d_n$asl_flag_n)
train$asl_flag_n_Group2<-ifelse(train$asl_flag_n=="Group2",1,0)
test$asl_flag_n_Group2<-ifelse(test$asl_flag_n=="Group2",1,0)


#area
summary(d_n$area_n)
train$area_n_Group2<-ifelse(train$area_n=="Group2",1,0)
test$area_n_Group2<-ifelse(test$area_n=="Group2",1,0)

train$area_n_Group3<-ifelse(train$area_n=="Group3",1,0)
test$area_n_Group3<-ifelse(test$area_n=="Group3",1,0)

#refurbnew
summary(train$refurb_new_n)
train$refurb_new_n_Group2<-ifelse(train$refurb_new_n=="Group2",1,0)
test$refurb_new_n_Group2<-ifelse(test$refurb_new_n=="Group2",1,0)

#ethnic
summary(train$ethnic_n)
train$ethnic_n_Group2<-ifelse(train$ethnic_n=="Group2",1,0)
test$ethnic_n_Group2<-ifelse(test$ethnic_n=="Group2",1,0)

train$ethnic_n_Group3<-ifelse(train$ethnic_n=="Group3",1,0)
test$ethnic_n_Group3<-ifelse(test$ethnic_n=="Group3",1,0)


#hnd_price
summary(train$hnd_price_n)
train$hnd_price_n_Group2<-ifelse(train$hnd_price_n=="Group2",1,0)
test$hnd_price_n_Group2<-ifelse(test$hnd_price_n=="Group2",1,0)

train$hnd_price_n_Group3<-ifelse(train$hnd_price_n=="Group3",1,0)
test$hnd_price_n_Group3<-ifelse(test$hnd_price_n=="Group3",1,0)

#actvsubs
summary(train$actvsubs_n)
train$actvsubs_n_Group2<-ifelse(train$actvsubs_n=="Group2",1,0)
test$actvsubs_n_Group2<-ifelse(test$actvsubs_n=="Group2",1,0)

#truck
summary(train$truck_n)
train$truck_n_Group2<-ifelse(train$truck_n=="Group2",1,0)
test$truck_n_Group2<-ifelse(test$truck_n=="Group2",1,0)

#prizm_social_one_n
summary(train$prizm_social_one_n)
train$prizm_social_one_n_Group3<-ifelse(train$prizm_social_one_n=="Group3",1,0)
test$prizm_social_one_n_Group3<-ifelse(test$prizm_social_one_n=="Group3",1,0)


#marital
summary(train$marital_n)
train$marital_n_Group2<-ifelse(train$marital_n=="Group2",1,0)
test$marital_n_Group2<-ifelse(test$marital_n=="Group2",1,0)

#age1_n
summary(train$age1_n)
train$age1_n_old<-ifelse(train$age1_n=="Old",1,0)
test$age1_n_old<-ifelse(test$age1_n=="Old",1,0)

train$age1_n_Middle<-ifelse(train$age1_n=="Middle",1,0)
test$age1_n_Middle<-ifelse(test$age1_n=="Middle",1,0)

train$age1_n_Young<-ifelse(train$age1_n=="Young",1,0)
test$age1_n_Young<-ifelse(test$age1_n=="Young",1,0)

#age2
summary(train$age2_n)
train$age2_n_old<-ifelse(train$age2_n=="Old",1,0)
test$age2_n_old<-ifelse(test$age2_n=="Old",1,0)


names(train)
###Rerunning the model with Significant Variables###
----------------------------------------------------
mod1<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_blk_Mean+
            drop_vce_Range+mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+ovrrev_Mean+
            avgmou+avg3qty+avgqty+avg6mou+adjmou+totrev+retdays_n+complete_Mean+prizm_social_one_n_Group3+
            age1_n_Middle+age1_n_old+age1_n_Young+age2_n_old+area_n_Group2+area_n_Group3+
            refurb_new_n_Group2+marital_n_Group2+ethnic_n_Group2+ethnic_n_Group3+hnd_price_n_Group2+hnd_price_n_Group3+actvsubs_n_Group2+
            truck_n_Group2+asl_flag_n_Group2,data=train,family="binomial")
summary(mod1)


###actvsubs_n_Group2 is not significant. Removing that variable and running the model###
----------------------------------------------------------------------------------------
mod2<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_blk_Mean+drop_vce_Range+
            mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+ovrrev_Mean+avgmou+
            avg3qty+avgqty+avg6mou+adjmou+totrev+retdays_n+complete_Mean+prizm_social_one_n_Group3+
            area_n_Group2+area_n_Group3+refurb_new_n_Group2+marital_n_Group2+ethnic_n_Group2+
            ethnic_n_Group3+hnd_price_n_Group2+hnd_price_n_Group3+truck_n_Group2+
            asl_flag_n_Group2+age1_n_Middle+age1_n_old+age2_n_old,data=train,family="binomial")
summary(mod2)


###marital_n_Group2 is not significant. Removing that variable and running the model###
----------------------------------------------------------------------------------------
mod3<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_blk_Mean+drop_vce_Range+
            mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+ovrrev_Mean+avgmou+
            avg3qty+avgqty+avg6mou+adjmou+totrev+retdays_n+complete_Mean+prizm_social_one_n_Group3+
            area_n_Group2+area_n_Group3+refurb_new_n_Group2+ethnic_n_Group2+
            ethnic_n_Group3+hnd_price_n_Group2+hnd_price_n_Group3+truck_n_Group2+
            asl_flag_n_Group2+age1_n_Middle+age1_n_old+age2_n_old,data=train,family="binomial")
summary(mod3)

----------------------------------------------------------------------------------------
                  ###-----Model Dignostics-----###
----------------------------------------------------------------------------------------
#All the variables are significant. Checking for Multicolinearity

library(car)
vif(mod3)
#vif should be less than 5. 
#6 variables having vif >5.Should be removed from the model.
#Variables are mou_Mean,avgmou,avg3qty,avg6mou,ethnic_n_Group2,ethnic_n_Group3
mod4<-glm(churn~totmrc_Mean+rev_Range+mou_Range+change_mou+drop_blk_Mean+drop_vce_Range+
            mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+ovrrev_Mean+
            avgqty+adjmou+totrev+retdays_n+complete_Mean+prizm_social_one_n_Group3+
            area_n_Group2+area_n_Group3+refurb_new_n_Group2+
            hnd_price_n_Group2+hnd_price_n_Group3+truck_n_Group2+
            asl_flag_n_Group2+age1_n_Middle+age1_n_old+age2_n_old,data=train,family="binomial")
summary(mod4)


#adjmou is insignificant. Removing that variable and rerunning the model
mod5<-glm(churn~totmrc_Mean+rev_Range+mou_Range+change_mou+drop_blk_Mean+drop_vce_Range+
            mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+ovrrev_Mean+
            avgqty+totrev+retdays_n+complete_Mean+prizm_social_one_n_Group3+
            area_n_Group2+area_n_Group3+refurb_new_n_Group2+
            hnd_price_n_Group2+hnd_price_n_Group3+truck_n_Group2+
            asl_flag_n_Group2+age1_n_Middle+age1_n_old+age2_n_old,data=train,family="binomial")
summary(mod5)

#All variables are significant. Checking the multicolinearity
vif(mod5)

#All variables are significant and the vif values are <5 shows there is no multicolinearity.
#This model can be considered as final

confint(mod5)


----------------------------------------------------------------------------------------
                          ###-----MODEL TESTING-----###
----------------------------------------------------------------------------------------
#Predicting the probability of customer churn
test$pred<-predict(mod5,type = "response",newdata=test)
head(test$pred)

#Assuming cutoff probability as per churn rate
table(d_n$churn)/nrow(d_n)


#choosing cutoff value according to kappa
library(caret)
s<-seq(0.2,0.5,0.01)
n<-1
a<-as.vector(length(s))
for (i in s){
  print(i)
  test$result<-ifelse(test$pred>i,1,0)
  a[n]<-confusionMatrix(table(test$result,test$churn))$overall[2]
  print(n)
  n=n+1
}

max(a)
#Maximum kappa is related to cutoff 0.2380871
#Model is predicting the best at this cutoff value
pred1<-ifelse(test$pred>=0.2380871,1,0)
table(pred1)

---------------------------------------------------------------------------------------
                          ###-----PREDICTION QUALITY CHECK----###
---------------------------------------------------------------------------------------
#kappa matrix
library(irr)
kappa2(data.frame(test$churn,pred1))

#confusion matrix
confusionMatrix(pred1,test$churn,positive="1")
confusionMatrix(table(pred1,test$churn))


#ROCR Curve
library(ROCR)
pred2<-prediction(pred1,test$churn)
perf<-performance(pred2,"tpr","fpr")
plot(perf,col="yellow")
abline(0,1,lty=10,col="green")
auc<-performance(pred2,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

library(pROC)
roc_obj<-roc(test$churn,pred1)
roc_obj
plot.roc(roc_obj,main="ROC")
coords(roc_obj,"best","threshold")
auc(roc_obj)
#AUC is 0.5839 which is more than 0.5
#Model seeems to be ok


#Gains chart
library(gains)
test$churn<-as.numeric(test$churn)
gains(test$churn,predict(mod5,type="response",newdata=test),groups=10)
#chart shows top30% probabilities contain 42.4% customers that are likely churn

test$prob<-predict(mod5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
#Top 40% lies between 0.2504866 and 0.2719415
#This can be used to extract the customer data who are likely to churn


-----------------------------------------------------------------------------------------
                         ###----BUSINESS QUESTIONS----###
-----------------------------------------------------------------------------------------
#Topline questions
------------------
  
#1. What are the top five factors driving likelihood of churn at Mobicom?
head(sort(abs(mod5$coefficients),decreasing = T),10)
summary(mod5)
#Factor                  coefficient
#1.retdays_n1             0.6574813
#2.asl_flag_n_Group2      0.4734461
#3.hnd_price_n_Group3     0.4164027
#4.area_n_Group3          0.3644660
#5.hnd_price_n_Group2     0.2708792

#Special offers should be given to customers who makes retention calls 
#Special plans should be rolled out for customers located in NORTHWEST/ROCKY MOUNTAIN AREA
#and SOUTH FLORIDA AREA

#2. Validation of survey findings
#a) Whether "cost and billing" and "network and service quality" are important factors
#influencing churn behaviour?
#Variables affects cost and billing are
------------------------------------------
#1. totmrc_Mean - Monthly Recurring Charge is the base cost of the calling plan regardless of actual minutes used.
#2. rev_range - Range of revenue (charge amount)
#3. ovrrev_Mean - DATOVR_MEAN + VCEOVR_MEAN.Mean overage revenue is the sum of data and voice overage revenues.
#4. totrev-Total revenue
  
#Beta Coefficients
mod5$coefficients
#totmrc_Mean :(-)0.0063960681 
#rev_range   :0.0019711383
#ovrrev_Mean :0.0066317704
#totrev      :0.0002317941 

#Above beta values shows that unit increase of those variables has no impact on the 
#churn. So cost and billing is not an important factor influencing churn behaviour

#Variables affects network and service quality are
#1. mou_Range -Range of number of minutes of use
#2. change_mou -Percentage change in monthly minutes of use vs previous three month average
#3. drop_blk_Mean - Mean number of dropped or blocked calls
#4. drop_vce_Range- Range of number of dropped (failed) voice calls
#5. mou_opkv_Range - Range of unrounded minutes of use of off-peak voice calls
#6. iwylis_vce_Mean - Mean number of inbound wireless to wireless voice calls
#7. avgqty - Average monthly number of calls over the life of the customer
#8. avg6mou - Average monthly minutes of use over the previous six months
#9. adjmou - Billing adjusted total minutes of use over the life of the customer
#10. retdays_n - Number of days since last retention call
#11. complete_Mean - COMP_DAT_MEAN + COMP_VCE_MEAN. Mean number of completed calls.

#Beta Coefficients
mod5$coefficients
#mou_Range         :0.0002562910 
#change_mou        :(-)0.0006289187
#drop_blk_Mean     :0.0071641598
#drop_vce_Range    :0.0180034620 
#mou_opkv_Range    :(-)0.0011672176
#iwylis_vce_Mean   :(-)0.0126706556
#avgqty            :0.0008278967
#retdays_n         :0.6574813402
#complete_Mean     :(-)0.0020419077

#Above values shows that retdays_n is an important factor influencing the churn behaviour. 
#As the number of days increases after the customer making the retention call, chances of churning is high.
#Special attention has to be given for those customers by providing special offers


#2b. Are the data usage connectivity issues turning out to be costly?In other words, is it leading to churn?
# Varaibles related to data usage connectivity are
#1. comp_dat_Mean :Mean number of completed data calls
#2. plcd_dat_Mean :Mean number of attempted data calls placed
#3. opk_dat_Mean  :Mean number of off-peak data calls
#4. blck_dat_Mean :Mean number of blocked (failed) data calls
#5. datovr_Mean   :Mean revenue of data overage
#6. datovr_Range  :Range of revenue of data overage
#7. drop_dat_Mean :Mean number of dropped (failed) data calls

quantile(d$plcd_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.85,0.90,1))
#Only 10 to 15% of the customers making the data calls 
#Work towards achiving more customers to use internet and provide quality data connectivity to reduce the churn
#There is no enough usable data for those variables to show the impact on churn


#3. Would your recommend rate plan migration as a proactive retention strategy?
#OVERREV_MEAN = DATOVR_MEAN + VCEOVR_MEAN - Mean overage revenue is the sum of data and voice overage revenues.
#Beta coefficient of ovrrev_Mean is 0.0066317704. The impact of this variable is less on the churn behaviour
#This may be the concern for individual customers but overall migration plan might not help much.


#4. What would be your recommendation on how to use this churn model for prioritisation
# of customers for a proactive retention compaigns in the future?
gains(test$churn,predict(mod5,type="response",newdata=test),groups=10)
#From this chart we understand that top 20% of the probabilites contain 29.2% customers that are likely to churn 

#customers with high churn rate
test$prob<-predict(mod5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
#Top 20%lies between 0.3411187  and 0.7532860

#Applying cutoff to predict the churn
pred4<-predict(mod5,type="response",newdata=test)
pred4<-ifelse(pred4>0.3411187,1,0)
table(pred4,test$churn)

Targeted<-test[test$prob>0.3411187&test$prob<=0.7532860&test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)

write.csv(Targeted,"Targeted_customers",row.names=F)
#Extract the target list using customer ID.

#5. What would be the target segments for proactive retention compaigns?
#Mobicom would like to save their high revenue customers besides manging churn
#Given a budget constraint of a contact list of 20% of subscriber pool which
#subscribers should prioritised if "revenue saves" is also a priority besides
#controlling churn
#controlling churn is the primary objective and revenue saves is the secondary objective
pred5<-predict(mod5,type="response",newdata=test)
test$prob<-predict(mod5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6<-ifelse(pred5<0.20,"Low_score",ifelse(pred5>=0.20&pred5<0.30,"Medium_score","High_score"))
table(pred6,test$churn)

str(test$totrev)
summary(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue<-ifelse(test$totrev<670.66,"Low_revenue",ifelse(test$totrev>=670.66&test$totrev<1034.281,"Medium_Revenue","High_Revenue"))
table(Revenue)
table(pred6,Revenue)

test$prob_levels<-ifelse(pred5<0.20,"Low_score",ifelse(pred5>=0.20&pred5<0.30,"Medium_score","High_score"))
test$Revenue_levels<-ifelse(test$totrev<670.66,"Low_revenue",ifelse(test$totrev>=670.66&test$totrev<1034.281,"Medium_Revenue","High_Revenue"))
table(test$prob_levels,test$Revenue_levels)
#This table can be used to select the level of customers are to be targeted
#Target list can be extracted as follows

Targeted1<-test[(test$prob_levels=="High_score"&test$Revenue_levels=="High_Revenue")|
                (test$prob_levels=="High_score"&test$Revenue_levels=="Medium_Revenue")|
                  (test$prob_levels=="Medium_score"&test$Revenue_levels=="High_Revenue"),"Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)
write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)

