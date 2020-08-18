rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
setwd("D:/Ankit Work/wind data")
A<- read_excel("KAVALDHARA_JUN_1_TO_JUL_17.xlsx",sheet = "Sheet1")
###### daily #############
wind <- A[, c(1,3)]
n <- 96;
act_avg=aggregate(wind,list(rep(1:(nrow(wind)%/%n+1),each=n,len=nrow(wind))),mean)[-1]
####### date series
start_date<-as.Date("2020/6/1")
dat=seq(start_date,by='day',length.out=47)
act_avg=cbind(dat,act_avg)
#######
N25<- A[,c(1,4)]
N25_avg=aggregate(N25,list(rep(1:(nrow(N25)%/%n+1),each=n,len=nrow(N25))),mean)[-1]
N25_avg=cbind(dat,N25_avg)

AN12<- A[,c(1,5)]
AN12_avg=aggregate(AN12,list(rep(1:(nrow(AN12)%/%n+1),each=n,len=nrow(AN12))),mean)[-1]
AN12_avg=cbind(dat,AN12_avg)

CWI<- A[,c(1,6)]
CWI_avg=aggregate(CWI,list(rep(1:(nrow(CWI)%/%n+1),each=n,len=nrow(CWI))),mean)[-1]
CWI_avg=cbind(dat,CWI_avg)

ECM9<- A[,c(1,7)]
ECM9_avg=aggregate(ECM9,list(rep(1:(nrow(ECM9)%/%n+1),each=n,len=nrow(ECM9))),mean)[-1]
ECM9_avg=cbind(dat,ECM9_avg)
########
datalist<- list(act_avg,N25_avg,AN12_avg,CWI_avg,ECM9_avg)
mymerge<- function(df1,df2){
  merge(df1,df2,by='dat')
}
library(dplyr)
library('tidyverse')
df_new<-Reduce(mymerge,datalist)
df2=df_new[c(-2,-4,-6,-8,-10)]
write_xlsx(df2,"D:/Ankit Work/wind data/daily.xlsx")
#######
library(dplyr)
new_df=merge(act_avg,N25_avg,AN12_avg,CWI_avg,ECM9_avg,by=1)
df=do.call("rbind", list(act_avg,N25_avg,AN12_avg,CWI_avg,ECM9_avg))

############ hourly #############
n<- 4;
wind_avg=aggregate(wind,list(rep(1:(nrow(wind)%/%n+1),each=n,len=nrow(wind))),mean)[-1]
tt<- seq(as.POSIXct("2020-06-01 00:00:00"), as.POSIXct("2020-07-17 23:00:00"), by="hour")
wind_avg=cbind(tt,wind_avg)

N25<- A[,c(1,4)]
N25_avg=aggregate(N25,list(rep(1:(nrow(N25)%/%n+1),each=n,len=nrow(N25))),mean)[-1]
N25_avg=cbind(tt,N25_avg)

AN12<- A[,c(1,5)]
AN12_avg=aggregate(AN12,list(rep(1:(nrow(AN12)%/%n+1),each=n,len=nrow(AN12))),mean)[-1]
AN12_avg=cbind(tt,AN12_avg)

CWI<- A[,c(1,6)]
CWI_avg=aggregate(CWI,list(rep(1:(nrow(CWI)%/%n+1),each=n,len=nrow(CWI))),mean)[-1]
CWI_avg=cbind(tt,CWI_avg)

ECM9<- A[,c(1,7)]
ECM9_avg=aggregate(ECM9,list(rep(1:(nrow(ECM9)%/%n+1),each=n,len=nrow(ECM9))),mean)[-1]
ECM9_avg=cbind(tt,ECM9_avg)
########
datalist<- list(wind_avg,N25_avg,AN12_avg,CWI_avg,ECM9_avg)
mymerge<- function(df1,df2){
  merge(df1,df2,by='tt')
}

df_new<-Reduce(mymerge,datalist)
df2=df_new[c(-2,-4,-6,-8,-10)]
write_xlsx(df2,"D:/Ankit Work/wind data/kaval_hourly.xlsx")
#######

new_df=merge(act_avg,N25_avg,AN12_avg,CWI_avg,ECM9_avg,by=1)
df=do.call("rbind", list(act_avg,N25_avg,AN12_avg,CWI_avg,ECM9_avg))

