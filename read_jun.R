rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
setwd("D:/Ankit Work/work assigned")
A<- read_excel("April WMS REPORT.xlsx",sheet = "WMS REPORT")
###################################################

#reading parameters from excel
Date <- data.frame(TIME=A$TimeStamp)
Irrad <- A[, c(1,3)]
DD<-round_date(Date$TIME,unit="1 minute")
Date$TIME <- DD
#TT15 <- data.frame(TIME=seq.POSIXt(from=Date$TIME[1],to=Date$TIME[nrow(Date)],by="1 min"))  
#creating minute wise time for April
dat = data.frame(TIME=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*43199, by=60),
                 count=sample(1:50, 43200, replace=TRUE))
dat$by15 = cut(dat$TIME, breaks="15 min")
newTime<- data.frame(TIME=dat$by15)
time_min=data.frame(TimeStamp=dat$TIME)
##########
library(dplyr)
a=left_join(time_min,Irrad, by = "TimeStamp")
#write_xlsx(a,"D:/Ankit Work/work assigned/file name.xlsx")
####### mean of every 15 values (15min average)
df=a
df <- df[-c(1:330), ]  #delete rows 
n <- 15;
aver=aggregate(df,list(rep(1:(nrow(df)%/%n+1),each=n,len=nrow(df))),mean)[-1];
write_xlsx(aver,"D:/Ankit Work/work assigned/15min.xlsx")

#########

E <- E[(!duplicated(E$TIME)),]
E$REPPER15 <- rep(1:(nrow(E)/15), each = 15)
GHI_15min <- tapply(E$GHI,E$REPPER15 ,mean,na.rm=T)

#Irrad <- A[, c("Irradiation GHI","TimeStamp")]


Irrad <- data.frame(GHI=A$Irradiation GHI)
