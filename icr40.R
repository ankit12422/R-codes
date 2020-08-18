rm(list=ls())
setwd("D:/Question")
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library(gtools)
setwd("D:/Ankit Work/wms/WMS new/WMS Data July-20 BKN Site")

A<- read.csv("ICR-40 WMS July-20 Bikaner 250MW.csv",header = FALSE)[,-c(1)]
Datetime <- seq(ymd_hm('2020-07-01 00:00'),ymd_hm('2020-07-31 23:59'), by = '1 mins')
tim1<- data.frame(Datetime)
A2<- cbind(tim1,A)

############### data cut from 6am to 7:15 pm
E <- df
DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=31)
H <- E[complete.cases(DAYTIME_CUTTER),]
####### converting data 1 min to 15 minute
A1<- read_excel("WMS_july_cleaned_1min.xlsx",sheet = "Sheet1")
df<- A1
n<-15
df2<- aggregate(df,list(rep(1:(nrow(df)%/%n+1),each=n,len=nrow(df))),mean)[-1];
#write_xlsx(df2,"ALL_DATA_mean_15min.xlsx")
########## csv read for GHI
setwd("D:/Ankit Work/wms/WMS new/WMS Data July-20 BKN Site/Portal/GHI")
LIST_OF_FILES <- list.files(pattern=".csv")

DATUM <- list()
for (i in 1:length(LIST_OF_FILES))
{
  A<- read.csv(LIST_OF_FILES[i],header = FALSE)[-c(1,2,3),c(1,3)]
  DATUM [[i]] <- A
  rm(A)
}
B <- do.call(rbind,DATUM)
Datetime <- seq(ymd_hm('2020-07-01 00:00'),ymd_hm('2020-07-31 23:45'), by = '15 mins')
tim1<- data.frame(Datetime)
Final<- cbind(tim1,B)
colnames(Final)[2]<- "Time_blocks"
colnames(Final)[3]<- "Actual_GHI"
GHI<- Final
######## csv read for Power
setwd("D:/Ankit Work/wms/WMS new/WMS Data July-20 BKN Site/Portal/Power")
Files_list<- list.files(pattern = ".csv")
DATUM<- list()
for(i in 1:length(Files_list))
{
  A<- read.csv(Files_list[i],header=FALSE)[-c(1,2,3),c(1,3)]
  DATUM[[i]]<- A
  rm(A)
}
B<- do.call(rbind,DATUM)
Datetime<- seq(ymd_hm("2020-07-01 00:00"),ymd_hm("2020-07-31 23:45"), by="15 mins")
tim1<- data.frame(Datetime)
Final<- cbind(tim1,B)
colnames(Final)[2]<- "Time_blocks"
colnames(Final)[3]<- "SCADA_Power"
Power<- Final
##### Binding GHI and power
df<- data.frame(cbind(GHI$Time_blocks,GHI$Actual_GHI,Power$SCADA_Power))
df2<- cbind(tim1,df)
colnames(df2)[2]<- "Time_blocks"
colnames(df2)[3]<- "Actual_GHI"
colnames(df2)[4]<- "SCADA_Power"
############
E <- df2
DAYTIME_CUTTER <- rep((c(rep(NA,times=25),rep(1,times=53),rep(NA,18))),times=30)
H <- E[complete.cases(DAYTIME_CUTTER),]
GHI_pow<- H
