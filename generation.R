rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
#library(hydroGOF)
library(gtools)
setwd("D:/Ankit Work/work assigned/GHI_50hertz/generation")
A1<- read_excel("alldays.xlsx",sheet = "April")
A2<- read_excel("alldays.xlsx",sheet = "May")
A3<- read_excel("alldays.xlsx",sheet = "June")
df=A1;


##### daytime cutter #############
E <- A1
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=53),rep(NA,19))),times=30)
H1<- E[complete.cases(DAYTIME_CUTTER),]
H2<- E[complete.cases(DAYTIME_CUTTER),]
H3 <- E[complete.cases(DAYTIME_CUTTER),]

gen=rbind(H1,H2,H3)
#write_xlsx(g1,"D:/Ankit Work/work assigned/GHI_50hertz/GHI_15min_84days.xlsx")

################## GHI 15 min mean


A1<- read_excel("ALL_DATA_cleaned_1min_DAYTIME.xlsx",sheet = "Sheet1")


A2<- read_excel("ICR_40_10_27_0.xlsx",sheet = "May")
A3<- read_excel("ICR_40_10_27_0.xlsx",sheet = "June")

dat = data.frame(time=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*116639, by=60),
                 count=sample(1:50, 116640, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
E<- newdat
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=81)
tim15 <- E[complete.cases(DAYTIME_CUTTER),]
GHI_whole<- smartbind(A1,A2,A3)
df<- GHI_whole[-c(1)]

GHI<- data.frame(GHI_whole$Irradiation.GHI)
###########
df1<- A1[-c(2,4,5,12,13,14,15,16,17,18)];
df2<- A2[-c(1,2,4,5,12,13,14,15,16,17,18)];
df3<- A3[-c(1,2,4,5,12,13,14,15,16,17,18)];

df1<- df1[-c(1)]
n <- 15;
g1<- aggregate(df1,list(rep(1:(nrow(df1)%/%n+1),each=n,len=nrow(df1))),mean)[-1];

g2<- aggregate(df1,list(rep(1:(nrow(df1)%/%n+1),each=n,len=nrow(df))),mean)[-1];
g3<- aggregate(df1,list(rep(1:(nrow(df)%/%n+1),each=n,len=nrow(df))),mean)[-1];
#################
# Do mean of every 15 rows of GHI
df=A1[c(2)]
n <- 15;
min15=aggregate(df,list(rep(1:(nrow(df)%/%n+1),each=n,len=nrow(df))),mean)[-1];
E <- GHI[,c(1,2)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=1),rep(1,times=795),rep(NA,0))),times=81)
GHI_act <- E[complete.cases(DAYTIME_CUTTER),]
########
length <- nrow(df1)/15
REPPER <- rep(1:4293,each=15)
View(REPPER)

IRR_15 <- tapply(df1$Irradiation.GHI,REPPER,mean,na.rm=T)
###########
gen3= smartbind(df2,df3,df4)

############ Power Generation ##############
setwd("D:/Ankit Work/work assigned/GHI_50hertz/generation")
A1<- read_excel("84days_all_parameter.xlsx",sheet = 1)
##################  GHI according to time format Mean and percentile
POW_TREND <- data.frame(Time=A1$TimeStamp,pow=A1$`SCADA(MW)`)
TIMES <- format(POW_TREND$Time,"%H:%M:%S")
POW_MEANS <- tapply(POW_TREND$pow,format(POW_TREND$Time,"%H:%M:%S"), quantile ,probs=0.95,na.rm=T)
pow_95=data.frame(POW_MEANS)

GHI_TREND <- data.frame(Time=A1$TimeStamp,ghi=A1$`Actual_GHI (portal)`)
TIMES <- format(GHI_TREND$Time,"%H:%M:%S")
GHI_MEANS <- tapply(GHI_TREND$ghi,format(GHI_TREND$Time,"%H:%M:%S"), quantile ,probs=0.95,na.rm=T)
act_GHI_95=data.frame(GHI_MEANS)


WMS_TREND <- data.frame(Time=A1$TimeStamp,wms=A1$GHI_wms)
TIMES <- format(GHI_TREND$Time,"%H:%M:%S")
WMS_MEANS <- tapply(WMS_TREND$wms,format(WMS_TREND$Time,"%H:%M:%S"), quantile ,probs=0.95,na.rm=T)
wms_GHI_95=data.frame(WMS_MEANS)


HUM_TREND <- data.frame(Time=A1$TimeStamp,hum=A1$Humidity)
TIMES <- format(GHI_TREND$Time,"%H:%M:%S")
HUM_MEANS <- tapply(HUM_TREND$hum,format(HUM_TREND$Time,"%H:%M:%S"), quantile ,probs=0.95,na.rm=T)
hum_95=data.frame(HUM_MEANS)

abc=cbind(hum_95,wms_GHI_95,act_GHI_95,pow_95)
write_xlsx(abc,"D:/Ankit Work/work assigned/GHI_50hertz/95percentile.xlsx")
