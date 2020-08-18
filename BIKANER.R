rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
setwd("D:/Ankit Work/work assigned/GHI_50hertz")
######## Daily for April##########
A1<- read_excel("GHI_81days.xlsx",sheet = "1_8apr")
GHI=A1[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
#GHI=cbind(A1$`Actual GHI (watt/m2)`,newdat$by15)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI1=GHI
######## April 2
A2<- read_excel("GHI_81days.xlsx",sheet = "9_16apr")
GHI=A2[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-04-09"), as.POSIXct("2020-04-09") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI2=GHI
######## April 3
A3<- read_excel("GHI_81days.xlsx",sheet = "17_24apr")
GHI=A3[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-04-17"), as.POSIXct("2020-04-17") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI3=GHI
######## April 4
A4<- read_excel("GHI_81days.xlsx",sheet = "25_30apr")
GHI=A4[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-04-25"), as.POSIXct("2020-04-25") + 60*8639, by=60),
                 count=sample(1:50, 8640, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI4=GHI

GHI_apr=rbind(GHI1,GHI2,GHI3,GHI4)
E <- GHI_apr[,c(1,2)]
###### checking time from 6am to 7:15 pm in each of the 96 time blocks(24+54+18)
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=30)
GHI_act <- E[complete.cases(DAYTIME_CUTTER),]

#write_xlsx(H,"D:/Ankit Work/work assigned/wms_min.xlsx")
#######################################################
ALL1=A1[c(1,3)]
GT1=A1[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F1=cbind(newdat,ALL1,GT1)
F1=F1[c(1,4,6)]


ALL2=A2[c(1,3)]
GT2=A2[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-04-09"), as.POSIXct("2020-04-09") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F2=cbind(newdat,ALL2,GT2)
F2=F2[c(1,4,6)]


ALL3=A3[c(1,3)]
GT3=A3[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-04-17"), as.POSIXct("2020-04-17") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F3=cbind(newdat,ALL3,GT3)
F3=F3[c(1,4,6)]


ALL4=A4[c(1,3)]
GT4=A4[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-04-25"), as.POSIXct("2020-04-25") + 60*8639, by=60),
                 count=sample(1:50, 8640, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F4=cbind(newdat,ALL4,GT4)
F4=F4[c(1,4,6)]

F_apr=rbind(F1,F2,F3,F4)
E <- F_apr[,c(1,2,3)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=30)
F_GHI <- E[complete.cases(DAYTIME_CUTTER),]

April_GHI=cbind(GHI_act,F_GHI)
April_GHI=April_GHI[c(-3)]
write_xlsx(April_GHI,"D:/Ankit Work/work assigned/GHI_50hertz/april_GHI.xlsx")

########################### MAY 1############################
A1<- read_excel("GHI_81days.xlsx",sheet = "1_8may")
GHI=A1[c(1,2)]
dat = data.frame(time=seq(as.POSIXct("2020-05-01"), as.POSIXct("2020-05-01") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI1=GHI
######## May 2
A2<- read_excel("GHI_81days.xlsx",sheet = "9_16may")
GHI=A2[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-05-09"), as.POSIXct("2020-05-09") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI2=GHI
######## May 3
A3<- read_excel("GHI_81days.xlsx",sheet = "17_24may")
GHI=A3[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-05-17"), as.POSIXct("2020-05-17") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI3=GHI
######## May 4
A4<- read_excel("GHI_81days.xlsx",sheet = "25_31may")
GHI=A4[c(1,2)]
dat = data.frame(time=seq(as.POSIXct("2020-05-25"), as.POSIXct("2020-05-25") + 60*10079, by=60),
                 count=sample(1:50, 10080, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI4=GHI

GHI_apr=rbind(GHI1,GHI2,GHI3,GHI4)
E <- GHI_apr[,c(1,2)]

DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=30)
GHI_act <- E[complete.cases(DAYTIME_CUTTER),]


ALL1=A1[c(1,3)]
GT1=A1[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-05-01"), as.POSIXct("2020-05-01") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F1=cbind(newdat,ALL1,GT1)
F1=F1[c(1,4,6)]


ALL2=A2[c(1,3)]
GT2=A2[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-05-09"), as.POSIXct("2020-05-09") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F2=cbind(newdat,ALL2,GT2)
F2=F2[c(1,4,6)]


ALL3=A3[c(1,3)]
GT3=A3[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-05-17"), as.POSIXct("2020-05-17") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F3=cbind(newdat,ALL3,GT3)
F3=F3[c(1,4,6)]


ALL4=A4[c(1,3)]
GT4=A4[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-05-25"), as.POSIXct("2020-05-25") + 60*10079, by=60),
                 count=sample(1:50, 10080, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F4=cbind(newdat,ALL4,GT4)
F4=F4[c(1,4,6)]

F_apr=rbind(F1,F2,F3,F4)
E <- F_apr[,c(1,2,3)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=30)
F_GHI <- E[complete.cases(DAYTIME_CUTTER),]

May_GHI=cbind(GHI_act,F_GHI)
May_GHI=May_GHI[c(-3)]
write_xlsx(May_GHI,"D:/Ankit Work/work assigned/GHI_50hertz/may_GHI.xlsx")

########################### JUNE 1############################
A1<- read_excel("GHI_81days.xlsx",sheet = "1_8jun")
GHI=A1[c(1,2)]
dat = data.frame(time=seq(as.POSIXct("2020-06-01"), as.POSIXct("2020-06-01") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI1=GHI
######## June 2
A2<- read_excel("GHI_81days.xlsx",sheet = "9_16jun")
GHI=A2[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-06-09"), as.POSIXct("2020-06-09") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI2=GHI
######## June 3
A3<- read_excel("GHI_81days.xlsx",sheet = "17_20jun")
GHI=A3[c(1,2)]
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-06-17"), as.POSIXct("2020-06-17") + 60*5759, by=60),
                 count=sample(1:50, 5760, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
G1=cbind(GHI,newdat)
GHI=G1[c(3,2)]
GHI3=GHI

GHI_apr=rbind(GHI1,GHI2,GHI3)
E <- GHI_apr[,c(1,2)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=30)
GHI_act <- E[complete.cases(DAYTIME_CUTTER),]


ALL1=A1[c(1,3)]
GT1=A1[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-06-01"), as.POSIXct("2020-06-01") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F1=cbind(newdat,ALL1,GT1)
F1=F1[c(1,4,6)]


ALL2=A2[c(1,3)]
GT2=A2[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-06-09"), as.POSIXct("2020-06-09") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F2=cbind(newdat,ALL2,GT2)
F2=F2[c(1,4,6)]


ALL3=A3[c(1,3)]
GT3=A3[c(1,4)]
dat = data.frame(time=seq(as.POSIXct("2020-06-17"), as.POSIXct("2020-06-17") + 60*5759, by=60),
                 count=sample(1:50, 5760, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)
F3=cbind(newdat,ALL3,GT3)
F3=F3[c(1,4,6)]


F_apr=rbind(F1,F2,F3)
E <- F_apr[,c(1,2,3)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=30)
F_GHI <- E[complete.cases(DAYTIME_CUTTER),]

Jun_GHI=cbind(GHI_act,F_GHI)
Jun_GHI=Jun_GHI[c(-3)]
write_xlsx(Jun_GHI,"D:/Ankit Work/work assigned/GHI_50hertz/june_GHI.xlsx")
