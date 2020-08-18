rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
setwd("D:/Ankit Work/work assigned/GHI_50hertz")
A1<- read_excel("GHI_morning_even_81days.xlsx",sheet = "wms")
GHI=A1[c(1,2)]
dat = data.frame(time=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*116639, by=60),
                 count=sample(1:50, 116640, replace=TRUE))
dat$TimeStamp = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ TimeStamp, FUN=sum, data=dat)

E<- newdat
DAYTIME_CUTTER <- rep((c(rep(NA,times=24),rep(1,times=54),rep(NA,18))),times=81)
tim15 <- E[complete.cases(DAYTIME_CUTTER),]

# Do mean of every 15 rows of GHI
df=A1[c(2)]
n <- 15;
min15=aggregate(df,list(rep(1:(nrow(df)%/%n+1),each=n,len=nrow(df))),mean)[-1];

E <- GHI[,c(1,2)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=1),rep(1,times=795),rep(NA,0))),times=81)
GHI_act <- E[complete.cases(DAYTIME_CUTTER),]
