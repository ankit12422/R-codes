rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
setwd("D:/Ankit Work/work assigned/GHI_50hertz")
######## April##########
A1<- read_excel("GHI_81days.xlsx",sheet = "1_8apr")
tt<- seq(as.POSIXct("2020-04-01 00:00:00"), as.POSIXct("2020-04-08 23:59:00"), by="min")
dat = data.frame(TimeStamp=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat=dat[c(-2)]
df1=cbind(A1,dat)

E <- df1[,c(5,2,3,4)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=8)
apr1_7 <- E[complete.cases(DAYTIME_CUTTER),]

A2<- read_excel("GHI_81days.xlsx",sheet = "9_16apr")
dat = data.frame(TimeStamp=seq(as.POSIXct("2020-04-09"), as.POSIXct("2020-04-09") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat=dat[c(-2)]
df1=cbind(A2,dat)
E <- df1[,c(5,2,3,4)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=8)
apr9_16 <- E[complete.cases(DAYTIME_CUTTER),]

A3<- read_excel("GHI_81days.xlsx",sheet = "17_24apr")
dat = data.frame(TimeStamp=seq(as.POSIXct("2020-04-17"), as.POSIXct("2020-04-17") + 60*11519, by=60),
                 count=sample(1:50, 11520, replace=TRUE))
dat=dat[c(-2)]
df1=cbind(A3,dat)
E <- df1[,c(5,2,3,4)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=8)
apr17_24 <- E[complete.cases(DAYTIME_CUTTER),]

A4<- read_excel("GHI_81days.xlsx",sheet = "25_30apr")
dat = data.frame(TimeStamp=seq(as.POSIXct("2020-04-25"), as.POSIXct("2020-04-25") + 60*8639, by=60),
                 count=sample(1:50, 8640, replace=TRUE))
dat=dat[c(-2)]
df1=cbind(A4,dat)
E <- df1[,c(5,2,3,4)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=8)
apr25_30 <- E[complete.cases(DAYTIME_CUTTER),]
apr=rbind(apr1_7,apr9_16,apr17_24,apr25_30)
write_xlsx(apr,"D:/Ankit Work/work assigned/GHI_50hertz/apr_15min.xlsx")
