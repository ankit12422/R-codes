rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
setwd("D:/Ankit Work/work assigned")
A<- read_excel("June WMS REPORT.xlsx",sheet = "WMS REPORT")
tt<- seq(as.POSIXct("2020-06-01 00:00:00"), as.POSIXct("2020-06-20 23:59:00"), by="min")
GHI <- A[, c(1,3)]
GII <- A[, c(1,2)]
wi_sp<- A[, c(1,7)]
wi_dir<- A[, c(1,8)]
humid <- A[, c(1,9)]
am_temp<- A[, c(1,10)]
mod_temp<- A[, c(1,11)]
rain<- A[, c(1,12)]
dust<- A[, c(1,13)]
vis<- A[, c(1,16)]
cl_ht1<- A[, c(1,20)]
cl_ht2<- A[, c(1,21)]
cl_lay1<- A[, c(1,23)]
cl_lay2<- A[, c(1,24)]
dat = data.frame(TimeStamp=seq(as.POSIXct("2020-06-01"), as.POSIXct("2020-06-01") + 60*28799, by=60),
                 count=sample(1:50, 28800, replace=TRUE))
tt=cbind(tt,dat)
###############
GHI_df=left_join(dat,GHI, by = "TimeStamp")
GII_df=left_join(dat,GII, by = "TimeStamp")
wisp_df=left_join(dat,wi_sp, by = "TimeStamp")
widir_df=left_join(dat,wi_dir, by = "TimeStamp")
humid_df=left_join(dat,humid, by = "TimeStamp")
amtemp_df=left_join(dat,am_temp, by = "TimeStamp")
modtemp_df=left_join(dat,mod_temp, by = "TimeStamp")
rain_df=left_join(dat,rain, by = "TimeStamp")
dust_df=left_join(dat,dust, by = "TimeStamp")
vis_df=left_join(dat,vis, by = "TimeStamp")
clht1_df=left_join(dat,cl_ht1, by = "TimeStamp")
clht2_df=left_join(dat,cl_ht2, by = "TimeStamp")
cllay1_df=left_join(dat,cl_lay1, by = "TimeStamp")
cllay2_df=left_join(dat,cl_lay2, by = "TimeStamp")
###########
GHI_df=GHI_df[c(-2)]
GII_df=GII_df[c(-2)]
wisp_df=wisp_df[c(-2)]
widir_df=widir_df[c(-2)]
humid_df=humid_df[c(-2)]
amtemp_df=amtemp_df[c(-2)]
modtemp_df=modtemp_df[c(-2)]
rain_df=rain_df[c(-2)]
dust_df=dust_df[c(-2)]
vis_df=vis_df[c(-2)]
clht1_df=clht1_df[c(-2)]
clht2_df=clht2_df[c(-2)]
cllay1_df=cllay1_df[c(-2)]
cllay2_df=cllay2_df[c(-2)]
#######
library(dplyr) 
abc = cbind(GHI_df,GII_df,wisp_df,widir_df,humid_df,amtemp_df,modtemp_df,
                  rain_df,dust_df,vis_df,clht1_df,clht2_df,cllay1_df,cllay2_df)
#######
df2=abc[c(-3,-5,-7,-9,-11,-13,-15,-17,-19,-21,-23,-25,-27)]
df2$TimeStamp<- df2$TimeStamp+hours(5)+minutes(30)
##### daytime cutter #############
E <- df2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=30)
H <- E[complete.cases(DAYTIME_CUTTER),]
write_xlsx(H,"D:/Ankit Work/work assigned/June_min.xlsx")
