rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
library(hydroGOF)
setwd("D:/Ankit Work/work assigned/GHI_50hertz")
A1<- read_excel("ICR_40_10_27.xlsx",sheet = "April")
A2<- read_excel("ICR_40_10_27.xlsx",sheet = "May")
A3<- read_excel("ICR_40_10_27.xlsx",sheet = "June")
GHI1<- A1[c(1,3)]
GHI2<- A2[c(1,3)]
GHI3<- A3[c(1,3)]
GHI_whole= rbind(GHI1,GHI2,GHI3)
##################  GHI
GHI_TREND <- data.frame(Time=GHI_whole$TIME,GHI=GHI_whole$Irradiation.GHI)
TIMES <- format(GHI_TREND$Time,"%H:%M:%S")
GHI_MEANS <- tapply(GHI_TREND$GHI,format(GHI_TREND$Time,"%H:%M:%S"), quantile ,probs=0.95,na.rm=T)

abc=data.frame(GHI_MEANS)
write_xlsx(abc,"D:/Ankit Work/work assigned/GHI_50hertz/95percentile.xlsx")
########  Humidity
Hum1<- A1[c(1,8)]
Hum2<- A2[c(1,8)]
Hum3<- A3[c(1,8)]
Hum_whole<- rbind(Hum1,Hum2,Hum3)
HUM_TREND <- data.frame(Time=GHI_whole$TIME,HUM=Hum_whole$Humidity)

TIMES <- format(HUM_TREND$Time,"%H:%M:%S")
HUM_MEANS <- tapply(HUM_TREND$HUM,format(HUM_TREND$Time,"%H:%M:%S"),  quantile ,probs=0.95,na.rm=T)
abc=data.frame(HUM_MEANS)
write_xlsx(abc,"D:/Ankit Work/work assigned/GHI_50hertz/95percentile.xlsx")



############
B1=A1[c(1,2,9,10,12,13,20)]
B2=A2[c(1,2,9,10,12,13,20)]
B3=A3[c(1,2,9,10,12,13,20)]
GHI_cloud=rbind(B1,B2,B3)
##write_xlsx(GHI_cloud,"D:/Ankit Work/work assigned/GHI_50hertz/GHI_cloud.xlsx")
#df=GHI_cloud[(GHI_cloud$Cloud.Hight1<=3000)]


df2=GHI_cloud[(GHI_cloud[,3]>=0000) & (GHI_cloud[,3]<=1000),]
df=df2[(df2[,7]<8),]
nw=cor( df$Irradiation.GHI, df$Total.Cloud,use = "pairwise.complete.obs")


nw=cor( df$Irradiation.GHI, df$Cloud.Hight1,use = "pairwise.complete.obs")
nw=cor( df$Irradiation.GHI, df$Cloud.Hight2,use = "pairwise.complete.obs")
nw=cor( df$Irradiation.GHI, df$Cloud.Layer1,use = "pairwise.complete.obs")
nw=cor( df$Irradiation.GHI, df$Cloud.Layer2,use = "pairwise.complete.obs")
nw=cor( df$Irradiation.GHI, df$Total.Cloud,use = "pairwise.complete.obs")

#write_xlsx(df,"D:/Ankit Work/work assigned/GHI_50hertz/cld_below2000andabove6octa.xlsx")
#MyData[(MyData[,2]>40),]

