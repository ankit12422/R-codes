rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library('tidyverse')
library(hydroGOF)
setwd("D:/Ankit Work/work assigned/GHI_50hertz")
A1<- read_excel("wms_new.xlsx",sheet = "April")
GHI1<- A1[c(1,2)]
df=A1[c(2)]
a=quantile(df$Irradiation_GII, 0.95,na.rm = TRUE)

n <- 15;
min15=aggregate(df,list(rep(1:(nrow(df)%/%n+1),each=n,len=nrow(df))),quantile(df$Irradiation_GII,0.95,na.rm = TRUE))[-1];

#df1 <- tapply(df,date, quantile,probs = 0.75, na.rm=T)
df1 <- tapply(GHI1$Irradiation_GII,GHI1$TIME, quantile,probs = 0.75, na.rm=T)
df2=data.frame(df1)

#######

m<-seq(1,(length(IC))*96,96)
n<-seq(96,(nrow(a)-96),96)

output = matrix(0, 796, 30);
for(i in 1:796)
{
  c=i
  for (j in 1:30)
  {
    t=df[c,1]
    output[i-1][j-1]=t
    c=c+796
  }
}
###########################
GHI_TREND <- data.frame(Time=A1$TIME,GHI=A1$Irradiation.GHI)
TIMES <- format(GHI_TREND$Time,"%H:%M:%S")
GHI_MEANS <- tapply(GHI_TREND$GHI,format(GHI_TREND$Time,"%H:%M:%S"), quantile ,probs=0.90,na.rm=T)

abc=data.frame(GHI_MEANS)


#########
GHI_TREND <- data.frame(TIME=H$TIME,GHI=H$Irradiation.GHI)

TIMES <- format(GHI_TREND$TIME,"%H:%M:%S")
GHI_MEANS <- tapply(GHI_TREND$GHI,format(GHI_TREND$TIME,"%H:%M:%S"), quantile ,probs=0.90,na.rm=T)


TEMP_TREND <- data.frame(TIME=H$TIME,TEMP=H$Mod..Temp)

TIMES <- format(GHI_TREND$TIME,"%H:%M:%S")
TEMP_MEANS <- tapply(TEMP_TREND$TEMP,format(TEMP_TREND$TIME,"%H:%M:%S"),  quantile ,probs=0.90,na.rm=T)

HUM_TREND <- data.frame(TIME=H$TIME,HUM=H$Humidity)

TIMES <- format(HUM_TREND$TIME,"%H:%M:%S")
HUM_MEANS <- tapply(HUM_TREND$HUM,format(HUM_TREND$TIME,"%H:%M:%S"),  quantile ,probs=0.90,na.rm=T)

WSP_TREND <- data.frame(TIME=H$TIME,WSP=H$Wind.Speed)

TIMES <- format(WSP_TREND$TIME,"%H:%M:%S")[1:796]

WSP_MEANS <- tapply(WSP_TREND$WSP,format(WSP_TREND$TIME,"%H:%M:%S"),  quantile ,probs=0.90,na.rm=T)

TRENDS <- data.frame(TIMES,GHI_MEANS,TEMP_MEANS,HUM_MEANS,WSP_MEANS)

write_xlsx(TRENDS,"NORMAL_DAY_TRENDS.xlsx")
