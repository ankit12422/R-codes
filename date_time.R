library(readxl)
library(writexl)
library(lubridate)
set.seed(4984)
dates = data.frame(time=seq(as.POSIXct("2016-05-01"), as.POSIXct("2016-05-01") + 60*99, by=60),
                 count=sample(1:50, 100, replace=TRUE))
dates$by15 = cut(dates$time, breaks="15 min")
dates.summary = aggregate(count ~ by15, FUN=sum, data=dates)

library(dplyr)
left_join(df1, df2, by = "date_time")

########### create 15 minute time interval#######
set.seed(4984)
dat = data.frame(time=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*99, by=60),
                 count=sample(1:50, 100, replace=TRUE))
dat$by15 = cut(dat$time, breaks="15 min")
newdat = aggregate(count ~ by15, FUN=sum, data=dat)

########### create every minute time interval #######
dat = data.frame(TimeStamp=seq(as.POSIXct("2020-04-01"), as.POSIXct("2020-04-01") + 60*43199, by=60),
                 count=sample(1:50, 43200, replace=TRUE))
tt<- seq(as.POSIXct("2020-04-01 00:00:00"), as.POSIXct("2020-04-30 23:59:00"), by="min")

##### daytime cutter #############
E <- df2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=30)
H <- E[complete.cases(DAYTIME_CUTTER),]
############ hourly #############
n<- 4;
wind_avg=aggregate(wind,list(rep(1:(nrow(wind)%/%n+1),each=n,len=nrow(wind))),mean)[-1]
tt<- seq(as.POSIXct("2020-06-01 00:00:00"), as.POSIXct("2020-07-17 23:00:00"), by="hour")
