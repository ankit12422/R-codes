rm(list=ls())
library(readxl)
library(writexl)
library(lubridate)
setwd("C:/Users/CASSIM/Downloads/WORK/WMS DATA")
LIST_ <- list.files(pattern="REPORT.xlsx")

DATUM <- list()

for(i in 1:3)
{
A<- read_xlsx(LIST_[i],sheet = 1)
DATUM[[i]] <- A
rm(A)
}

C <- do.call(rbind,DATUM)

#write_xlsx(C,"DATA.xlsx")

###################################################
#FIRST SMOOTHENING THE GHI DATA TO 15 mins to plot in excel and check unusual/cloud affected days

D <- data.frame(TIME=C$TimeStamp,GHI=as.numeric(C$`Irradiation GHI`))
DD<-round_date(D$TIME,unit="1 minute")

D$TIME <- DD
TT15 <- data.frame(TIME=seq.POSIXt(from=D$TIME[1],to=D$TIME[nrow(D)],by="1 min")) 

E <- merge(TT15,D,all.x =T)
E <- E[(!duplicated(E$TIME)),]
E$REPPER15 <- rep(1:(nrow(E)/15), each = 15)
GHI_15min <- tapply(E$GHI,E$REPPER15 ,mean,na.rm=T)

TT15 <- data.frame(TIME=seq.POSIXt(from=E$TIME[1],to=E$TIME[nrow(E)],by="15 min"),GHI=GHI_15min) 

#write_xlsx(TT15,"GHI_15min.xlsx")
#######################################################
#ADJUSTING TIME STAMPS FOR MISSING DATES AND SORTING ACCORDINGLY

D <- data.frame(TIME=C$TimeStamp,C[,-1])
DD<-round_date(D$TIME,unit="1 minute")

D$TIME <- DD
TT1 <- data.frame(TIME=seq.POSIXt(from=D$TIME[1],to=D$TIME[nrow(D)],by="1 min")) 

E <- merge(TT1,D,all.x =T)
E <- E[(!duplicated(E$TIME)),]
E <- E[,-4]
#write_xlsx(E,"ALL_DATA_cleaned_1min.xlsx")

##############################################################
#DIRECT CROSS CORRELATION BETWEEN VARIABLES

FF <- cor(E[,-1],use = "pairwise.complete.obs")

G <- data.frame(rownames(FF),FF[,2])
#rownames(G) <- paste(rownames(FF))

#write_xlsx(G,"CORRELATION_WITH_GHI_all_DAT.xlsx")

#############################################################
#CUTTING THE DATA TO ONLY DAY-TIME i.e. 0600 to 1915
 
E <- E[,-4]

DAYTIME_CUTTER <- rep((c(rep(NA,times=360),rep(1,times=796),rep(NA,284))),times=81)

H <- E[complete.cases(DAYTIME_CUTTER),]

#write_xlsx(H,"ALL_DATA_cleaned_1min_DAYTIME.xlsx")

#############################################################

#DIRECT CROSS CORRELATION BETWEEN VARIABLES AGAIN FOR DAYTIME ONLY

FFF <- cor(H[,-1],use = "pairwise.complete.obs")

G <- data.frame(rownames(FFF),FFF[,2])
#rownames(G) <- paste(rownames(FF))

#write_xlsx(G,"CORRELATION_WITH_GHI_all_DAT_DAYTIME_FILTERED.xlsx")


##############################################################

DUST <- H$DUST.IQ
DUST_CATEGORIZED <- split(DUST,DUST)

SR1 <- H$Soil.Ratio1
SR1_CATEGORIZED <- split(SR1,SR1)

SR2 <- H$Soil.Ratio2
SR2_CATEGORIZED <- split(SR2,SR2)

RAIN <- H$Rain
RAIN_CATEGORIZED <-split(RAIN,RAIN)


