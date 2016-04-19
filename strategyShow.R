rm(list=ls())
## load library and functions
source("funDevelop.R")

### import CSV data
nam.dirData <- "/Users/JasonWang/Documents/GTJA_TRADE/equityCSV_DB/"
## import ZZ500
nam.csvFile <- "SH000905.csv"
dataRaw <- read.csv(file=paste0(nam.dirData,nam.csvFile),header=TRUE,stringsAsFactors=FALSE)
dataRaw <- dataRaw[dataRaw$volume>0,]
dataRaw$date <- as.Date(dataRaw$date)
dfEq500 <- Eq.PrepMovAvg2(dataRaw,c(5,10,20,60))
## import HS300 
nam.csvFile <- "SH000300.csv"
dataRaw <- read.csv(file=paste0(nam.dirData,nam.csvFile),header=TRUE,stringsAsFactors=FALSE)
dataRaw <- dataRaw[dataRaw$volume>0,]
dataRaw$date <- as.Date(dataRaw$date)
dfEq300 <- Eq.PrepMovAvg2(dataRaw,c(5,10,20,60))


############################################TypeA: full position single asset in/out ###################################################
dfEq <- dfEq500[dfEq500$date>=as.Date("2007-07-01"),]
chart.TimeSeries(xts(dfEq$close,order.by=dfEq$date))
vecLogi <- dfEq$close>dfEq$priceMA20 
### approach A: buy/sell @ open price at +1 day after the signal shows
tst <- Eq.ImplStrategy(dfEq,vecLogi,nrDays2ExcuteOrder=1,type.price="open",holdTime2Sim=500)
### approach B: buy/sell @ close price at the same day the signal shows
tst <- Eq.ImplStrategy(dfEq,vecLogi,nrDays2ExcuteOrder=0,type.price="close",holdTime2Sim=500)
###############################################################################END: Type A###############


############################################Type B: partial position single asset in/out ###################################################
### import strategy dictionary
nam.dirData <- ""
dictStra <- DataP.DfFactor2Character(data.frame(read.csv(file=paste0(nam.dirData,"dict_invEq.csv"),head=TRUE),stringsAsFactors=FALSE))
###### get buy/sell signal
dateCutOff <- as.Date("2013-03-01")
dfEq <- dfEq300[dfEq300$date>=dateCutOff,]
retBen <- Eq.CalReturnAdjacent(dfEq$close,dfEq$date)
dfEq$state <- NA
dfEq$state[dfEq$close>dfEq$priceMA20 & dfEq$priceMA5 >= dfEq$priceMA20] <- "full"
dfEq$state[dfEq$close>dfEq$priceMA20 & dfEq$priceMA5<dfEq$priceMA20] <- "part"
dfEq$state[dfEq$close<dfEq$priceMA20] <- "clean"
Check.StopIf(sum(is.na(dfEq$state))>0,"Should not contain any NA. Sth is not defined")
aggregate(close~state,data=dfEq,FUN=length)
###### get return of strategy
ret.oneAsset <- Eq.calRetAccountWeights(Eq.PrepHistBySym("ETF",dfEq),dictStra)
df <- data.frame(date=ret.oneAsset$retStra$date,benchmark=ret.oneAsset$retStra.extra$value,strategy=ret.oneAsset$retStra$return,stringsAsFactors=TRUE)
res <- Eq.EvalPerform(df,ret.oneAsset$retStra$hold,holdTime2Sim=250)
####################################################################################END: Type B############


######################################### Type C: partial postion with multiple assets ################################
retBen <- df[,match(c("date","benchmark"),names(df))]
### import strategy dictionary
nam.dirData <- ""
dictStra <- DataP.DfFactor2Character(data.frame(read.csv(file=paste0(nam.dirData,"dict_invMultiEq.csv"),head=TRUE),stringsAsFactors=FALSE))
###### get buy/sell singla
dfEq.hs <- Tmp.PrepStateByMa(dfEq300[dfEq300$date>=dateCutOff,])
dfEq.zz <- Tmp.PrepStateByMa(dfEq500[dfEq500$date>=dateCutOff,])
dfAdj <- Eq.MergeTwoSymHist(Eq.PrepHistBySym("hs300",dfEq.hs),Eq.PrepHistBySym("zz500",dfEq.zz))
tmp <- Eq.calRetAccountWeights(dfAdj,dictStra)
df <- data.frame(date=tmp$retStra$date,benchmark=retBen$benchmark,strategy=tmp$retStra$return,stringsAsFactors=TRUE)
res <- Eq.EvalPerform(df,tmp$retStra$hold,holdTime2Sim=250)
####################################################


