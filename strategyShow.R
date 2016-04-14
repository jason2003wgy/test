rm(list=ls())
## load library and functions
source("/Users/JasonWang/Documents/LibR/funDevelop.R")

### import CSV data
nam.dirData <- "/Users/JasonWang/Documents/GTJA_TRADE/equityCSV_DB/"
nam.csvFile <- "SH000905.csv"

dataRaw <- read.csv(file=paste0(nam.dirData,nam.csvFile),header=TRUE,stringsAsFactors=FALSE)
dataRaw <- dataRaw[dataRaw$volume>0,]
dataRaw$date <- as.Date(dataRaw$date)
dfEq500 <- Eq.PrepMovAvg2(dataRaw,c(5,10,20,60))


dfEq <- dfEq500[dfEq500$date>=as.Date("2010-03-07"),]
chart.TimeSeries(xts(dfEq$close,order.by=dfEq$date))
vecLogi <- dfEq$close>dfEq$priceMA20 
tst <- Eq.ImplStrategy(dfEq,vecLogi,holdTime2Sim=250)
