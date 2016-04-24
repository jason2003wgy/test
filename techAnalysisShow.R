rm(list=ls())
## load library and functions
source("funDevelop.R")
source("funDevGetEqData.R")

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


####### prepare data for library of technical analysis
ohEq500 <- GetData.Covert2TTR(dfEq500)

#### considering functions summarized in http://www.rdocumentation.org/packages/TTR
# MACD(ohEq500$close)
# RSI(ohEq500$close)