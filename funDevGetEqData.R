### functions collect equity data and process them for further technical analysis
############## these functions assume the GENERIC functions "funDevelop.R" are already beed sourced
library(jsonlite)
library(RCurl)
library(TTR)
#####################BEGIN: functions to source data################################################################
GetData.PrepEquityId.126API <- function(idStock){
  Check.StopIf(!(is.vector(idStock)&is.character(idStock)),"Must be string vector")
  Check.StopIf((!is.character(idStock))&sum(nchar(idStock)!=6)>0,"Must be character of 6 elements!")
  
  tmp <- as.numeric(idStock)
  if(sum(is.na(tmp))>0){
    stop(paste("Some idStock is not valid: ",paste0(idStock[is.na(tmp)],collapse=",")))
  }
  ### covert normal 6-digit-id to website 7-digit-id
  # add 0 at 1st place means: shanghai exchange
  # add 1 at 1st place means: sz exchange
  idOut <- idStock
  idOut[tmp >= 500000] <- paste0("0",idStock[tmp >= 500000])
  idOut[tmp < 500000] <- paste0("1",idStock[tmp < 500000])
  return(idOut)
}

GetData.EquityIntraDay.126API <- function(idEquity){
  ### Prepare the input standard 6-digit stock id to 126API format  
  idStock <- GetData.PrepEquityId.126API(idEquity)  
  ### treatment for single stock inqury or multi stocks
  if(length(idStock)==1) {
    str.url <- paste0("http://api.money.126.net/data/feed/",idStock,",money.api")
  } else{
    str.url <- paste0("http://api.money.126.net/data/feed/",paste0(idStock,collapse=","),",money.api")
  }
  raw <- readLines(str.url, warn = "F",encoding="UTF-8")
  ########################### the following string positions are tylored for 126API
  json_data <- fromJSON(substr(substr(raw,1,nchar(raw)-2),22,nchar(raw)-2))
  if(length(json_data)==0){
    stop("None of the proved idStock can be found in api.money.126.net!")
  }
  namOut <- names(json_data)
  if (!identical(setdiff(idStock,namOut),character(0))){
    print(paste0("The provided idStock: ",paste0(substr(setdiff(idStock,namOut),2,7),collapse=",")," cannot be found in api.money.126.net"))
  }
  ########## convert list to data frame ##############
  ## select common data fields overtime
  dfOut <- as.data.frame(json_data[[1]],stringsAsFactors=FALSE)#[match(namField,names(json_data[[1]]))]
  if (length(namOut)>1){
    for (idx in 2:length(namOut)){
      tmp <- as.data.frame(json_data[[idx]],stringsAsFactors=FALSE)
      namField <- intersect(names(dfOut),names(json_data[[idx]]))
      dfOut <- rbind(dfOut[,match(namField,names(dfOut))],tmp[,match(namField,names(tmp))])
    }
  }
  namMust <- c("time","symbol","price","bid5","bid4","bid3","bid2","bid1","ask1","ask2","ask3","ask4","ask5",
               "bidvol5","bidvol4","bidvol3","bidvol2","bidvol1","askvol5","askvol4","askvol3","askvol2","askvol1",
               "yestclose","open","high","low","volume","turnover","percent")
  dfOut <- dfOut[,match(namMust,names(dfOut))]
  names(dfOut) <- c("time","idEquity","price","sel5","sel4","sel3","sel2","sel1","buy1","buy2","buy3","buy4","buy5",
                    "volSel5","volSel4","volSel3","volSel2","volSel1","volBuy5","volBuy4","volBuy3","volBuy2","volBuy1",
                    "closePreviDay","open","high","low","volume","turnover","percent")
  return(dfOut)
}
##############################################################################################END: FUNCTIONS TO SOURCE DATA##############

#################################BEGIN: functions to process data for further technical analysis ######################################
GetData.Covert2OHLC <- function(dfEq){
  Check.StopIf(!is.data.frame(dfEq),"Must be data frame")
  Check.ExistVarInDF(dfEq,c("date","open","high","low","close","volume"))
  print("This functions assumes the dfEq does not contain Adjusted prices")
  res <- dfEq[,match(c("open","high","low","close","volume","close"),names(dfEq))]
  names(res) <- c("Open", "High","Low", "Close","Volume", "Adjusted")
  return(as.quantmod.OHLC(xts(res,order.by = dfEq$date)))
}

GetData.Covert2TTR <- function(dfEq){
  Check.StopIf(!is.data.frame(dfEq),"Must be data frame")
  Check.ExistVarInDF(dfEq,c("date","open","high","low","close","volume"))
  print("This functions assumes the dfEq does not contain Adjusted prices")
  res <- dfEq[,match(c("open","high","low","close","volume"),names(dfEq))]
  return(res)
  #names(res) <- c("Open", "High","Low", "Close","Volume", "Adjusted")
  #return(as.quantmod.OHLC(xts(res,order.by = dfEq$date)))
}
############################################################# END: functions to process data for further technical analysis ##########


