############# load library
library(xlsx)
library(stringr)
library(PerformanceAnalytics)


####################################################################################################################################################################################
###################################################################### CHECK function development################################################################################
###################### check functions
#check.allNA <- function(inp){ return(Check.AllNA(inp)) }
#check.isScalar <- function(inp) { return(Check.IsScalar(inp))}
#check.stopIf <- function(conditionTrue,stopMessage){ return(Check.StopIf(conditionTrue,stopMessage)) }
#check.unique <- function(inpVec){ return(Check.Unique(inpVec))}
#check.existVarInDF <- function(inpDf,varNames) { return(Check.ExistVarInDF(inpDf,varNames))}

Check.AllNA <- function(inp){
  ## only works for vector!
  if (sum(is.na(inp))==length(inp)){
    stop("The input vector is all NA")
  }
}

Check.IsScalar <- function(inp){
  #scalar <=> inp is a vector AND length is one#
  if(!( is.vector(inp) & length(inp)==1)){
    stop("inp is not a scalar")
  }
}

Check.StopIf <- function(conditionTrue,stopMessage){  
  Check.IsScalar(conditionTrue)  
  if(!is.logical(conditionTrue)){
    stop("'conditionTrue' input must be logical varaible")
  }
  if(!is.character(stopMessage)){
    stop("'stopMessage' input must be string")
  }
  if (conditionTrue){
    stop(stopMessage)
  }
}

Check.Unique <- function(inpVec){
  if (length(inpVec) != length(unique(inpVec))){
    stop("The input vector is not unique!")
  }
}

Check.ExistVarInDF <- function(inpDf,varNames){
  Check.StopIf(!(is.data.frame(inpDf)|is.matrix(inpDf)|sum(class(inpDf)%in%"xts")>0),"Must be df, matrix or xts")
  Check.StopIf(!(is.character(varNames)&is.vector(varNames)),"Must be character vector")
  Check.StopIf(length(unique(varNames))!=length(varNames),"varNames Must be unique")
  if (sum(names(inpDf) %in% varNames)!=length(varNames)){
    stop(paste0("These names cannot be found in inpDf ",setdiff(varNames,names(inpDf))))		 
  }  
}

Check.VerifyDictStr <- function(dictStra){
  Check.StopIf(!is.data.frame(dictStra),"Must be data frame!")
  Check.ExistVarInDF(dictStra,c("state","asset","weight"))
  uniState <- sort(unique(dictStra$state))
  uniAsset <- sort(unique(dictStra$asset))
  for (idx in 1:length(uniState)) {
    Check.StopIf(!identical(sort(unique(dictStra$asset[dictStra$state==uniState[idx]])),uniAsset),
                 paste0("In state of ",uniState[idx]," all asset class should be defined!"))
  }
}
##############################################END: check functions#########

#################### Begin: DATA PREPARE functions###############################
DataP.GetDfByCommonDict <- function(rawVal,rawVarName,dict){
  Check.StopIf(!is.character(rawVarName),"rawVarName must be character.")
  Check.StopIf(!is.data.frame(rawVal),"rawVal must be data frame.")
  Check.StopIf(!is.data.frame(dict),"dict must be data frame.")
  Check.ExistVarInDF(dict,"varXls")
  Check.ExistVarInDF(dict,"varR")
  ## check the name vector has the same size as the column of the data matrix
  Check.StopIf(length(rawVarName) != dim(rawVal)[2],"Mismatch between col names and #col in data input")
  ## check for empty col name vector
  Check.StopIf(sum(rawVarName == "") >0,"Col name vector has missing values!")
  rawVarName <- toupperNoSpace(rawVarName)
  dict$varXls <- toupperNoSpace(dict$varXls)
  Check.StopIf(identical(intersect(rawVarName,dict$varXls),character(0)),"One of rawVarName must present in dictNames!")
  ## names matching
  namComm <- intersect(rawVarName,dict$varXls)
  mapData2Nam <- match(namComm,rawVarName)
  res <- rawVal[,mapData2Nam]
  mapDict2Nam <- match(namComm,dict$varXls)
  names(res) <- dict$varR[mapDict2Nam]
  return(list(data=res,desc=dict[mapDict2Nam,]))
}

DataP.DfFactor2Character <- function(dfInp){  
  ## For a given data frame with factor class data fields, this function converts
  # the factor class to character class  
  if(!is.data.frame(dfInp)){
    stop("Input must be data frame!")
  }
  nCol <- dim(dfInp)[2]
  dataName <- names(dfInp)
  for(idx in 1:nCol){
    if(is.factor(dfInp[,idx])){
      dfInp[,idx] <- as.character(dfInp[,idx])
      print(paste0(dataName[idx]," has been converted to character class"))
    }
  }
  return(dfInp)
}

DataP.GetFieldsForUniId <- function(idDuplicated,ref2Judge,mat2Pick,condit2Pick){
  ## for each unique id of idDuplicated (i.e., ) pick a row of the associated mat2Pick according to min/max of the associated ref2Judge values
  ## this function can be used as max EAD rule!!! DataP.GetFieldsForUniId(idDuplicated,EAD,mat2Pick,"max")
  if (class(idDuplicated)!="Date" & class(mat2Pick)!="Date"){
    Check.StopIf(!(is.vector(idDuplicated)&is.vector(ref2Judge)),"Both ref2Judge and idDuplicated must be vector") 
  }
  Check.StopIf(!(is.vector(mat2Pick)|is.matrix(mat2Pick)|is.data.frame(mat2Pick)),"mat2Pick must be vector, matrix or data.frame")
  Check.StopIf(!(length(idDuplicated)==length(ref2Judge) & length(idDuplicated)==Size(mat2Pick)[1]),
               "ref2Judge, idDuplicated and mat2Pick must have same Nr rows")
  Check.IsScalar(condit2Pick)
  Check.StopIf(sum(toupperNoSpace(condit2Pick)%in%c("MIN","MAX"))!=1,"condit2Pick must be either MIN or MAX")
  
  uniId <- unique(idDuplicated)
  if (length(uniId)==length(idDuplicated)){
    print("The input idDuplicated is unique! Thus output is equal to mat2Pick!")
    return(list(idUnique=uniId,matPicked=mat2Pick))
  } else {   ### normal non-unique cases
    #### initialized the values of res
    ixSel <- idDuplicated==uniId[1]
    tmpSortIx <- match(sort(ref2Judge[ixSel]),ref2Judge[ixSel])
    ### logical values will be same over the loop
    logiConOfMin <- toupperNoSpace(condit2Pick)=="MIN"
    logiConOfMax <- toupperNoSpace(condit2Pick)=="MAX"
    logiTrueVector <- is.vector(mat2Pick)
    logiTrueMat <- is.data.frame(mat2Pick)|| is.matrix(mat2Pick)
    # deceide which row of the corresponding mat2Pick to choose
    if (logiConOfMin){
      posRow2Select <- tmpSortIx[1]
    } else if (logiConOfMax) {
      posRow2Select <- tail(tmpSortIx,1)
    } else {
      stop("value of condit2Pick should be either MIN or MAX!!!")
    }
    # treatment of mat2Pick given vecto or matrix/dataFrame
    if (logiTrueVector) {
      res <- mat2Pick[ixSel][posRow2Select]
    } else if (logiTrueMat) {
      res <- mat2Pick[ixSel,][posRow2Select,]
    } else {
      stop("Here: mat2Pick must be vector, matrix or data.frame")
    }
    if (length(uniId)>1) {
      for (idx in 2:length(uniId)){
        ixSel <- idDuplicated==uniId[idx]
        tmpSortIx <- match(sort(ref2Judge[ixSel]),ref2Judge[ixSel])
        #################BEGIN:  same treatment as initialization part###
        # deceide which row of the corresponding mat2Pick to choose
        if (logiConOfMin){
          posRow2Select <- tmpSortIx[1]
        } else if (logiConOfMax) {
          posRow2Select <- tail(tmpSortIx,1)
        } else {
          stop("value of condit2Pick should be either MIN or MAX!!!")
        }
        #############END###
        # treatment of mat2Pick given vecto or matrix/dataFrame
        if (logiTrueVector) {
          res <- c(res,mat2Pick[ixSel][posRow2Select])
        } else if (logiTrueMat) {
          res <- rbind(res,mat2Pick[ixSel,][posRow2Select,])
        } else {
          stop("Here: mat2Pick must be vector, matrix or data.frame")
        }
      }
    }
    Check.StopIf(length(uniId)!=Size(res)[1],"Failed quality control")
    return(list(idUnique=uniId,matPicked=res))
  }
}

DataP.PrepXts <- function(dfEq,order.by,fieldOut=names(dfEq),rm.na=TRUE,...){
  Check.StopIf(!(is.data.frame(dfEq)||is.matrix(dfEq)||is.vector(dfEq)),"dfEq should be in these classes, otherwise expand!")
  Check.StopIf(class(order.by)!="Date","Must be class of Date!")
  Check.Unique(order.by)
  Check.StopIf(!identical(order.by,sort(order.by)),"Must be sorted")
  
  xtsOut <- xts(dfEq,order.by=order.by,...)
  if(!identical(names(xtsOut),fieldOut)){
    Check.Unique(fieldOut)
    Check.StopIf(!(is.character(dfEq)&identical(setdiff(fieldOut,names(xtsOut)),character(0))),"Must be string and be subset of names(xtsOut)")
    xtsOut <- xtsOut[,match(fieldOut,names(xtsOut))]
  }
  if (rm.na){
    ixExcl <- apply(is.na(xtsOut),1,sum)>0
    if(sum(ixExcl)>0){
      print(paste0("There are ",sum(ixExcl)," rows contain at least one NA. These rows are REMOVED."))
      if (sum(ixExcl)==length(ixExcl)){
        stop("Every row of output contains a NA! Please consider set rm.na=FALSE! ")
      } else{
        xtsOut <- xtsOut[!ixExcl,]
      }
    }
  }
  return(xtsOut)
} 

DataP.xts2DataFrame <- function(xtsInp){
  Check.StopIf(sum(class(xtsInp)%in%"xts")==0,"xtsInp must be class of xts")
  if(sum(names(xtsInp)=="date")>0){
    print("xtsInp contains filed of date. However, which is replace by index(xtsInp)")
  } else {
    print("A filed named by date is created for output df")
  }
  res <- data.frame(xtsInp[,names(xtsInp)!="date"],stringsAsFactors=FALSE)
  res$date <- index(xtsInp)
  return(res)
}

DataP.GetPosBegEndTrueGroup <- function(vecLogical){
  Check.StopIf(!(is.vector(vecLogical)&class(vecLogical)=="logical"),"Must be logical vector!")
  if(sum(is.na(vecLogical))>0){
    print(paste0("There are ",sum(is.na(vecLogical))," NA in the input, which are replaced by FALSE!"))
    vecLogical[is.na(vecLogical)] <- FALSE
  }
  Check.StopIf(sum(vecLogical==FALSE)==length(vecLogical),"vecLogical only have FALSE! No need for this function")
  
  res <- data.frame(beg=0,end=0,stringsAsFactors=FALSE)
  idx <- 1
  for (ct in 1:length(vecLogical)){
    pBuy <- which(vecLogical[idx:length(vecLogical)])[1] + idx-1
    if(is.na(pBuy)) break
    pSell <- which(!vecLogical[pBuy:length(vecLogical)])[1]+pBuy-1
    idx <- pSell
    res <- rbind(res,c(pBuy,pSell))
    if(is.na(pSell)) break
  }  
  res <- res[-1,]
  res$end <- res$end -1
  if (is.na(tail(res$end,1))){
    res$end[length(res$end)] <- length(vecLogical)
  }
  Check.StopIf(sum(is.na(res))>0,"Do not expect any NA")
  return(res)
}
##############################################END: data prepare functions#########


####################################################################BEGIN: GENERIC functions############################################
toUpperNoSpace <- function(stringInput){ return(toupperNoSpace(stringInput)) }
toupperNoSpace <- function(stringInput){
  Check.StopIf(!is.character(stringInput),"The input must be a string vector")
  return( toupper(gsub(" ","",stringInput)) )
}

toNoSpace <- function(stringInput){
  Check.StopIf(!is.character(stringInput),"The input must be a string vector")
  return(gsub(" ","",stringInput) )
}

out.getPercentageString <- function(vecNumeric,nrDigits){
  Check.IsScalar(nrDigits)
  Check.StopIf(!is.numeric(nrDigits),"nrDigits must be numeric!")
  Check.StopIf(!(is.numeric(vecNumeric)&is.vector(vecNumeric)),"vecNumeric must be numeric and a vector!")
  return(paste0(round(vecNumeric*100, digits=nrDigits),"%"))
}

Size <- function(matOrVec){
  if (is.vector(matOrVec)){
    res <- length(matOrVec)
  } else if (is.matrix(matOrVec)|is.data.frame(matOrVec)|is.array(matOrVec)) {
    res <- dim(matOrVec)
  } else {
    stop("size function can only handle data frame, matrix, vector or array!")
  }
  return(res)
}

Match.Robust <- function(vec2Match,refUsed){
  Check.StopIf(!(is.vector(vec2Match)&is.vector(refUsed)),"Both inputs should be vector")
  Check.Unique(refUsed)
  if(length(intersect(vec2Match,refUsed))==0){
    stop("vec2Match and refUsed do not share any common value. It's useless to apply this function!")
  }
  tmp <- setdiff(vec2Match,refUsed)
  if(length(tmp)!=0){
    print(paste0("Some values of vec2Match cannot be found in refUsed: ",paste(tmp,collapse = ", ")))
  }
  return(match(vec2Match,refUsed))
}
###################################################################################################END: GENERIC functions###############



#####################BEGIN: equity performance functions################################################################
Eq.GetMoveAvg <- function(vecPrice,vecDate,nLag){
  Check.StopIf(class(vecDate)!="Date","Must be class of Date!")
  Check.Unique(vecDate)
  Check.StopIf(!identical(vecDate,sort(vecDate)),"Must be sorted")
  Check.StopIf(!(is.vector(vecPrice)),"Both price and date should be vector")
  Check.StopIf(length(vecPrice)!=length(vecDate),"Both price and date should be have same length")
  Check.Unique(vecDate)
  Check.StopIf(!identical(sort(vecDate),vecDate),"date must be sorted from small to large")
  Check.IsScalar(nLag)
  Check.StopIf(!(is.numeric(nLag)&is.numeric(vecPrice)),"Both nLag and vecPrice must be numeric")
  Check.StopIf(length(vecPrice)<=nLag,"length of vecPrice must be minimumely as long as nLag")
  ## checking missing values!
  if(sum(is.na(vecPrice))>0) {
    print(paste0("WARNING: There are ",sum(is.na(vecPrice))," NA's in the price vector. We ignore them from the calculation!!!!!"))
    vecPrice <- vecPrice[!is.na(vecPrice)]
    vecDate <- vecDate[!is.na(vecPrice)]
  }
  ## calculation
  tmp <- rep(NA,length(vecPrice)-nLag+1) 
  for (idx in nLag:length(vecPrice)){
    tmp[idx-nLag+1] <- sum(vecPrice[(idx-nLag+1):idx])/nLag
  }
  res <- data.frame(value=tmp,stringsAsFactors=FALSE)
  res$date <- vecDate[nLag:length(vecPrice)]
  return(res[,match(c("date","value"),names(res))])
}

Eq.PrepMovAvg <- function(vecPrice,vecDate,nLag){
  mv <- Eq.GetMoveAvg(vecPrice,vecDate,nLag)
  res <- data.frame(date=vecDate,stringsAsFactors = FALSE)
  res$value <- mv$value[match(res$date,mv$date)]
  return(res)
}

Eq.PrepMovAvg2 <- function(dfInp,vecMovAvgDays){
  dfEq <- dfInp
  Check.StopIf(!is.data.frame(dfEq),"dfEq must be data frame!")
  Check.ExistVarInDF(dfEq,c("date","close","volume"))
  Check.Unique(dfEq$date)
  Check.StopIf(!identical(sort(dfEq$date),dfEq$date),"date must be sorted from small to large")
  Check.StopIf(!(is.vector(vecMovAvgDays)&is.numeric(vecMovAvgDays)),"vecMovAvgDays must be num vector")
  Check.StopIf(sum(vecMovAvgDays > 1)!=length(vecMovAvgDays),"vecMovAvgDays must be all larger than 1")
  for (idx in vecMovAvgDays){
    eval(parse(text=paste0("dfEq$priceMA",idx,"<-Eq.PrepMovAvg(dfEq$close,dfEq$date,",idx,")[,2]")))
    eval(parse(text=paste0("dfEq$volMA",idx,"<-Eq.PrepMovAvg(dfEq$volume,dfEq$date,",idx,")[,2]")))
  }
  return(dfEq)
}

Eq.CalReturnAdjacent <- function(vecPrice,vecDate){
  Check.StopIf(class(vecDate)!="Date","Must be class of Date!")
  Check.Unique(vecDate)
  Check.StopIf(!identical(vecDate,sort(vecDate)),"Must be sorted")
  Check.StopIf(!(is.vector(vecPrice)),"Both price and date should be vector")
  Check.StopIf(length(vecPrice)!=length(vecDate),"Both price and date should be have same length")
  Check.Unique(vecDate)
  Check.StopIf(!identical(sort(vecDate),vecDate),"date must be sorted from small to large")
  Check.StopIf(!(is.numeric(vecPrice)),"vecPrice must be numeric")
  ## checking missing values!
  if(sum(is.na(vecPrice))>0) {
    print(paste0("WARNING: There are ",sum(is.na(vecPrice))," NA's in the price vector. We ignore them from the calculation!!!!!"))
    vecPrice <- vecPrice[!is.na(vecPrice)]
    vecDate <- vecDate[!is.na(vecPrice)]
  }
  ## calculation
  tmp <- vecPrice[2:length(vecPrice)]/vecPrice[1:(length(vecPrice)-1)] -1
  res <- data.frame(value=tmp,stringsAsFactors=FALSE)
  res$date <- vecDate[2:length(vecPrice)]
  return(res[,match(c("date","value"),names(res))])
}

Eq.CalPortVal <- function(valInitial,vecReturn,vecDate){
  Check.StopIf(class(vecDate)!="Date","Must be class of Date!")
  Check.Unique(vecDate)
  Check.StopIf(!identical(vecDate,sort(vecDate)),"Must be sorted")
  Check.StopIf(!(is.vector(vecReturn)),"Both price and date should be vector")
  Check.StopIf(length(vecReturn)!=length(vecDate),"Both price and date should be have same length")
  Check.Unique(vecDate)
  Check.StopIf(!identical(sort(vecDate),vecDate),"date must be sorted from small to large")
  Check.IsScalar(valInitial)
  Check.StopIf(!(is.numeric(vecReturn)&is.numeric(valInitial)),"vecReturn and valInitial must be numeric")
  ## calculation
  if(sum(is.na(vecReturn))>0) {
    print(paste0("WARNING: There are ",sum(is.na(vecReturn))," NA's in the return vector. We ignore them from the calculation!!!!!"))
    vecReturn <- vecReturn[!is.na(vecReturn)]
    vecDate <- vecDate[!is.na(vecReturn)]
  }
  tmp <- 100*cumprod(1+vecReturn)
  res <- data.frame(value=tmp,stringsAsFactors=FALSE)
  res$date <- vecDate
  return(res[,match(c("date","value"),names(res))])
}

Eq.GetBuySellDates <- function(vecLogical,vecDate,nrDays2ExcuteOrder=1){
  Check.StopIf(length(vecLogical)!=length(vecDate),"Must be same length!")
  Check.StopIf(class(vecDate)!="Date","Must be class of Date!")
  Check.StopIf(nrDays2ExcuteOrder<0,"nrDays2ExcuteOrder must be >= 0!!!")
  Check.Unique(vecDate)
  Check.StopIf(!identical(vecDate,sort(vecDate)),"Must be sorted")
  pos <- DataP.GetPosBegEndTrueGroup(vecLogical)
  print(paste0("Assume we are able to +/- position at t+",nrDays2ExcuteOrder,", where t is the day signal shows"))
  ### !!!! In the following code, note that pos$end gives the last position of TRUE, thus +1 is the first position of FASLE 
  res <- data.frame(dateBuy=vecDate[pos$beg+nrDays2ExcuteOrder],  ###!!!!! here +1 shows next day buy
                    dateSell=vecDate[pos$end+1+nrDays2ExcuteOrder]) ###!!!!! here +2 shows next day of sell. Note +1 is the sell signal date
  if(is.na(tail(res$dateBuy,1))){
    res <- res[1:(dim(res)[1]-1),]
  }
  if(is.na(tail(res$dateSell,1))){
    #res$dateSell[length(res$dateSell)] <- tail(vecDate,1)
    res <- res[1:(dim(res)[1]-1),]
  }
  Check.StopIf(sum(is.na(res))>0,"Do not expect any NA now")
  return(res)
}

Eq.ImplStrategy <- function(dfEq,vecLogi,...){
  Check.StopIf(!is.data.frame(dfEq),"Must be data frame")
  Check.ExistVarInDF(dfEq,c("close","date","open"))
  ### benchmark, i.e., buy & hold
  retBench <- Eq.CalReturnAdjacent(dfEq$close,dfEq$date)
  dfInv <- Eq.GetBuySellDates(vecLogi,dfEq$date)
  Check.Unique(dfInv$dateBuy)
  Check.Unique(dfInv$dateSell)
  Check.StopIf(sum(dfInv$dateBuy > dfInv$dateSell)>0,"Error: dateBuy should be always larger than dateSell!")
  Check.StopIf(!identical(dfInv$dateBuy,sort(dfInv$dateBuy)),"Should be sequential buy dates")
  Check.StopIf(!identical(dfInv$dateSell,sort(dfInv$dateSell)),"Should be sequential sell dates")
  Check.StopIf(sum(dfInv$dateBuy[2:length(dfInv$dateBuy)] < dfInv$dateSell[1:(length(dfInv$dateSell)-1)])>0,"1st buy then sell!!")
  dfInv$priceBuy <- dfEq$open[match(dfInv$dateBuy,dfEq$date)]
  dfInv$priceSell <- dfEq$open[match(dfInv$dateSell,dfEq$date)]
  dfInv$retPeriod <- dfInv$priceSell/dfInv$priceBuy-1
  ####### here we construct daily returns according to chosen strategy
  print("This function assumes that buy/sell are made on open price! Make sense if we assume t+1 to react with signal")
  retStr <- retBench
  retStr$value <- 0
  retStr$hold <- FALSE
  for (idx in 1:dim(dfInv)[1]){
    if (identical(dfInv$dateBuy[idx],dfInv$dateSell[idx])){
      retStr$value[match(dfInv$dateBuy[idx],retStr$date)] <- dfInv$priceSell[idx]/dfInv$priceBuy[idx]-1
      retStr$hold[match(dfInv$dateBuy[idx],retStr$date)] <- TRUE
    } else {
      posSel <- which(dfEq$date >= dfInv$dateBuy[idx] & dfEq$date <= dfInv$dateSell[idx])
      # dayBuy, we enter with open price, thus return is generated at the end of dayBuy and calculated by open and close price of dayBuy
      # daySell, we sell with open price, thus return is generated at the end of dayBuy and cal by open_daySell / close_daySell-1 -1
      tmp <- c(dfEq$open[min(posSel)],dfEq$close[posSel[-length(posSel)]],dfEq$open[max(posSel)]) 
      tmp <- Eq.CalReturnAdjacent(tmp,c(dfEq$date[min(posSel)-1],dfEq$date[posSel]))
      retStr$value[match(tmp$date,retStr$date)] <- tmp$value
      retStr$hold[match(tmp$date[-length(tmp$date)],retStr$date)] <- TRUE  # the last day the sell day, i.e, do not hold 
    }
  }
  Check.StopIf(abs(prod(dfInv$retPeriod+1)-prod(1+retStr$value))>1e-6,"These two returns should be the same!!!")
  tmp <- DataP.GetPosBegEndTrueGroup(retStr$hold)
  dfInv$daysHold <- tmp[,2]-tmp[,1]+1
  ############# here we evalueate performance
  tmp <- data.frame(date=retBench$date,benchmark=retBench$value,strategy=retStr$value,stringsAsFactors=TRUE)
  tmp <- Eq.EvalPerform(tmp,retStr$hold,...)
  return(list(strInfo=dfInv,strOverview=tmp$infoOverview,xtsReturn=tmp$xtsReturn))
}

Eq.EvalPerform <- function(dfRet,vecLogi,holdTime2Sim=1250,nSim=1000){
  Check.IsScalar(holdTime2Sim)
  Check.IsScalar(nSim)
  Check.StopIf(!(is.numeric(nSim)&is.numeric(holdTime2Sim)),"holdTime2Sim and nSim Both should be numeric ")
  Check.StopIf(!is.data.frame(dfRet),"dfRet must be data frame")
  Check.ExistVarInDF(dfRet,c("date","benchmark","strategy"))
  Check.StopIf(!is.data.frame(dfRet),"dfRet must be data frame")
  Check.StopIf(class(dfRet$date)!="Date","dfRet$date Must be class of Date!")
  Check.Unique(dfRet$date)
  Check.StopIf(!identical(dfRet$date,sort(dfRet$date)),"dfRet$date Must be sorted")
  Check.StopIf(dim(dfRet)[1]!=length(vecLogi),"Must have the same nr rows")
  ixExl <- apply(is.na(dfRet[,match(c("benchmark","strategy"),names(dfRet))]),1,sum)>0
  if (sum(ixExl)>0) {
    print(paste0("dfRet has ",sum(ixExl)," rows contains at least one NA, which are excluded from the further analysis"))
    dfRet <- dfRet[!ixExl,]
    vecLogi <- vecLogi[!ixExl]
  } else if (sum(ixExl)==length(ixExl)){
    stop("dfRet contains only NA")
  }
  ##### here we evaluate performace
  tmp <- DataP.GetPosBegEndTrueGroup(vecLogi)
  daysHold <- tmp[,2]-tmp[,1]+1
  tmpXts <- xts(cbind(dfRet$benchmark,dfRet$strategy),order.by = dfRet$date)
  names(tmpXts) <- c("bench","strategy")
  par(mfrow=c(3,1))
  chart.CumReturns(tmpXts,main="Returns pver time")
  legend("topleft", c("Benchmark","Strategy"),col=c("black","red"),text.col=c("black","red"),pch='--',bty = "n",cex=0.8)
  chart.RelativePerformance(tmpXts$strategy,tmpXts$bench,main="Relative performance: strategy vs. benchmark")
  chart.Drawdown(tmpXts,main="Relative loss to the corresponding peaks")
  legend("bottomleft", c("Benchmark","Strategy"),col=c("black","red"),text.col=c("black","red"),pch='--',bty = "n",cex=0.8)
  par(mfrow=c(1,1))
  #chart.RollingPerformance(tmpXts,width=180)
  tmp <- table.CAPM(tmpXts$strategy,tmpXts$bench)
  infoOverview <- data.frame(value=c( out.getPercentageString(as.numeric(sum(vecLogi)/length(vecLogi)),0),
                                      length(daysHold),out.getPercentageString(sum(daysHold==1)/length(daysHold),0),
                                      out.getPercentageString(sum(daysHold<5)/length(daysHold),0),tmp[,1]),stringsAsFactors=FALSE)
  row.names(infoOverview) <- c("% trading days holding position ",
                               "[A] Number of times opening position ",
                               "of [A], % overnight holding , i.e., close right next day",
                               "of [A], % less-than-5-trading day holding",
                               row.names(tmp))
  ### simulation performance, 
  if(length(tmpXts$bench)-holdTime2Sim>1){
    if(holdTime2Sim != min(holdTime2Sim,floor(0.6*length(tmpXts$bench)),1250)){
      holdTime2Sim <- min(holdTime2Sim,floor(0.6*length(tmpXts$bench)),1250)
      print("The holding day has been floor to 60% of total sample size or 1250 (about 5 years)")
    }
    print(paste0("We perform simulation analysis to randomly draw a interval of ",holdTime2Sim," for ",nSim," times"))
    pos2sim <- matrix(NA,nrow=length(tmpXts$bench)-holdTime2Sim+1,ncol=2)
    for (idx in 1:(length(tmpXts$bench)-holdTime2Sim+1) ){
      pos2sim[idx,] <- c(idx,(idx+holdTime2Sim-1))
    }
    Check.StopIf(sum(pos2sim[,2]-pos2sim[,1]!=holdTime2Sim-1)+sum(pos2sim>length(tmpXts$bench))>0,"The above loop is wrong")
    realPos <- sample(1:dim(pos2sim)[1],nSim, replace = TRUE)
    resPerform <- matrix(NA,nrow=length(realPos),ncol=2)
    for (idx in 1:length(realPos)){
      resPerform[idx,] <- c(prod(1+tmpXts[pos2sim[realPos[idx],1]:pos2sim[realPos[idx],2],1])-1,
                            prod(1+tmpXts[pos2sim[realPos[idx],1]:pos2sim[realPos[idx],2],2])-1)
    }
    tmp <- data.frame(value=c(infoOverview$value,
                              paste0(paste(round(apply(resPerform,2,mean)*100,1),collapse="% vs. "),"%"),
                              paste0(paste(round(apply(resPerform,2,min)*100,1),collapse="% vs. "),"%"),
                              paste0(paste(round(apply(resPerform,2,max)*100,1),collapse="% vs. "),"%"),
                              paste0(paste(round(apply(resPerform,2,median)*100,1),collapse="% vs. "),"%"),
                              paste0(paste(round(apply(resPerform>0,2,sum)/dim(resPerform)[1]*100,1),collapse="% vs. "),"%"),
                              paste0(paste(round(apply(resPerform>0.6,2,sum)/dim(resPerform)[1]*100,1),collapse="% vs. "),"%") ),
                      stringsAsFactors=FALSE)
    row.names(tmp) <- c(row.names(infoOverview),
                        paste0("Simulation, holding time of ",holdTime2Sim," bench vs. strategy (avg return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (min return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (max return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (median return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (prob of positive return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (prob of >60% return):"))
    infoOverview <- tmp
  }
  print(infoOverview)
  return(list(infoOverview=infoOverview,xtsReturn=tmpXts))
}
################################################################################################################################END: equity performance functions###################




####################################################################################################################################################################################
###################################### the following functions are for Trade projection development################################################################################
dataP.readTradeSheet <- function(dirXlsFile,dictTradeSheet){
  Check.IsScalar(dirXlsFile)
  Check.StopIf(!is.character(dirXlsFile),"dirXlsFile must be string scalar!")
  tmp <- read.xlsx(file=dirXlsFile,sheetIndex=1,rowIndex=1,colIndex=1,header=FALSE)
  tmpHead <- sapply(read.xlsx(file=dirXlsFile,sheetIndex=1,rowIndex=5,header=FALSE), as.character)
  tmpHeadCase2 <- sapply(read.xlsx(file=dirXlsFile,sheetIndex=1,rowIndex=6,header=FALSE), as.character)
  if ( grepl("营业部",as.character(tmp[1,1])) & (grepl("交收日期",tmpHead[1])|grepl("序号",tmpHead[1])) ){
    #print("1")
    valMatRaw <- read.xlsx(file=dirXlsFile,sheetIndex=1,startRow=6,header=FALSE)
    headerRaw <- read.xlsx(file=dirXlsFile,sheetIndex=1,rowIndex=5,header=FALSE)
  } else if (grepl("营业部",as.character(tmp[1,1]))&(grepl("交收日期",tmpHeadCase2[1])|grepl("序号",tmpHeadCase2[1]))){
    #print("2")
    valMatRaw <- read.xlsx(file=dirXlsFile,sheetIndex=1,startRow=7,header=FALSE)
    headerRaw <- read.xlsx(file=dirXlsFile,sheetIndex=1,rowIndex=6,header=FALSE)
  } else if (grepl("交收日期",as.character(tmp[1,1])) | grepl("序号",as.character(tmp[1,1]))){
    #print("3")
    valMatRaw <- read.xlsx(file=dirXlsFile,sheetIndex=1,startRow=2,header=FALSE)
    headerRaw <- read.xlsx(file=dirXlsFile,sheetIndex=1,rowIndex=1,header=FALSE)
  } else{
    print(paste0("Cannot handle this type trade sheet, please check the case that cell(1,1) is ",as.character(tmp[,1])))
  }
  headerString <- sapply(headerRaw, as.character)
  resDf <- DataP.GetDfByCommonDict(valMatRaw,headerString,dictTradeSheet)
  resDf$data <- DataP.DfFactor2Character(resDf$data)
  return(resDf)
}

gtja.getEqId <- function(idInp){
  Check.StopIf(!is.vector(idInp),"idInp must be a vector!")
  tmpId <- as.character(idInp)
  naPos <- (is.na(idInp)) | (idInp=="") | (idInp==" ") | (idInp=="NA") | (idInp=="N.A.")
  ## complete the equity id
  numId <- nchar(tmpId)
  for (idx in 1:length(tmpId)){
    if ((numId[idx] < 6) & (!naPos[idx])){
      tmpId[idx] <- paste(paste(replicate(6-numId[idx],"0"),collapse=""),tmpId[idx],sep='')
    }
  }
  tmpId[naPos] <- NA
  return(tmpId)
}

gtja.getcashBalBegEndFromTradeHist <- function(inpTradHist){
  ## get cashBalance (beginning and end) of each day, given trade records history
  Check.StopIf(!is.data.frame(inpTradHist),"inpTradHist must be data frame")
  Check.ExistVarInDF(inpTradHist,c("idTradeSeq","cashBalAfterTrade","amtNet","date"))
  Check.Unique(inpTradHist$idTradeSeq)
  Check.StopIf(!identical(inpTradHist$idTradeSeq,sort(inpTradHist$idTradeSeq)),"idTradeSeq must be sequential")
  
  tmpDayEnd <- DataP.GetFieldsForUniId(inpTradHist$date,inpTradHist$idTradeSeq,inpTradHist,"max")
  if (length(inpTradHist$idTradeSeq)==1) {
    tmpBeg <- inpTradHist$cashBalAfterTrade[1]+inpTradHist$amtNet[1]
  } else{
    tmpBeg <- c(inpTradHist$cashBalAfterTrade[1]+inpTradHist$amtNet[1],
                tmpDayEnd$matPicked$cashBalAfterTrade[1:(length(tmpDayEnd$matPicked$cashBalAfterTrade)-1)])
  }
  return(data.frame(date=tmpDayEnd$idUnique,idTradeSeq=tmpDayEnd$matPicked$idTradeSeq,cashBalBeg=tmpBeg,cashBalEnd=tmpDayEnd$matPicked$cashBalAfterTrade))
}

gtja.getEndBalByVolFow <- function(dfTradeHist){
  dfHistTrade <- dfTradeHist
  Check.StopIf(!is.data.frame(dfHistTrade),"Must be a data frame")
  Check.ExistVarInDF(dfHistTrade,c("date","idEquity","volFlowPosition","idTradeSeq"))
  #Check.StopIf(class(dfHistTrade$date)!="Date","dateTrade must be in date format")
  Check.StopIf(!is.numeric(dfHistTrade$volFlowPosition),"must be numeric")
  Check.Unique(dfHistTrade$idSequence)
  Check.StopIf(!identical(dfHistTrade$idSequence,sort(dfHistTrade$idSequence)),"idSequence must be unique and sequential!")
  
  if(sum(dfHistTrade$volFlowPosition<0)==0){
    print("Input volFow shows no record to outflow. volFlowPosition is assumed that positive for inflow vol and negative for outflow")
  }
  
  if(sum(is.na(dfHistTrade$volFlowPosition))>0){
    print(paste0("volFlowPosition has ",sum(is.na(dfHistTrade$volFlowPosition))," missing values, which are replced by 0"))
    volFlowPosition[is.na(dfHistTrade$volFlowPosition)] <- 0
  }
  ### for each Security, calculate its cumsum according to trade sequence!
  # note that the combination of idTrade and dateTrade might be duplicated, since one day one can buy the same security
  # multiple times
  uniIdEq <- unique(dfHistTrade$idEquity)
  dfHistTrade$cumSum <- NA
  idx =1
  for (idx in 1:length(uniIdEq)){
    ixSel <- dfHistTrade$idEquity==uniIdEq[idx]
    tmpIx <- match(sort(dfHistTrade$idTradeSeq[ixSel]),dfHistTrade$idTradeSeq[ixSel])
    tmp <- dfHistTrade[ixSel,][tmpIx,]
    tmp$cumSum <- cumsum(dfHistTrade$volFlowPosition[ixSel][tmpIx])
    if (idx==1){
      dfRes <- tmp
    } else {
      dfRes <- rbind(dfRes,tmp)
    }
  }
  dfOut <- DataP.GetFieldsForUniId(paste(dfRes$idEquity,":",dfRes$date),dfRes$idTradeSeq,dfRes,"max")
  dfOut$matPicked$balEnd <- dfOut$matPicked$cumSum
  return(dfOut$matPicked[,match(c("date","idEquity","idTradeSeq","balEnd"),names(dfOut$matPicked))])
}


gtja.getAllHistBal <- function(dfTradeHist,dfCashBal,dictTypeTrade){
  Check.StopIf(!(is.data.frame(dfTradeHist)&is.data.frame(dfCashBal)&is.data.frame(dictTypeTrade)),
               "dfTradeHist, dfCashBal and dictTypeTrade must be data.frame")
  Check.ExistVarInDF(dfCashBal,c("date","cashBalEnd"))
  Check.ExistVarInDF(dfTradeHist,c("date","idTradeSeq","idEquity","vol","amtNet","cashBalAfterTrade","balEquity"))
  Check.ExistVarInDF(dictTypeTrade,c("flowPosition","typeTrade","balSheetItem","typeSecurity"))
  Check.Unique(dfCashBal$date)
  Check.Unique(dfTradeHist$idTradeSeq)
  Check.StopIf(!(identical(dfTradeHist$idTradeSeq,sort(dfTradeHist$idTradeSeq))),"dfTradeHist$idTradeSeq must be unique and sequential!")
  ###### dates of two df must be idential
  Check.StopIf(!identical(sort(dfCashBal$date),sort(unique(dfTradeHist$date))),"dates of two inputs must be matched! Can relaxed!!!! more code!")
  
  tmp <- toUpperNoSpace(dictTypeTrade$flowPosition[Match.Robust(dfTradeHist$typeTrade,dictTypeTrade$typeTrade)])
  print("volFlowPosition is created based on vol, i.e., positive for inflow vol and negative for outflow")
  dfTradeHist$volFlowPosition <- dfTradeHist$vol
  dfTradeHist$volFlowPosition[tmp=="OUT"] <- dfTradeHist$vol[tmp=="OUT"] * (-1)
  
  ##### we construct Repo balance seperately
  ## select Repo historical data
  ixRepo <- toUpperNoSpace(dictTypeTrade$typeSecurity[Match.Robust(dfTradeHist$typeTrade,dictTypeTrade$typeTrade)])=="REPO"
  ### select equity data
  ixEquity <- toUpperNoSpace(dictTypeTrade$balSheetItem[Match.Robust(dfTradeHist$typeTrade,dictTypeTrade$typeTrade)])==toupper("idEquity") & 
                          (!ixRepo)
  dfEqHist <- dfTradeHist[ixEquity,]
  dfBalEndEquity <- DataP.GetFieldsForUniId(paste0(dfEqHist$date,dfEqHist$idEquity),
                                 dfEqHist$idTradeSeq,dfEqHist[,match(c("date","idEquity","idTradeSeq","balEquity"),names(dfEqHist))],"max")
  names(dfBalEndEquity$matPicked)[names(dfBalEndEquity$matPicked)=="balEquity"] <- "balEnd"
  if(sum(ixRepo)==0) {
    print("No records of repo trades.")
    dfBalEnd <- rbind(dfBalEndEquity$matPicked)
  } else {
    dfBalEndRepo <- gtja.getEndBalByVolFow(dfTradeHist[ixRepo,])
    dfBalEnd <- rbind(dfBalEndEquity$matPicked,dfBalEndRepo)
  }
  ### attach cash bal @ day end
  dfCashBal$idEquity <- "cash"
  dfCashBal$balEnd <- dfCashBal$cashBalEnd
  dfBalEnd <- rbind(dfBalEnd,dfCashBal[,match(names(dfBalEnd),names(dfCashBal))])
  Check.Unique(paste0(dfBalEnd$idEquity,":",dfBalEnd$date))
  #Check.Unique(dfBalEnd$idTradeSeq)
  tmp <- sort(dfBalEnd$idTradeSeq,index.return=T)
  return(list(dfBalEndAll=dfBalEnd[tmp$ix,],dfBalEndRepo=dfBalEndRepo,dfBalEndEquity=dfBalEndEquity$matPicked,dfBalEndCash=dfCashBal))
}

gtja.getEndBalGivenDate <- function(date2show,dfHistBalEnd){
  Check.StopIf(!(class(date2show)=="Date"&length(date2show)==1),"date2show must be scalar Date class")
  Check.StopIf(!is.data.frame(dfHistBalEnd),"dfHistBalEnd must be data frame")
  Check.ExistVarInDF(dfHistBalEnd,c("date","idEquity","idTradeSeq","balEnd"))
  Check.Unique(paste0(dfHistBalEnd$date,":",dfHistBalEnd$idEquity))
  Check.StopIf(class(dfHistBalEnd$date)!="Date","dfHistBalEnd$date must be Date class")
  Check.StopIf(date2show>max(dfHistBalEnd$date)|date2show<min(dfHistBalEnd$date),
               paste0("date2show must be between ",min(dfHistBalEnd$date)," and ",max(dfHistBalEnd$date)))
  
  ixSel <- dfHistBalEnd$date <= date2show
  df2use <- dfHistBalEnd[ixSel,]
  
  tmp <- DataP.GetFieldsForUniId(df2use$idEquity,df2use$idTradeSeq,df2use,"max")
  ix2use <- abs(tmp$matPicked$balEnd)>0.01
  return(tmp$matPicked[ix2use,])
}

gtja.getHistTradePriceCost <- function(idInp,histTrade){
  #####print("This function does not take into account any cost generated after the stocks are cleanned up to zero! 
  ## e.g., dividens recerivd after the position is closed!")
  ##
  ## checks
  Check.IsScalar(idInp)
  Check.StopIf(!(is.character(idInp)&nchar(idInp)),"Must be string with 6 elements!")
  Check.ExistVarInDF(histTrade,c("idEquity","idTradeSeq","typeSecurity","date","amtNet","priceGross"))
  Check.Unique(histTrade$idTradeSeq)
  ### select relevant data 
  dfUsed <- histTrade[which(histTrade$idEquity==idInp & toupperNoSpace(histTrade$typeSecurity) %in% toupperNoSpace(c("stock","dividen"))),]
  Check.StopIf(!identical(dfUsed$idTradeSeq,sort(dfUsed$idTradeSeq)),"idTradeSeq Must be sorted")
  ### adjust balEquity for dividen records, which have balEquity of 0
  dfUsed$balEquityAdj <- dfUsed$balEquity
  if (sum(toupperNoSpace(dfUsed$typeSecurity) == toupper("dividen"))>0){
    posSel <- which(toupperNoSpace(dfUsed$typeSecurity) == toupper("dividen"))
    for (idx in posSel){
      if (idx != 1){
        dfUsed$balEquityAdj[idx] <- dfUsed$balEquityAdj[idx-1]
      }
    }
  }
  ### determine price for the stock overall
  dfUsed$amtNetCumTotal <- cumsum(dfUsed$amtNet)
  tmp <- (-dfUsed$amtNetCumTotal)/dfUsed$balEquityAdj
  tmp[!is.finite(tmp)] <- NA
  dfUsed$priceCostTotal <- tmp
  
  ### determine price of stock
  if (length(which(abs(dfUsed$balEquityAdj) < 1e-3))==0){
    ### handle the situation that all balEquity is above zero!
    dfUsed$amNetCumPerStrategy <- cumsum(dfUsed$amtNet)
    dfUsed$priceCostPerStrategy <- (-dfUsed$amNetCumPerStrategy)/dfUsed$balEquityAdj
    dfUsed$dateBegStrategy <- dfUsed$date[1]
  } else{
    posTmp <- c(0,which(abs(dfUsed$balEquityAdj) < 1e-3))
    dfUsed$amNetCumPerStrategy <- NA
    dfUsed$dateBegStrategy <- dfUsed$date[1]
    for (idx in 2:length(posTmp)){
      Check.StopIf(abs(dfUsed$balEquityAdj[posTmp[idx]])>1e-3,"Should be zero")
      dfUsed$amNetCumPerStrategy[(posTmp[idx-1]+1):(posTmp[idx])] <- cumsum(dfUsed$amtNet[(posTmp[idx-1]+1):(posTmp[idx])])
      dfUsed$dateBegStrategy[(posTmp[idx-1]+1):(posTmp[idx])] <- min(dfUsed$date[(posTmp[idx-1]+1):(posTmp[idx])])
    }
    if (tail(posTmp,1)!=dim(dfUsed)[1]){
      dfUsed$amNetCumPerStrategy[(tail(posTmp,1)+1):dim(dfUsed)[1]] <- cumsum(dfUsed$amtNet[(tail(posTmp,1)+1):dim(dfUsed)[1]])
      dfUsed$dateBegStrategy[(tail(posTmp,1)+1):dim(dfUsed)[1]] <- min(dfUsed$date[(tail(posTmp,1)+1):dim(dfUsed)[1]])
    }
    tmp <- (-dfUsed$amNetCumPerStrategy)/dfUsed$balEquityAdj
    tmp[!is.finite(tmp)] <- NA
    dfUsed$priceCostPerStrategy <- tmp
  }
  ## final adjustment for the cost at the trade of cleanning position, take the previous cost!
  if(sum(is.na(dfUsed$priceCostTotal))>0){
    pos <- which(is.na(dfUsed$priceCostTotal))
    for (idx in pos){
      if (idx!=1){
        dfUsed$priceCostTotal[idx] <- dfUsed$priceCostTotal[idx-1]
        dfUsed$priceCostPerStrategy[idx] <- dfUsed$priceCostPerStrategy[idx-1]
      }
    }
  }
  return(dfUsed[,match(c("idTradeSeq","date","idEquity","amtNet","balEquityAdj",
                         "priceGross","priceCostTotal","priceCostPerStrategy","dateBegStrategy"),names(dfUsed))])
}

gtja.getBalSingleDate <- function(dateInt,dfBalEnd,dfPriceCost,dfTrade){
  ### checks
  Check.StopIf(!(is.data.frame(dfBalEnd)&is.data.frame(dfPriceCost)&is.data.frame(dfTrade)),"dfBalEnd,dfTrade and dfPriceCost must be data frame")
  Check.IsScalar(dateInt)
  Check.StopIf(!is.character(dateInt),"dateInt must be string")
  dateInt <- try(as.Date(dateInt),silent=TRUE)
  if (class(dateInt)=="try-error"){
    stop("dateInpt must be in the format of YYYY-MM-DD ")
  }
  Check.ExistVarInDF(dfPriceCost,c("priceCostPerStrategy","idTradeSeq","dateBegStrategy"))
  Check.ExistVarInDF(dfTrade,c("typeSecurity","idTradeSeq","namEquity"))
  ixSel <- (!is.na(dfTrade$idEquity)) & (!is.na(dfTrade$namEquity))
  dictIdName <- DataP.GetFieldsForUniId(dfTrade$idEquity[ixSel],dfTrade$idTradeSeq[ixSel],dfTrade$namEquity[ixSel],"max")
  
  infoBal <- gtja.getEndBalGivenDate(dateInt,dfBalEnd)
  infoBal$date <- dateInt
  infoBal$namEquity <- dictIdName$matPicked[Match.Robust(infoBal$idEquity,dictIdName$idUnique)]
  infoBal$typeSecurity <- dfTrade$typeSecurity[Match.Robust(infoBal$idTradeSeq,dfTrade$idTradeSeq)]
  infoBal$typeSecurity[infoBal$idEquity=="cash"] <- "cash"
  infoBal$priceCostPerStrategy <- dfPriceCost$priceCostPerStrategy[Match.Robust(infoBal$idTradeSeq,dfPriceCost$idTradeSeq)]
  infoBal$dateBegStrategy <- dfPriceCost$dateBegStrategy[Match.Robust(infoBal$idTradeSeq,dfPriceCost$idTradeSeq)]
  infoStock <- unique(infoBal$idEquity[toupperNoSpace(infoBal$typeSecurity)==toupperNoSpace("stock") & infoBal$idEquity!="cash"])
  infoStock <- yahoo.getExchange(infoStock)
  infoStock$priceClose <- NA
  infoStock$priceClosePrevDay <- NA
  for (idx in 1:dim(infoStock)[1]){
    tst <- yahoo.getHistPriceSingleId(infoStock$idEquity[idx],infoStock$namExchange[idx],dateInt-15,dateInt)
    infoStock$priceClose[idx] <- tail(tst$priceClose,1)
    tmp <- which(tst$vol>0)
    if(length(tmp)>0){
      infoStock$priceClosePrevDay[idx] <- tst$priceClose[sort(tmp,decreasing=T)[2]]
    } else {
      infoStock$priceClosePrevDay[idx] <- infoStock$priceClose[idx]
    }
  }
  infoBal$daysHolding <- infoBal$date-infoBal$dateBegStrategy
  infoBal$priceClose <- infoStock$priceClose[Match.Robust(infoBal$idEquity,infoStock$idEquity)]
  infoBal$priceClosePrevDay <- infoStock$priceClosePrevDay[Match.Robust(infoBal$idEquity,infoStock$idEquity)]
  infoBal$pnlCum <- (infoBal$priceClose - infoBal$priceCostPerStrategy)* infoBal$balEnd
  infoBal$pnlDaily <- (infoBal$priceClose - infoBal$priceClosePrevDay)* infoBal$balEnd
  ixSel <- infoBal$typeSecurity !="stock" 
  print("Currently, this function cannot calculate P&L for none-stock items")
  infoBal$pnlCum[ixSel] <- 0
  infoBal$pnlDaily[ixSel] <- 0
  infoBal$value <- infoBal$priceClose * infoBal$balEnd
  ixSel <- infoBal$typeSecurity %in% c("cash","interestMoneyFund","moneyFund","capital")
  infoBal$value[ixSel] <- infoBal$balEnd[ixSel]
  ixSel <- infoBal$typeSecurity %in% c("repo")
  infoBal$value[ixSel] <- infoBal$balEnd[ixSel]*100
  return(infoBal)
}

gtja.getHistTradePriceCostAllId <- function(dfTrade) {
  Check.StopIf(!is.data.frame(dfTrade),"dfTrade must be data frame!")
  Check.ExistVarInDF(dfTrade,c("idEquity","typeSecurity"))
  
  uniIdStock <- unique(dfTrade$idEquity[toupperNoSpace(dfTrade$typeSecurity)==toupper("stock")])
  if (length(uniIdStock)==0){
    stop("The input does not contain any record of stock, according to dfTrade$typeSecurity")
  }
  print("This function does not take into account any cost generated after the stocks are cleanned up to zero! e.g., dividens recerivd after the position is closed!")
  res <- gtja.getHistTradePriceCost(uniIdStock[1],dfTrade)
  if (length(uniIdStock)>1) {
    for (idx in 2:length(uniIdStock))
    {
      res <- rbind(res,gtja.getHistTradePriceCost(uniIdStock[idx],dfTrade))
    }
  }
  return(res)
}


sina.getCurrentPriceOneId <- function(sina.id){
  Check.IsScalar(sina.id)
  tmpId <- sina.id
  Check.StopIf(!(is.character(tmpId)&nchar(tmpId)==6),"Must be character of 6 elements!")
  ## collect stock data
  sina.path <- "http://hq.sinajs.cn/list="
  sina.exchange <- "sz"
  tmp <- read.table(url(paste0(sina.path,tolower(sina.exchange),tmpId),encoding="ISO8859-1"),stringsAsFactors=TRUE)
  tmp <- tmp[,Size(tmp)[2]]
  tst <- gregexpr(",\\s*",tmp)
  if (tst[[1]][1]== -1){
    ### if not SZ market then it's from Shanghai exchange
    sina.exchange <- "sh"
    tmp <- read.table(url(paste0(sina.path,tolower(sina.exchange),tmpId),encoding="ISO8859-1"),stringsAsFactors=TRUE)
    tmp <- tmp[,Size(tmp)[2]]
    tst <- gregexpr(",\\s*",tmp)
  }
  
  ##### throw error if cannot find both SH and SZ, otherwise proceed
  if (tst[[1]][1]== -1){
    print(paste0(tmpId," cannot be found in both SZ and SH markets"))
  } else {
    pos <- str_locate_all(tmp,",")[[1]][,2]
    val <- rep(NA,length(pos)-2)
    for (idx in 2:length(pos)){
      val[idx-1] <- str_sub(tmp,pos[idx-1]+1,pos[idx]-1) 
    }
    res <- data.frame(t(as.numeric(val[1:(length(val)-2)])))
    names(res) <- c("open","closePrevious","priceCurrent","high","low","priceBuy","priceSell","vol","amt",
                    "buy1Vol","buy1Price","buy2Vol","buy2Price","buy3Vol","buy3Price","buy4Vol","buy4Price","buy5Vol","buy5Price",
                    "sell1Vol","sell1Price","sell2Vol","sell2Price","sell3Vol","sell3Price","sell4Vol","sell4Price","sell5Vol","sell5Price")
    res$date <- as.Date(val[length(val)-1],"%Y-%m-%d")
    res$time <- as.POSIXct(strptime(paste(tail(val,2),collapse=" "), "%Y-%m-%d %H:%M:%S"))
    res$idEquity <- tmpId
    res$namMarket <- sina.exchange
    return(res)
  }
}

sina.getHistPriceOneId <- function(idInp,dateBegInp,dateEndInp=Sys.Date()){
  Check.IsScalar(idInp)
  Check.StopIf(!(is.character(idInp)&nchar(idInp)==6),"Must be character of 6 elements!")
  Check.StopIf(!(class(dateBegInp)=="Date"&class(dateEndInp)=="Date"),"dateBegInp and dateEndInp are both class of Date")
  Check.StopIf(dateBegInp>dateEndInp,"dateEndInp must be larger than dateBegInp")
  
  dateBeg <- gsub("-","",as.character(dateBegInp))
  dateEnd <- gsub("-","",as.character(dateEndInp))
  tstBeg <- "http://biz.finance.sina.com.cn/stock/flash_hq/kline_data.php?&rand=random(10000)&symbol="
  tstDateEnd <- "&end_date="
  tstDateBeg <- "&begin_date="
  tstEnd <- "&type=plain"
  ##############
  tmp <- try(read.table(url(paste0(tstBeg,"sh",idInp,tstDateEnd,dateEnd,tstDateBeg,dateBeg,tstEnd)),stringsAsFactors=FALSE),silent=TRUE)
  if (class(tmp)=="try-error"){
    tmp <- try(read.table(url(paste0(tstBeg,"sz",idInp,tstDateEnd,dateEnd,tstDateBeg,dateBeg,tstEnd)),stringsAsFactors=FALSE),silent=TRUE)
    if (class(tmp)=="try-error"){
      print(paste0("Possible 1: ",idInp," cannot be found in both SZ and SH markets. Possible 2: date issues, dateBegInp is ",dateBegInp,
                   " dateEndInp is ",dateEndInp))
    }
  }
  if (class(tmp)!="try-error"){
    strMat <- unlist(strsplit(as.character(tmp[1,]),","))
    res <- sina.prepareHist(strMat)
    if (dim(tmp)[1]>1){
      for (idx in 2:dim(tmp)[1]){
        strMat <- unlist(strsplit(as.character(tmp[idx,]),","))
        res <- rbind(res,sina.prepareHist(strMat))
      }
    }
    res$idEquity <- idInp
    return(res)
  } 
}

sina.prepareHist <- function(strMat){
  return(data.frame(date = as.Date(as.character(strMat[1]),"%Y-%m-%d"),
                    open = as.double(strMat[2]),
                    high = as.double(strMat[3]),
                    close = as.double(strMat[4]),
                    low = as.double(strMat[5]),
                    vol = as.double(strMat[6])*100,
                    stringsAsFactors=FALSE))
}

yahoo.getExchange <- function(idStock){
  Check.StopIf(!(is.vector(idStock)&is.character(idStock)),"Must be string vector!")
  Check.StopIf(sum(sapply(idStock,nchar)==6)!=length(idStock),"Must all have length of 6")
  num <- as.numeric(sapply(idStock,function(x) substr(x,1,1)))
  res <- rep("ss",length(num))
  res[num<6] <- "sz"
  return(data.frame(idEquity=idStock,namExchange=res,stringsAsFactors = FALSE))
}

yahoo.getHistPriceSingleId <- function(idEq,namExchange,dateBeg,dateEnd) {
  if (class(dateBeg)!="Date"){
    Check.StopIf(!is.character(dateBeg),"dateBeg must be string")
    dateBeg <- try(as.Date(dateBeg),silent=TRUE)
    if (class(dateBeg)=="try-error"){
      stop("dateBeg must be in the format of YYYY-MM-DD ")
    }
  }
  Check.StopIf(length(dateBeg)!=1,"dateBeg Must be scalar Date class")
  if (class(dateEnd)!="Date"){
    Check.StopIf(!is.character(dateEnd),"dateEnd must be string")
    dateEnd <- try(as.Date(dateEnd),silent=TRUE)
    if (class(dateEnd)=="try-error"){
      stop("dateEnd must be in the format of YYYY-MM-DD ")
    }
  }
  Check.StopIf(length(dateEnd)!=1,"dateEnd Must be scalar Date class")
  Check.StopIf(dateEnd<dateBeg,"dateEnd Must be later than dateBeg")
  Check.IsScalar(idEq)
  Check.IsScalar(namExchange)
  Check.StopIf(!(is.character(idEq)&nchar(idEq)==6),"idEq Must be scalar character of 6")
  Check.StopIf(!is.character(namExchange),"namExchange Must be  character ")
  Check.StopIf(sum(namExchange%in%c("ss","sz"))!=1,"namExchange Must be either ss or sz! ")
  
  setSymbolLookup(WHHX=list(name=paste0(idEq,".",namExchange),src="yahoo"))
  tst <- try(getSymbols("WHHX",from=dateBeg,to=dateEnd),silent=T)
  if (class(tst)=="try-error"){
    stop(paste0(idEq,".",namExchange," cannot be found in Yahoo between ",dateBeg," and ",dateEnd,". Check idEq and dates!!!"))
  } else {
    res <- data.frame(date=index(WHHX),priceOpen=as.numeric(WHHX[,1]),priceHigh=as.numeric(WHHX[,2]),priceLow=as.numeric(WHHX[,3]),
                      priceClose=as.numeric(WHHX[,4]),vol=as.numeric(WHHX[,5]),priceAdj=as.numeric(WHHX[,6]),stringsAsFactors=FALSE)
    return(res)
  }
}
##################################################################################################################################################END: trade project###############
