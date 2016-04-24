############# load library
#library(xlsx)
library(stringr)
library(PerformanceAnalytics)


########################################################################################################################################################################
###################################################################### CHECK function development#################################################################
###################### check functions
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
    Check.StopIf(sum(dictStra$weight[dictStra$state==uniState[idx]])>1,"Weights for each state should be less than 1!")
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
  
  xtsOut <- xts(dfEq,order.by=order.by,stringsAsFactors=FALSE,...)
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
Eq.PrepHistBySym <- function(sym.int,dfEq){
  Check.IsScalar(sym.int)
  Check.StopIf(!is.character(sym.int),"Must be string scalar!")
  intVar <- c("date","open","close","state")
  Check.ExistVarInDF(dfEq,intVar)
  df <- dfEq[,match(intVar,names(dfEq))]
  names(df) <- c("date",paste0("sym_",toUpperNoSpace(sym.int),"_open"),
                 paste0("sym_",toUpperNoSpace(sym.int),"_close"),
                 paste0("sym_",toUpperNoSpace(sym.int),"_state"))
  return(df)
}

Eq.MergeTwoSymHist <- function(dfAdj1,dfAdj2,dateCommonOnly=FALSE){
  Check.ExistVarInDF(dfAdj2,"date")
  Check.ExistVarInDF(dfAdj1,"date")
  Check.StopIf(!identical(intersect(names(dfAdj1),names(dfAdj2)),"date"),"Two df should only share date as common col name!")
  Check.Unique(dfAdj2$date)
  Check.Unique(dfAdj1$date)
  Check.IsScalar(dateCommonOnly)
  Check.StopIf((!is.logical(dateCommonOnly))|is.na(dateCommonOnly),"dateCommonOnly should be either TRUE or FALSE")
  Check.StopIf(!(sum(dfAdj2$date!=sort(dfAdj2$date))==0 & sum(dfAdj1$date!=sort(dfAdj1$date))==0),"Both should be sorted and identical")
  if (dateCommonOnly) {
    print("Here, we only keep the common dates of two inputs")
    dateComm <- sort(as.Date(intersect(dfAdj1$date,dfAdj2$date)))
    out <- merge(dfAdj1,dfAdj2,by="date")
    Check.StopIf(!identical(dateComm,out$date),"Expected to be identical here!")
  } else {
    dateFull <- sort(as.Date(union(dfAdj1$date,dfAdj2$date)))
    out <- merge(dfAdj1,dfAdj2,by="date",all=TRUE)
    Check.StopIf(sum(is.na(out$date))>0,"date should not contain NA! Pls check input")
    colNoDate <- which(names(out)!="date")
    for (idx in colNoDate){
      rowNa <- which(is.na(out[,idx]))
      if(length(rowNa)>0){ ### only perform if there is NA
        for (idy in rowNa){
          if (idy==1){ # if the first position is NA, then we take the 1st noneNA
            nonNa <- which(!is.na(out[,idx])) 
            if (length(nonNa)==0){ ### handle the case all NA
              stop(paste0("All NA column. Useless to merge two df"))
            } else {
              out[idy,idx] <- out[min(nonNa),idx] # set it equal to previous value
            }
          } else { ## if NA is not the 1st observation, then just take the previous row
            out[idy,idx] <- out[idy-1,idx] # set it equal to previous value
          }
        } # END loop over rows
      }
    }# END loop over columns
  }
  return(out)
}

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
  Check.StopIf(length(vecPrice)!=length(vecDate),"!!!! ??? Both price and date should be have same length")
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

Eq.ImplStrategy <- function(dfEq,vecLogi,type.price="open",nrDays2ExcuteOrder=1,...){
  Check.StopIf(!is.data.frame(dfEq),"Must be data frame")
  Check.ExistVarInDF(dfEq,c("close","date","open"))
  ### benchmark, i.e., buy & hold
  retBench <- Eq.CalReturnAdjacent(dfEq$close,dfEq$date)
  lst <- Eq.CalRetByFlagHold(vecLogi,dfEq$date,dfEq$close,dfEq$open,type.price,nrDays2ExcuteOrder)
  ############# here we evalueate performance
  tmp <- data.frame(date=retBench$date,benchmark=retBench$value,strategy=lst$retStra$value,stringsAsFactors=TRUE)
  tmp <- Eq.EvalPerform(tmp,lst$retStra$hold,...)
  return(list(infoStra=lst$infoStra,strOverview=tmp$infoOverview,xtsReturn=tmp$xtsReturn))
}

Eq.CalRetByFlagHold <- function(flagHold,vecDate,vecClose,vecOpen,type.price="open",...){
  dfInv <- Eq.GetBuySellDates(flagHold,vecDate,...)
  Check.IsScalar(type.price)
  Check.StopIf(!is.character(type.price),"Must be string")
  Check.StopIf(!(is.vector(vecOpen)&is.vector(vecClose)),"vecOpen and vecClose must be vector")
  Check.StopIf(!(is.numeric(vecOpen)&is.numeric(vecClose)),"vecOpen and vecClose must be numeric")
  Check.StopIf(!(length(vecClose)==length(vecOpen)&length(vecDate)==length(vecClose)),"Must be same length")
  Check.ExistVarInDF(dfInv,c("dateBuy","dateSell"))
  Check.Unique(dfInv$dateBuy)
  Check.Unique(dfInv$dateSell)
  Check.StopIf(sum(dfInv$dateBuy > dfInv$dateSell)>0,"Error: dateBuy should be always larger than dateSell!")
  Check.StopIf(!identical(dfInv$dateBuy,sort(dfInv$dateBuy)),"Should be sequential buy dates")
  Check.StopIf(!identical(dfInv$dateSell,sort(dfInv$dateSell)),"Should be sequential sell dates")
  Check.StopIf(sum(dfInv$dateBuy[2:length(dfInv$dateBuy)] < dfInv$dateSell[1:(length(dfInv$dateSell)-1)])>0,"1st buy then sell!!")
  
  dfEq <- data.frame(date=vecDate,open=vecOpen,close=vecClose,stringsAsFactors=FALSE)
  if (toUpperNoSpace(type.price)==toUpperNoSpace("open")){
    dfInv$priceBuy <- dfEq$open[match(dfInv$dateBuy,dfEq$date)]
    dfInv$priceSell <- dfEq$open[match(dfInv$dateSell,dfEq$date)]
  } else if (toUpperNoSpace(type.price)==toUpperNoSpace("close")) {
    dfInv$priceBuy <- dfEq$close[match(dfInv$dateBuy,dfEq$date)]
    dfInv$priceSell <- dfEq$close[match(dfInv$dateSell,dfEq$date)]
  }  else{
    stop("Currently, only type.price of open and close is implemented. Pls expand here")
  }
  dfInv$retPeriod <- dfInv$priceSell/dfInv$priceBuy-1
  retStr <- Eq.CalReturnAdjacent(dfEq$close,dfEq$date)
  retStr$value <- 0
  retStr$hold <- FALSE
  for (idx in 1:dim(dfInv)[1]){
    posSel <- which(dfEq$date >= dfInv$dateBuy[idx] & dfEq$date <= dfInv$dateSell[idx])
    if (identical(dfInv$dateBuy[idx],dfInv$dateSell[idx])){
      print("We have intra-day holding in this case. Pls investigate if it is valid")
      retStr$value[match(dfInv$dateBuy[idx],retStr$date)] <- 
        dfEq$close[match(dfInv$dateBuy[idx],dfEq$date)]/dfEq$open[match(dfInv$dateBuy[idx],dfEq$date)]-1
      retStr$hold[match(dfInv$dateBuy[idx],retStr$date)] <- TRUE
    } else {
      if(toUpperNoSpace(type.price)==toUpperNoSpace("open")){
        ######### approach 1: buy & sell at open price!
        # dayBuy, we enter with open price, thus return is generated at the end of dayBuy and calculated by open and close price of dayBuy
        # daySell, we sell with open price, thus return is generated at the end of dayBuy and cal by open_daySell / close_daySell-1 -1
        tmp <- c(dfEq$open[min(posSel)],dfEq$close[posSel[-length(posSel)]],dfEq$open[max(posSel)]) 
        tmp <- Eq.CalReturnAdjacent(tmp,c(dfEq$date[min(posSel)-1],dfEq$date[posSel]))
        retStr$value[match(tmp$date,retStr$date)] <- tmp$value
        retStr$hold[match(tmp$date[-length(tmp$date)],retStr$date)] <- TRUE  # the last day the sell day, i.e, do not hold
      } else if (toUpperNoSpace(type.price)==toUpperNoSpace("close")) {
        ########## approach 2: buy & sell at close price
        tmp <- c(dfEq$close[min(posSel)],dfEq$close[posSel])
        tmp <- Eq.CalReturnAdjacent(tmp,c(dfEq$date[min(posSel)]-1,dfEq$date[posSel]))
        retStr$value[match(as.Date(intersect(tmp$date,retStr$date)),retStr$date)] <- 
          tmp$value[match(as.Date(intersect(tmp$date,retStr$date)),tmp$date)]
        retStr$hold[match(tmp$date[-1],retStr$date)] <- TRUE  # the last day the sell day, i.e, do not hold
      } else{
        stop("Currently, only type.price of open and close is implemented. Pls expand here")
      }
    }
  }
  Check.StopIf(abs(prod(dfInv$retPeriod+1)-prod(1+retStr$value))>1e-6,"These two returns should be the same!!!")
  tmp <- DataP.GetPosBegEndTrueGroup(retStr$hold)
  dfInv$daysHold <- tmp[,2]-tmp[,1]+1
  return(list(infoStra=dfInv,retStra=retStr))
}

Eq.AssignWeights2BuyHold <- function(dfState,strRet,infoStra,dictStra,namAssetUsed,nrDays2ExcuteOrder){
  Check.IsScalar(nrDays2ExcuteOrder)
  Check.StopIf(!(is.numeric(nrDays2ExcuteOrder) && nrDays2ExcuteOrder>=0),"nrDays2ExcuteOrder must be numberic and >= 0")
  Check.IsScalar(namAssetUsed)
  Check.ExistVarInDF(dfState,c("date","state"))
  Check.ExistVarInDF(strRet,c("date"))
  Check.ExistVarInDF(infoStra,c("dateSell"))
  Check.ExistVarInDF(dictStra,c("asset","state","weight"))
  Check.StopIf(!identical(setdiff(toupperNoSpace(namAssetUsed),toupperNoSpace(dictStra$asset)),character(0)),
               "namAssetUsed should be defined!!!")
  
  dfRet <- strRet
  tmpState <- dfState
  tmpState$state[match(infoStra$dateSell,tmpState$date)] <- 
    tmpState$state[match(infoStra$dateSell,tmpState$date)-nrDays2ExcuteOrder]
  dfRet$state <- tmpState$state[match(dfRet$date,tmpState$date)]
  tmpDict <- dictStra[toupperNoSpace(dictStra$asset)==toupperNoSpace(namAssetUsed),]
  dfRet$weight <- tmpDict$weight[Match.Robust(dfRet$state,tmpDict$state)]  
  dfRet$return <- dfRet$value * as.numeric(dfRet$weight)
  return(dfRet)
}

Eq.calRetAccountWeights <- function(dfAdj,dictStra,state.cashOnly="clean",type.price="open",nrDays2ExcuteOrder=1){
  Check.IsScalar(state.cashOnly)
  Check.StopIf(!is.character(state.cashOnly),"state.cashOnly must be string scalar")
  Check.VerifyDictStr(dictStra)
  Check.StopIf(!identical(setdiff(state.cashOnly,unique(dictStra$state)),character(0)),"state.cashOnly must be defined in dictStra")
  namAssetUsed <- toUpperNoSpace(as.character(unique(dictStra$asset)))
  ## select relevant columns of interested assets
  tmp <- rep(0,length(names(dfAdj)))
  for (idx in 1:length(namAssetUsed)){
    tmp <- tmp + grepl(namAssetUsed[idx],toUpperNoSpace(names(dfAdj)))
  }
  Check.StopIf(sum(tmp)==0,"dfAdj does not contain any asset defined in dictStra")
  dfUse <-  dfAdj[match(c("date",names(dfAdj)[tmp>0]),names(dfAdj))]
  
  # get interested asset names
  tmp <- rep(NA,length(namAssetUsed))
  for (idx in 1:length(namAssetUsed)){
    tmp[idx] <- sum(grepl(namAssetUsed[idx],toUpperNoSpace(names(dfUse))))>0
  }
  namAssetUsed <- namAssetUsed[tmp]
  if (length(namAssetUsed)<length(unique(dictStra$asset))){
    print(paste0(paste(setdiff(toUpperNoSpace(as.character(unique(dictStra$asset))),namAssetUsed),collapse=","),
                 " defined in dictionary cannot be found in the input dfAdj"))
  } 
  
  # get relevant position of interested asset names
  posClose <- which(grepl("_CLOSE",toUpperNoSpace(names(dfUse))))
  posOpen <- which(grepl("_OPEN",toUpperNoSpace(names(dfUse))))
  posState <- which(grepl("_STATE",toUpperNoSpace(names(dfUse))))
  Check.StopIf(sum(posClose)==0,"Selected data must have close price")
  Check.StopIf(sum(posOpen)==0,"Selected data must have open price")
  Check.StopIf(sum(posState)==0,"Selected data must have state")
  Check.StopIf(!(length(posState)==length(posOpen)&length(posOpen)==length(posClose)),"posState, posOpen, and posClose Must be same length")
  
  ### Step1: first determine Flag of empty holing, i.e., cash only, in next step, we allocation weights
  # Note all here we assume all strtegy is determined by the close price of each day. Hence, next open price to react!
  if (length(posState)==1) {
    flagHold <- dfUse[,posState] != state.cashOnly
  } else if (length(posState)>1){
    flagHold <- dfUse[,posState] != state.cashOnly
  } else {
    stop("length(posState) should not be 0 or negative!!!")
  }
  if (length(namAssetUsed)==1){
    tst <- Eq.CalRetByFlagHold(flagHold,dfUse$date,dfUse[,posClose[1]],dfUse[,posOpen[1]],type.price,nrDays2ExcuteOrder)
    ## adjust state dates to be consistent 
    tmpState <- data.frame(date=dfUse$date[(1+nrDays2ExcuteOrder):length(dfUse$date)],
                           state=dfUse[1:(length(dfUse$date)-nrDays2ExcuteOrder),posState[1]],stringsAsFactors=FALSE)
    strRet <- Eq.AssignWeights2BuyHold(tmpState,tst$retStra,tst$infoStra,dictStra,namAssetUsed,nrDays2ExcuteOrder)
    return(list(infoStra=tst$infoStra,retStra=strRet[,match(c("date","return","hold"),names(strRet))],retStra.extra=strRet))
  } else {
    # stop("Please extend the code for multi-assets with partial investment!!!!")
    retCol <- matrix(NA,ncol=length(namAssetUsed),nrow=length(dfUse$date)-nrDays2ExcuteOrder)
    holdCol <- retCol
    lstInfo <- vector("list", length(namAssetUsed))
    lstRet <- vector("list", length(namAssetUsed))
    for (idx in 1:length(namAssetUsed)){
      tst <- Eq.CalRetByFlagHold(flagHold[,idx],dfUse$date,dfUse[,posClose[idx]],dfUse[,posOpen[idx]],type.price,nrDays2ExcuteOrder)
      tmpState <- data.frame(date=dfUse$date[(1+nrDays2ExcuteOrder):length(dfUse$date)],
                             state=dfUse[1:(length(dfUse$date)-nrDays2ExcuteOrder),posState[idx]],stringsAsFactors=FALSE)
      strRet <- Eq.AssignWeights2BuyHold(tmpState,tst$retStra,tst$infoStra,dictStra,namAssetUsed[idx],nrDays2ExcuteOrder)
      retCol[,idx] <- strRet$return
      holdCol[,idx] <- strRet$return
      lstInfo[[idx]] <- tst$infoStra
      lstRet[[idx]] <- strRet
    }
    names(lstInfo) <- namAssetUsed
    names(lstRet) <- namAssetUsed
    Check.StopIf(sum(is.na(holdCol))>0,"Do not expect any NA in hold collection!")
    return(list(infoStra=lstInfo,
                retStra=data.frame(date=strRet$date,return=apply(retCol,1,sum),hold=apply(holdCol,1,sum)>0,stringsAsFactors=FALSE),
                retStra.extra=lstRet))
  }
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
                              paste0(paste(round(apply(resPerform>0.1,2,sum)/dim(resPerform)[1]*100,1),collapse="% vs. "),"%"),
                              paste0(paste(round(apply(resPerform>0.2,2,sum)/dim(resPerform)[1]*100,1),collapse="% vs. "),"%"),
                              paste0(paste(round(apply(resPerform>0.6,2,sum)/dim(resPerform)[1]*100,1),collapse="% vs. "),"%") ),
                      stringsAsFactors=FALSE)
    row.names(tmp) <- c(row.names(infoOverview),
                        paste0("Simulation, holding time of ",holdTime2Sim," bench vs. strategy (avg return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (min return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (max return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (median return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (prob of positive return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (prob of >10% return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (prob of >20% return):"),
                        paste0("Holding of ",holdTime2Sim," bench vs. strategy (prob of >60% return):"))
    infoOverview <- tmp
  }
  print(infoOverview)
  return(list(infoOverview=infoOverview,xtsReturn=tmpXts))
}

Tmp.PrepStateByMa <- function(df){
  dfEq <- df
  dfEq$state <- NA
  dfEq$state[dfEq$close>dfEq$priceMA20 & dfEq$priceMA5 >= dfEq$priceMA20] <- "full"
  dfEq$state[dfEq$close>dfEq$priceMA20 & dfEq$priceMA5<dfEq$priceMA20] <- "part"
  dfEq$state[dfEq$close<dfEq$priceMA20] <- "clean"
  Check.StopIf(sum(is.na(dfEq$state))>0,"Should not contain any NA. Sth is not defined")
  print(aggregate(close~state,data=dfEq,FUN=length))
  return(dfEq)
}
##############################################################################################END: equity performance functions###################


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