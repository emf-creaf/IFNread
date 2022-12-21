#' Extract regeneration data from IFN3/4
#'
#' @param regData A data frame returned by \code{readRegenera}
#' @param height.cm Flag to return height in 'cm' instead of 'meters'
#' @param subsetVars Flag to filter data columns
#'
extractRegTreeData<-function(regData, height.cm=FALSE, subsetVars=TRUE){
  sel4 = (regData$CatDes==4)
  #Separa Pies menores (REG=1)
  regTreeData1<-regData[sel4,]
  regTreeData1$DBH = 5
  regTreeData1$NumStems<-as.numeric(as.character(regTreeData1$NumStems))*127.3239546
  if(height.cm) regTreeData1$H = regTreeData1$H*10 #Translate from dm to cm
  else regTreeData1$H = regTreeData1$H/10 #Translate from dm to m
  if(subsetVars){
    n = names(regTreeData1)
    regTreeData1 <- regTreeData1[,c(which(n=="ID"),which(n=="Species"),which(n=="NumStems"), which(n=="DBH"),
                                          which(n=="H"))]
    names(regTreeData1)<-c("ID","Species","N","DBH","H")
  }
  regTreeData1$REG<-1

  #OTHER REGENERATING (REG=2)
  regTreeData2<-regData[!sel4,]
  NCat = c(2.5,10,20) #1 a 4 / 5 a 15 / >15
  if(height.cm) HtCat = c(10,80,150) #< 30 cm / 30 - 130 cm / > 130 cm
  else HtCat = c(0.1,0.80,1.5)
  DGCat = c(0.1,0.5,1.5) # ? / ? / < 2.5 cm
  regTreeData2$N = NCat[regTreeData2$Density]*127.3239546
  regTreeData2$DBH = DGCat[regTreeData2$CatDes]
  regTreeData2$H = HtCat[regTreeData2$CatDes]
  if(subsetVars){
    n = names(regTreeData2)
    regTreeData2 <- regTreeData2[,c(which(n=="ID"),which(n=="Species"),which(n=="N"), which(n=="DBH"),
                                  which(n=="H"))]
    names(regTreeData2)<-c("ID","Species","N","DBH","H")
  }
  regTreeData2$REG<-2
  return(rbind(regTreeData1, regTreeData2))
}

