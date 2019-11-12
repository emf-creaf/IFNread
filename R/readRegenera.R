#' Read regeneration data
#'
#' Reads regeneration data from MSAccess database (IFN3-IFN4)
#'
#' @name readRegenera
#'
#' @param accessFiles A character vector of access files to be read.
#' @param provincias Numeric or character vector with the province numbers corresponding to \code{accessFiles}
#' @param plotType Subset of plot types to include:
#' @param plotIDs String vector with a selection of plots
#' @param rem.nospecies Flag to remove records without species identity
#' @param subsetVars Flag to filter data columns
#'
#' @return a data frame with regeneration data
#'
readRegenera<-function(accessFiles, provincias = NULL, plotType=c("A1","NN"),
                       plotIDs = NULL, rem.nospecies=FALSE, subsetVars=TRUE){

  cat(paste("Reading",accessFiles[1],"...\n"))
  ch<-RODBC::odbcConnectAccess2007(accessFiles[1])
  regData<-RODBC::sqlFetch(ch,"PCRegenera")
  if(!is.null(provincias)) regData$Provincia = provincias[1]
  close(ch)

  if(length(accessFiles)>1){
    for(i in 2:length(accessFiles)){
      cat(paste("Reading",accessFiles[i],"...\n"))
      chi<-RODBC::odbcConnectAccess2007(accessFiles[i])
      tdi<-RODBC::sqlFetch(chi,"PCRegenera")
      if(!is.null(provincias)) tdi$Provincia = provincias[i]
      close(chi)
      regData = merge(regData, tdi, all=TRUE, sort=FALSE)
    }
  }
  sc = as.character(regData$Subclase)
  sc[is.na(sc)]<-""
  pdtype = paste(regData$Cla,sc, sep="")
  regData$TYPE  = pdtype


  if(!is.null(provincias)) regData$ID<-as.character(as.numeric(as.character(regData$Provincia))*10000+as.numeric(as.character(regData$Estadillo)))
  else {
    regData$ID<-as.character(as.numeric(as.character(regData$Estadillo)))
    nc = nchar(as.character(regData$Estadillo))
    ncI = nc-3
    regData$Provincia = substr(as.character(regData$Estadillo),1,nc-4)
    regData$Estadillo = substr(as.character(regData$Estadillo),ncI,nc)
  }

  #Selection
  sel = rep(FALSE, nrow(regData))
  if(!is.null(plotType)) sel = sel | (pdtype %in% plotType)
  if(!is.null(plotIDs)) sel = sel | (regData$ID %in% as.character(plotIDs))
  regData <-regData[sel,]

  regData$Tipo<-as.character(regData$Tipo)
  regData$Especie<-as.character(regData$Especie)
  if(rem.nospecies) regData<-regData[!is.na(regData$Especie),]
  if(subsetVars){
    n = names(regData)
    regData <- regData[,c(which(n=="ID"),which(n=="Especie"),which(n=="CatDes"), which(n=="Densidad"),
                                  which(n=="NumPies"), which(n=="Hm"))]
  }
  return(regData)
}


#' @rdname readRegenera
#'
#' @param regData A data frame returned by \code{readRegenera}
#' @param height.cm Flag to return height in 'cm' instead of 'meters'
#'
extractRegTreeData<-function(regData, height.cm=TRUE, subsetVars=TRUE){
  sel4 = (regData$CatDes==4)
  #Separa Pies menores (REG=1)
  regTreeData1<-regData[sel4,]
  regTreeData1$DG = 5
  regTreeData1$NumPies<-as.numeric(as.character(regTreeData1$NumPies))*127.3239546
  if(height.cm) regTreeData1$Hm = regTreeData1$Hm*10 #Translate from dm to cm
  if(subsetVars){
    n = names(regTreeData1)
    regTreeData1 <- regTreeData1[,c(which(n=="ID"),which(n=="Especie"),which(n=="NumPies"), which(n=="DG"),
                                          which(n=="Hm"))]
    names(regTreeData1)<-c("ID","Especie","N","DG","Ht")
  }
  regTreeData1$REG<-1

  #OTHER REGENERATING (REG=2)
  regTreeData2<-regData[!sel4,]
  NCat = c(2.5,10,20) #1 a 4 / 5 a 15 / >15
  HtCat = c(10,80,150) #< 30 cm / 30 - 130 cm / > 130 cm
  DGCat = c(0.1,0.5,1.5) # ? / ? / < 2.5 cm
  regTreeData2$N = NCat[regTreeData2$Densidad]*127.3239546
  regTreeData2$DG = DGCat[regTreeData2$CatDes]
  regTreeData2$Ht = HtCat[regTreeData2$CatDes]
  if(subsetVars){
    n = names(regTreeData2)
    regTreeData2 <- regTreeData2[,c(which(n=="ID"),which(n=="Especie"),which(n=="N"), which(n=="DG"),
                                  which(n=="Ht"))]
    names(regTreeData2)<-c("ID","Especie","N","DG","Ht")
  }
  regTreeData2$REG<-2
  return(rbind(regTreeData1, regTreeData2))
}
