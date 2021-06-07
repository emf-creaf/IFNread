#' Read shrub data
#'
#' Reads shrub data from MSAccess database
#'
#' @param accessFiles A character vector of access files to be read.
#' @param provincias Numeric or character vector with the province numbers corresponding to \code{accessFiles}
#' @param plotType Subset of plot types to include:
#' @param plotIDs String vector with a selection of plots
#' @param rem.nospecies Flag to remove records without species identity
#' @param height.cm Flag to return height in 'cm' instead of 'meters'
#' @param subsetVars Flag to filter data columns
#'
#' @return a data frame with shrub data
#'
readMatorral<-function(accessFiles, provincias = NULL, plotType=c("A1","NN"),
                       plotIDs = NULL, rem.nospecies=FALSE, height.cm = FALSE,
                       subsetVars=TRUE){
  cat(paste("Reading",accessFiles[1],"...\n"))
  ch<-RODBC::odbcConnectAccess2007(accessFiles[1])
  shrubData<-RODBC::sqlFetch(ch,"PCMatorral")
  if(!is.null(provincias)) shrubData$Provincia = provincias[1]
  close(ch)

  if(length(accessFiles)>1){
    for(i in 2:length(accessFiles)){
      cat(paste("Reading",accessFiles[i],"...\n"))
      chi<-RODBC::odbcConnectAccess2007(accessFiles[i])
      tdi<-RODBC::sqlFetch(chi,"PCMatorral")
      if(!is.null(provincias)) tdi$Provincia = provincias[i]
      close(chi)
      shrubData = merge(shrubData, tdi, all=TRUE, sort=FALSE)
    }
  }
  sc = as.character(shrubData$Subclase)
  sc[is.na(sc)]<-""
  pdtype = paste(shrubData$Cla,sc, sep="")
  shrubData$TYPE  = pdtype


  if("Provincia" %in% names(shrubData)) shrubData$ID<-as.character(as.numeric(as.character(shrubData$Provincia))*10000+as.numeric(as.character(shrubData$Estadillo)))
  else {
    shrubData$ID<-as.character(as.numeric(as.character(shrubData$Estadillo)))
    nc = nchar(as.character(shrubData$Estadillo))
    ncI = nc-3
    shrubData$Provincia = substr(as.character(shrubData$Estadillo),1,nc-4)
    shrubData$Estadillo = substr(as.character(shrubData$Estadillo),ncI,nc)
  }


  #Selection
  sel = rep(FALSE, nrow(shrubData))
  if(!is.null(plotType)) sel = sel | (pdtype %in% plotType)
  if(!is.null(plotIDs)) sel = sel | (shrubData$ID %in% as.character(plotIDs))
  shrubData <-shrubData[sel,]


  #Translate meters to cms
  if(height.cm) shrubData$Hm = as.numeric(as.character(shrubData$Hm))*10 #dm to cms
  else shrubData$Hm = as.numeric(as.character(shrubData$Hm))/10 #dm to m
  if(subsetVars){
    n = names(shrubData)
    shrubData <- shrubData[,c(which(n=="Provincia"),which(n=="Estadillo"),
                              which(n=="ID"),which(n=="Especie"),
                              which(n=="Fcc"), which(n=="Hm"))]
    names(shrubData)<-c("Provincia", "Estadillo","ID","Species","FCC", "H")
  }
  shrubData$REG<-0
  return(shrubData)
}

#' @rdname readMatorral
readMatorralIFN2<-function(prov, DBFdir = "DBF",
                           height.cm = FALSE, subsetVars=TRUE){
  provNum = as.numeric(prov)
  shrubDataIFN2<-read.dbf(paste(DBFdir,"/",prov[1],"/MATORR",prov[1],".dbf",sep=""))
  cat(paste(prov[1],".",sep=""))
  shrubDataIFN2$PROVINCIA = provNum[1]
  if(length(prov)>1){
    for(i in 2:length(prov)){
      td<-read.dbf(paste(DBFdir,"/",prov[i],"/MATORR",prov[i],".dbf",sep=""))
      cat(paste(prov[i],".",sep=""))
      td$PROVINCIA = provNum[i]
      shrubDataIFN2 = merge(shrubDataIFN2, td, all=TRUE, sort=FALSE)
    }
  }
  shrubDataIFN2$ESPECIE = as.numeric(as.character(shrubDataIFN2$ESPECIE))
  shrubDataIFN2$ID<-shrubDataIFN2$PROVINCIA*10000+as.numeric(as.character(shrubDataIFN2$ESTADILLO))
  shrubDataIFN2$ALTUMED = sub(",",".",as.character(shrubDataIFN2$ALTUMED))
  shrubDataIFN2$ALTUMED[shrubDataIFN2$ALTUMED=="NA"]<-NA
  shrubDataIFN2$ALTUMED <- as.numeric(shrubDataIFN2$ALTUMED)
  ### SPECIFY SELECTION CRITERIA #####
  #Translate meters to cms
  if(height.cm) shrubDataIFN2$ALTUMED = shrubDataIFN2$ALTUMED*10 #dm to cms
  else shrubDataIFN2$ALTUMED = as.numeric(as.character(shrubDataIFN2$ALTUMED))/10 #dm to m
  if(subsetVars){
    n = names(shrubDataIFN2)
    shrubDataIFN2 <- shrubDataIFN2[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),
                                      which(n=="ESPECIE"),which(n=="FRACCAB"), which(n=="ALTUMED"))]
    names(shrubDataIFN2)<-c("Provincia", "Estadillo","ID","Species","FCC", "H")
  }
  shrubDataIFN2$REG<-0
  return(shrubDataIFN2)
}

