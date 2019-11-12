#' Reads plot data (PC parcelas)
#'
#' @name readPCParcela
#'
#' @param prov Character vector with codes of provinces (e.g. \code{c("01","03")}) or \code{"all"} for all provinces.
#' @param DBFdir String with the path where provinces are stored
#' @param plotIDs String vector with a selection of plots
#'
#' @return a data frame with plot data
#'
readPCParcelaIFN2<-function(prov, DBFdir = "DBF", plotIDs = NULL){
  if(prov=="all") prov = .getSpainProv()
  pd<-read.dbf(paste(DBFdir,"/",prov[1],"/DATEST",prov[1],".dbf",sep=""))
  cat(paste(prov[1],".",sep=""))
  if(length(prov)>1) for(i in 2:length(prov)) {
    pd<-merge(pd,read.dbf(paste(DBFdir,"/",prov[i],"/DATEST",prov[i],".dbf",sep="")), all=TRUE, sort=FALSE)
    cat(paste(prov[i],".",sep=""))
  }
  pd$ID<-as.character(as.numeric(as.character(pd$PROVINCIA))*10000+as.numeric(as.character(pd$ESTADILLO)))
  if(!is.null(plotIDs)) {
    sel = (pd$ID %in% as.character(plotIDs))
    pd <-pd[sel,]
  }

  if(length(pd$ID)==length(unique(pd$ID))) rownames(pd)<-pd$ID
  return(pd)
}


#' @rdname readPCParcela
#'
#' @param plotTypeIFN3 Subset of plot types to include (see \code{\link{readPiesMayoresIFN3}})
#'
readPCParcelaIFN3<-function(prov, DBFdir = "DBF", plotTypeIFN3=c("A1","NN"), plotIDs = NULL){
  if(prov=="all") prov = .getSpainProv()
  cat(paste(prov[1],".",sep=""))
  pd<-read.dbf(paste(DBFdir,"/",prov[1],"/PCPARC",prov[1],".dbf",sep=""),as.is=TRUE)
  pd$FECHAPH<-as.Date(pd$FECHAINI, format="%d/%m/%y")
  pd$FECHAINI<-as.Date(pd$FECHAINI, format="%d/%m/%y")
  pd$FECHAFIN<-as.Date(pd$FECHAFIN, format="%d/%m/%y")
  pd$HORAPH <-as.character(pd$HORAPH)
  pd$HORAINI <-as.character(pd$HORAINI)
  pd$HORAFIN<-as.character(pd$HORAFIN)
  if(length(prov)>1) {
    for(i in 2:length(prov)) {
      cat(paste(prov[i],".",sep=""))
      pdi<-read.dbf(paste(DBFdir,"/",prov[i],"/PCPARC",prov[i],".dbf",sep=""),as.is=TRUE)
      pdi$FECHAPH<-as.Date(pdi$FECHAINI, format="%d/%m/%y")
      pdi$FECHAINI<-as.Date(pdi$FECHAINI, format="%d/%m/%y")
      pdi$FECHAFIN<-as.Date(pdi$FECHAFIN, format="%d/%m/%y")
      pdi$HORAPH <-as.character(pdi$HORAPH)
      pdi$HORAINI <-as.character(pdi$HORAINI)
      pdi$HORAFIN<-as.character(pdi$HORAFIN)
      pd<-merge(pd,pdi, all=TRUE, sort=FALSE)
    }
  }
  pd$ID<-as.character(as.numeric(as.character(pd$PROVINCIA))*10000+as.numeric(as.character(pd$ESTADILLO)))

  #Selection
  sc = pd$SUBCLASE
  sc[is.na(sc)]<-""
  pdtype = paste(pd$CLA,sc, sep="")
  sel = pdtype %in% plotTypeIFN3
  if(!is.null(plotIDs)) sel = sel | (pd$ID %in% as.character(plotIDs))
  pd <-pd[sel,]
  if(length(pd$ID)==length(unique(pd$ID))) rownames(pd)<-pd$ID
  return(pd)
}

#' @rdname readPCParcela
#' @param accessFiles A character vector of access files to be read.
#' @param plotTypeIFN4 Subset of plot types to include (see \code{\link{readPiesMayoresIFN3}})
#'
readPCParcelaIFN4<-function(accessFiles, plotTypeIFN4=c("A1","N"), plotIDs = NULL){
  cat(".")
  ch<-RODBC::odbcConnectAccess2007(accessFiles[1])
  pd<-RODBC::sqlFetch(ch, "PCParcelas")
  close(ch)
  # pd$FechaIni<-as.Date(pd$FechaIni)
  # pd$FechaFin<-as.Date(pd$FechaFin)
  if(length(accessFiles)>1) {
    for(i in 2:length(accessFiles)) {
      cat(".")
      chi<-RODBC::odbcConnectAccess2007(accessFiles[i])
      pdi<-RODBC::sqlFetch(chi, "PCParcelas")
      close(chi)
      pd<-merge(pd,pdi, all=TRUE, sort=FALSE)
    }
  }
  #subset Estadillo if necessary
  nc = nchar(as.character(pd$Estadillo))
  pd$Estadillo = substr(as.character(pd$Estadillo),nc-3,nc)
  #Determine ID
  pd$ID<-as.character(as.numeric(as.character(pd$Provincia))*10000+as.numeric(as.character(pd$Estadillo)))

  #Selection
  sc = as.character(pd$Subclase)
  sc[is.na(sc)]<-""
  pdtype = paste(pd$Cla,sc, sep="")
  sel = pdtype %in% plotTypeIFN4
  if(!is.null(plotIDs)) sel = sel | (pd$ID %in% as.character(plotIDs))

  pd <-pd[sel,]
  if(length(pd$ID)==length(unique(pd$ID))) rownames(pd)<-pd$ID
  return(pd)
}

