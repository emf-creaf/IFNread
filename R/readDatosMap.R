#' Reads map data (DatosMap)
#'
#' @name readDatosMap
#'
#' @param prov Character vector with codes of provinces (e.g. \code{c("01","03")}) or \code{"all"} for all provinces.
#' @param DBFdir String with the path where provinces are stored
#' @param plotTypeIFN3 Subset of plot types to include (see \code{\link{readPiesMayoresIFN3}})
#'
#' @return a data frame with map data
#'
readDatosMapIFN3<-function(prov, DBFdir = "DBF", plotTypeIFN3=c("A1","NN")){
  if(prov=="all") prov = .getSpainProv()
  cat(paste(prov[1],".",sep=""))
  pd<-read.dbf(paste(DBFdir,"/",prov[1],"/DAT",prov[1],".dbf",sep=""),as.is=TRUE)
  if(length(prov)>1) {
    for(i in 2:length(prov)) {
      cat(paste(prov[i],".",sep=""))
      pdi<-read.dbf(paste(DBFdir,"/",prov[i],"/DAT",prov[i],".dbf",sep=""),as.is=TRUE)
      pd<-rbind(pd,pdi)
    }
  }
  pd<-as.data.frame(pd)
  sc = pd$SUBCLASE
  sc[is.na(sc)]<-""
  pdtype = paste(pd$CLA,sc, sep="")
  sel = pdtype %in% plotTypeIFN3
  pd <-pd[sel,]
  pd$ID<-as.numeric(as.character(pd$PROVINCIA))*10000+as.numeric(as.character(pd$ESTADILLO))
  if(length(pd$ID)==length(unique(pd$ID))) rownames(pd)<-pd$ID
  return(pd)
}
