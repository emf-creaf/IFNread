#' Reads the table of pies menores
#'
#'
#' @name readPiesMayores
#'
#' @param prov Character vector with codes of provinces (e.g. \code{c("01","03")}) or \code{"all"} for all provinces.
#' @param DBFdir String with the path where provinces are stored
#' @param subsetVars Flag to filter data columns
#'
#' @return a data frame
#'
readPiesMenoresIFN2<-function(prov, DBFdir = "DBF",subsetVars=TRUE){
  provNum = as.numeric(prov)
  regTreeDataIFN2<-read.dbf(paste(DBFdir,"/",prov[1],"/PIESME",prov[1],".dbf",sep=""))
  cat(paste(prov[1],".",sep=""))
  regTreeDataIFN2$PROVINCIA = provNum[1]
  if(length(prov)>1){
    for(i in 2:length(prov)){
      td<-read.dbf(paste(DBFdir,"/",prov[i],"/PIESME",prov[i],".dbf",sep=""))
      cat(paste(prov[i],".",sep=""))
      td$PROVINCIA = provNum[i]
      regTreeDataIFN2 = merge(regTreeDataIFN2, td, all=TRUE, sort=FALSE)
    }
  }
  factor<-c(127.3239546, 31.83098865,14.14710607, 5.092958185)
  regTreeDataIFN2$ID<-regTreeDataIFN2$PROVINCIA*10000+as.numeric(as.character(regTreeDataIFN2$ESTADILLO))
  regTreeDataIFN2$NUMERO<-as.numeric(as.character(regTreeDataIFN2$NUMERO))*factor[1]
  regTreeDataIFN2$ESPECIE<-as.numeric(as.character(regTreeDataIFN2$ESPECIE))
  regTreeDataIFN2$DG = 5
  #Remove records with no height and number of individuals
  regTreeDataIFN2<-regTreeDataIFN2[!is.na(regTreeDataIFN2$NUMERO),]
  regTreeDataIFN2<-regTreeDataIFN2[!regTreeDataIFN2$NUMERO==0,]
  regTreeDataIFN2$ALTUMED = as.numeric(as.character(regTreeDataIFN2$ALTUMED))/10  #dm to m
  regTreeDataIFN2<-regTreeDataIFN2[!is.na(regTreeDataIFN2$ALTUMED),]
  ### SPECIFY SELECTION CRITERIA #####
  if(subsetVars){
    n = names(regTreeDataIFN2)
    regTreeDataIFN2 <- regTreeDataIFN2[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),which(n=="ESPECIE"),
                                          which(n=="NUMERO"), which(n=="DG"), which(n=="ALTUMED"))]
    names(regTreeDataIFN2) <- c("Provincia", "Estadillo", "ID","Species","N","DBH","H")
  }
  regTreeDataIFN2$OIF2<-"-1"
  return(regTreeDataIFN2)
}
