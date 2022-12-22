#' Functions to read IFN2 data
#'
#'
#' @param prov Character vector with codes of provinces (e.g. \code{c("01","03")}).
#' @param DBFdir String with the path where provinces are stored
#' @param renameVars Flag to rename data columns to make them similar to IFN3/IFN4
#'
#' @return a data frame
#' @name readIFN2
readPiesMenoresIFN2<-function(prov, DBFdir = "DBF",renameVars=TRUE){
  df_list <- vector("list", length(prov))
  for(i in 1:length(prov)){
    td<-read.dbf(paste(DBFdir,"/",prov[i],"/PIESME",prov[i],".DBF",sep=""))
    td$PROVINCIA = prov[i]
    df_list <- as_tibble(td)
  }
  regTreeDataIFN2<-bind_rows(df_list)

  factor<-c(127.3239546, 31.83098865,14.14710607, 5.092958185)
  regTreeDataIFN2$ID<-as.character(as.numeric(regTreeDataIFN2$PROVINCIA)*10000+as.numeric(regTreeDataIFN2$ESTADILLO))
  regTreeDataIFN2$NUMERO<-as.numeric(as.character(regTreeDataIFN2$NUMERO))*factor[1]
  regTreeDataIFN2$ESPECIE<-as.character(regTreeDataIFN2$ESPECIE)
  regTreeDataIFN2$DG = 5
  #Remove records with no height and number of individuals
  regTreeDataIFN2<-regTreeDataIFN2[!is.na(regTreeDataIFN2$NUMERO),]
  regTreeDataIFN2<-regTreeDataIFN2[!regTreeDataIFN2$NUMERO==0,]
  regTreeDataIFN2$ALTUMED = as.numeric(as.character(regTreeDataIFN2$ALTUMED))/10  #dm to m
  regTreeDataIFN2<-regTreeDataIFN2[!is.na(regTreeDataIFN2$ALTUMED),]
  ### SPECIFY SELECTION CRITERIA #####
  if(renameVars){
    n = names(regTreeDataIFN2)
    regTreeDataIFN2 <- regTreeDataIFN2[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),which(n=="ESPECIE"),
                                          which(n=="NUMERO"), which(n=="DG"), which(n=="ALTUMED"))]
    names(regTreeDataIFN2) <- c("Provincia", "Estadillo", "ID","Species","N","DBH","H")
  }
  return(regTreeDataIFN2)
}

#' @rdname readIFN2
readMatorralIFN2<-function(prov, DBFdir = "DBF", renameVars = TRUE){
  df_list <- vector("list", length(prov))
  for(i in 1:length(prov)){
    td<-read.dbf(paste(DBFdir,"/",prov[i],"/MATORR",prov[i],".DBF",sep=""))
    td$PROVINCIA = prov[i]
    df_list[[i]] <- as_tibble(td)
  }
  shrubDataIFN2<-bind_rows(df_list)

  #Check Estadillo characters
  shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==1] <- paste0("0000",shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==1])
  shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==2] <- paste0("000",shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==2])
  shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==3] <- paste0("00",shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==3])
  shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==4] <- paste0("0",shrubDataIFN2$ESTADILLO[nchar(shrubDataIFN2$ESTADILLO)==4])

  #Define ID
  shrubDataIFN2$ID<-paste0(shrubDataIFN2$PROVINCIA,shrubDataIFN2$ESTADILLO)

  #Check variable types
  shrubDataIFN2$ESPECIE = as.character(shrubDataIFN2$ESPECIE)
  shrubDataIFN2$ALTUMED = sub(",",".",as.character(shrubDataIFN2$ALTUMED))
  shrubDataIFN2$ALTUMED[shrubDataIFN2$ALTUMED=="NA"]<-NA
  shrubDataIFN2$ALTUMED <- as.numeric(shrubDataIFN2$ALTUMED)
  shrubDataIFN2$FRACCAB <- as.numeric(as.character(shrubDataIFN2$FRACCAB))

  if(renameVars) {
    n = names(shrubDataIFN2)
    shrubDataIFN2 <- shrubDataIFN2[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),
                                      which(n=="ESPECIE"),which(n=="FRACCAB"), which(n=="ALTUMED"))]
    names(shrubDataIFN2)<-c("Provincia", "Estadillo","ID","Species","Fcc", "Hm")
  }

  return(shrubDataIFN2)
}

#' @rdname readIFN2
readPiesMayoresIFN2<-function(prov, DBFdir = "DBF", renameVars=TRUE){

  df_list <- vector("list", length(prov))
  for(i in 1:length(prov)){
    td<-read.dbf(paste(DBFdir,"/",prov[i],"/PIESMA",prov[i],".DBF",sep=""))
    td$PROVINCIA <- prov[i]
    df_list[[i]] <- as_tibble(td)
  }
  treeDataIFN2<-bind_rows(df_list)


  #Check Estadillo characters
  treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==1] <- paste0("0000",treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==1])
  treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==2] <- paste0("000",treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==2])
  treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==3] <- paste0("00",treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==3])
  treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==4] <- paste0("0",treeDataIFN2$ESTADILLO[nchar(treeDataIFN2$ESTADILLO)==4])

  #Define ID
  treeDataIFN2$ID <- paste0(treeDataIFN2$PROVINCIA, treeDataIFN2$ESTADILLO)

  treeDataIFN2$NUMORDEN = as.character(treeDataIFN2$NUMORDEN)
  treeDataIFN2$NUMORDEN[nchar(treeDataIFN2$NUMORDEN)==1] <- paste0("00",treeDataIFN2$NUMORDEN[nchar(treeDataIFN2$NUMORDEN)==1])
  treeDataIFN2$NUMORDEN[nchar(treeDataIFN2$NUMORDEN)==2] <- paste0("0",treeDataIFN2$NUMORDEN[nchar(treeDataIFN2$NUMORDEN)==2])

  treeDataIFN2$ESPECIE <- as.character(treeDataIFN2$ESPECIE)
  treeDataIFN2$DISTANCI <- sub(",",".",as.character(treeDataIFN2$DISTANCI))
  treeDataIFN2$DISTANCI[treeDataIFN2$DISTANCI=="NA"]<-NA
  treeDataIFN2$DISTANCI <- as.numeric(treeDataIFN2$DISTANCI)
  treeDataIFN2$ALTURA <- sub(",",".",as.character(treeDataIFN2$ALTURA))
  treeDataIFN2$ALTURA[treeDataIFN2$ALTURA=="NA"]<-NA
  treeDataIFN2$ALTURA <- as.numeric(treeDataIFN2$ALTURA)
  treeDataIFN2$DIAMETRO1 = as.character(treeDataIFN2$DIAMETRO1)
  treeDataIFN2$DIAMETRO1[treeDataIFN2$DIAMETRO1=="NA"]<-NA
  treeDataIFN2$DIAMETRO1 <- as.numeric(treeDataIFN2$DIAMETRO1)
  treeDataIFN2$DIAMETRO2 <- as.character(treeDataIFN2$DIAMETRO2)
  treeDataIFN2$DIAMETRO2[treeDataIFN2$DIAMETRO2=="NA"]<-NA
  treeDataIFN2$DIAMETRO2 <- as.numeric(treeDataIFN2$DIAMETRO2)

  if(renameVars){
    n <- names(treeDataIFN2)
    treeDataIFN2 <- treeDataIFN2[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),
                                    which(n=="NUMORDEN"),
                                    which(n=="ESPECIE"), which(n=="DISTANCI"), which(n=="DIAMETRO1"), which(n=="DIAMETRO2"),
                                    which(n=="ALTURA"))]
    names(treeDataIFN2)<-c("Provincia", "Estadillo","ID",
                           "OrdenIfn2",
                           "Species", "Distanci", "Dn1", "Dn2",
                           "Ht")
  }
  return(treeDataIFN2)
}

#' @rdname readIFN2
readPCParcelaIFN2<-function(prov, DBFdir = "DBF", renameVars = TRUE){

  df_list <- vector("list", length(prov))
  for(i in 1:length(prov)) {
    pd <- read.dbf(paste(DBFdir,"/",prov[1],"/DATEST",prov[1],".DBF",sep=""))
    pd$ID <- as.character(as.numeric(as.character(pd$PROVINCIA))*10000+as.numeric(as.character(pd$ESTADILLO)))
    if(!is.null(plotIDs)) {
      sel <- (pd$ID %in% as.character(plotIDs))
      pd <-pd[sel, , drop=FALSE]
    }
    df_list[[i]] <- as_tibble(pd)
  }
  return(bind_rows(df_list))
}
