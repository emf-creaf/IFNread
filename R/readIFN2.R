#' Functions to read IFN2 data
#'
#'
#' @param prov Character vector with codes of provinces (e.g. \code{c("01","03")}).
#' @param DBFdir String with the path where provinces are stored
#' @param height.cm Flag to return height in 'cm' instead of 'meters'
#' @param rem.nodensity Flag to remove records without density
#' @param rem.nospecies Flag to remove records without species identity
#' @param rem.noheight Flag to remove records without tree height
#' @param subsetVars Flag to filter data columns
#' @param plotIDs String vector with a selection of plots
#'
#' @return a data frame
#' @name readIFN2
readPiesMenoresIFN2<-function(prov, DBFdir = "DBF",subsetVars=TRUE){
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
  if(subsetVars){
    n = names(regTreeDataIFN2)
    regTreeDataIFN2 <- regTreeDataIFN2[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),which(n=="ESPECIE"),
                                          which(n=="NUMERO"), which(n=="DG"), which(n=="ALTUMED"))]
    names(regTreeDataIFN2) <- c("Provincia", "Estadillo", "ID","Species","N","DBH","H")
  }
  return(regTreeDataIFN2)
}

#' @rdname readIFN2
readMatorralIFN2<-function(prov, DBFdir = "DBF",
                           height.cm = FALSE, subsetVars=TRUE){
  df_list <- vector("list", length(prov))
  for(i in 1:length(prov)){
    td<-read.dbf(paste(DBFdir,"/",prov[i],"/MATORR",prov[i],".DBF",sep=""))
    td$PROVINCIA = prov[i]
    df_list[[i]] <- as_tibble(td)
  }
  shrubDataIFN2<-bind_rows(df_list)

  shrubDataIFN2$ID<-as.character(as.numeric(shrubDataIFN2$PROVINCIA)*10000+as.numeric(shrubDataIFN2$ESTADILLO))
  shrubDataIFN2$ESPECIE = as.character(shrubDataIFN2$ESPECIE)
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
  return(shrubDataIFN2)
}

#' @rdname readIFN2
readPiesMayoresIFN2<-function(prov, DBFdir = "DBF", plotIDs = NULL,
                              rem.nodensity=FALSE, rem.nospecies=FALSE, rem.noheight=FALSE,
                              height.cm = FALSE, subsetVars=TRUE){
  df_list <- vector("list", length(prov))
  for(i in 1:length(prov)){
    td<-read.dbf(paste(DBFdir,"/",prov[i],"/PIESMA",prov[i],".DBF",sep=""))
    td$PROVINCIA = prov[i]
    df_list[[i]] <- as_tibble(td)
  }
  treeDataIFN2<-bind_rows(df_list)

  treeDataIFN2$ID<- as.character(as.numeric(treeDataIFN2$PROVINCIA)*10000+as.numeric(treeDataIFN2$ESTADILLO))

  #Selection
  sel<-rep(TRUE, nrow(treeDataIFN2))
  if(!is.null(plotIDs)) sel <- sel & (treeDataIFN2$ID %in% as.character(plotIDs))
  treeDataIFN2 <-treeDataIFN2[sel,]


  treeDataIFN2$ESPECIE<-as.character(treeDataIFN2$ESPECIE)
  treeDataIFN2$DISTANCI = sub(",",".",as.character(treeDataIFN2$DISTANCI))
  treeDataIFN2$DISTANCI[treeDataIFN2$DISTANCI=="NA"]<-NA
  treeDataIFN2$DISTANCI <- as.numeric(treeDataIFN2$DISTANCI)
  treeDataIFN2$ALTURA = sub(",",".",as.character(treeDataIFN2$ALTURA))
  treeDataIFN2$ALTURA[treeDataIFN2$ALTURA=="NA"]<-NA
  treeDataIFN2$ALTURA <- as.numeric(treeDataIFN2$ALTURA)
  treeDataIFN2$DIAMETRO1 = as.character(treeDataIFN2$DIAMETRO1)
  treeDataIFN2$DIAMETRO1[treeDataIFN2$DIAMETRO1=="NA"]<-NA
  treeDataIFN2$DIAMETRO1 <- as.numeric(treeDataIFN2$DIAMETRO1)
  treeDataIFN2$DIAMETRO2 = as.character(treeDataIFN2$DIAMETRO2)
  treeDataIFN2$DIAMETRO2[treeDataIFN2$DIAMETRO2=="NA"]<-NA
  treeDataIFN2$DIAMETRO2 <- as.numeric(treeDataIFN2$DIAMETRO2)
  treeDataIFN2$DM<-(treeDataIFN2$DIAMETRO1+treeDataIFN2$DIAMETRO2)/(2*10)
  treeDataIFN2$N<-.densityFactor(treeDataIFN2$DM)
  # SPECIFY SELECTION CRITERIA #
  # remove tree when we lack Species  #
  if(rem.nospecies) treeDataIFN2<-treeDataIFN2[!is.na(treeDataIFN2$ESPECIE),]
  # remove tree when we lack N  #
  if(rem.nodensity) treeDataIFN2<-treeDataIFN2[!is.na(treeDataIFN2$N),]

  #Translate meters to cms
  if(height.cm) treeDataIFN2$ALTURA = treeDataIFN2$ALTURA*100 #meters to cms
  if(rem.noheight) treeDataIFN2<-treeDataIFN2[!is.na(treeDataIFN2$ALTURA),]
  if(subsetVars){
    n = names(treeDataIFN2)
    treeDataIFN2 <- treeDataIFN2[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),which(n=="ESPECIE"),which(n=="N"), which(n=="DM"),
                                    which(n=="ALTURA"), which(n=="NUMORDEN"))]
    names(treeDataIFN2)<-c("Provincia", "Estadillo","ID","Species", "N", "DBH","H", "OIF2")
  }
  return(treeDataIFN2)
}

#' @rdname readIFN2
readPCParcelaIFN2<-function(prov, DBFdir = "DBF", plotIDs = NULL){

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
