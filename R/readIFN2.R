


#' Functions to read IFN2 data
#'
#'
#' @param prov Character vector with codes of provinces (e.g. \code{c("01","03")}).
#' @param source_path String with the path where provinces are stored
#' @param rename_vars Flag to rename data columns to make them similar to IFN3/IFN4
#'
#' @return a data frame
#' @name readIFN2
readPiesMenoresIFN2<-function(prov, source_path = "DBF",rename_vars=TRUE){
  #Read data
  df <- .readIFN2Tables(prov, source_path, "PIESME")

  df$ESPECIE <- .checkSpecies(df$ESPECIE)

  df$NUMERO <- as.numeric(df$NUMERO)

  df$ALTUMED <- sub(",",".",as.character(df$ALTUMED))
  df$ALTUMED[df$ALTUMED=="NA"]<-NA
  df$ALTUMED <- as.numeric(df$ALTUMED)

  ### SPECIFY SELECTION CRITERIA #####
  if(rename_vars){
    n <- names(df)
    df <- df[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),which(n=="ESPECIE"),
                which(n=="NUMERO"), which(n=="ALTUMED"))]
    names(df) <- c("Provincia", "Estadillo", "ID","Especie","Numero","Hm")
  }
  return(df)
}

#' @rdname readIFN2
readMatorralIFN2<-function(prov, source_path = "DBF", rename_vars = TRUE){
  #Read data
  df <- .readIFN2Tables(prov, source_path, "MATORR")

  #Check variable types
  df$ESPECIE <- .checkSpecies(df$ESPECIE)

  df$ALTUMED <- sub(",",".",as.character(df$ALTUMED))
  df$ALTUMED[df$ALTUMED=="NA"]<-NA
  df$ALTUMED <- as.numeric(df$ALTUMED)

  df$FRACCAB <- sub(",",".",as.character(df$FRACCAB))
  df$FRACCAB[df$FRACCAB=="NA"]<-NA
  df$FRACCAB <- as.numeric(df$FRACCAB)

  if(rename_vars) {
    n <- names(df)
    df <- df[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),
                                      which(n=="ESPECIE"),which(n=="FRACCAB"), which(n=="ALTUMED"))]
    names(df)<-c("Provincia", "Estadillo","ID","Especie","Fcc", "Hm")
  }

  return(df)
}

#' @rdname readIFN2
readPiesMayoresIFN2<-function(prov, source_path = "DBF", rename_vars=TRUE){

  #Read data
  df <- .readIFN2Tables(prov, source_path, "PIESMA")

  df$ESPECIE <- .checkSpecies(df$ESPECIE)

  df$NUMORDEN = as.character(df$NUMORDEN)
  df$NUMORDEN[nchar(df$NUMORDEN)==1] <- paste0("00",df$NUMORDEN[nchar(df$NUMORDEN)==1])
  df$NUMORDEN[nchar(df$NUMORDEN)==2] <- paste0("0",df$NUMORDEN[nchar(df$NUMORDEN)==2])

  df$DISTANCI <- sub(",",".",as.character(df$DISTANCI))
  df$DISTANCI[df$DISTANCI=="NA"]<-NA
  df$DISTANCI <- as.numeric(df$DISTANCI)

  df$ALTURA <- sub(",",".",as.character(df$ALTURA))
  df$ALTURA[df$ALTURA=="NA"]<-NA
  df$ALTURA <- as.numeric(df$ALTURA)

  df$DIAMETRO1 <- as.character(df$DIAMETRO1)
  df$DIAMETRO1[df$DIAMETRO1=="NA"]<-NA
  df$DIAMETRO1 <- as.numeric(df$DIAMETRO1)

  df$DIAMETRO2 <- as.character(df$DIAMETRO2)
  df$DIAMETRO2[df$DIAMETRO2=="NA"]<-NA
  df$DIAMETRO2 <- as.numeric(df$DIAMETRO2)

  if(rename_vars){
    n <- names(df)
    df <- df[,c(which(n=="PROVINCIA"),which(n=="ESTADILLO"),which(n=="ID"),
                                    which(n=="NUMORDEN"),
                                    which(n=="ESPECIE"), which(n=="DISTANCI"), which(n=="DIAMETRO1"), which(n=="DIAMETRO2"),
                                    which(n=="ALTURA"))]
    names(df)<-c("Provincia", "Estadillo","ID",
                           "OrdenIfn2",
                           "Especie", "Distanci", "Dn1", "Dn2",
                           "Ht")
  }
  return(df)
}

#' @rdname readIFN2
readPCParcelaIFN2<-function(prov, source_path = "DBF", rename_vars = TRUE){

  #Read data
  df <- .readIFN2Tables(prov, source_path, "DATEST")

  return(df)
}
