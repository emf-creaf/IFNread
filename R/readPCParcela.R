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
#' @param source_path Path to the location of BBDD-Campo
#' @param ifn Either 3 or 4 (for IFN3 or IFN4, respectively)
#' @param prov Character vector with province codes (e.g. c("03", "14", "25"))
#' @param ccaa Character vector with names of autonomous communities (e.g. "Catal)
#' @param plotType Subset of plot types to include (see \code{\link{readPiesMayoresIFN3}})
#'
readPCParcela<-function(source_path, ifn = 3, prov = NULL, ccaa = NULL,
                        plotType = NULL){
  colClasses3 <- c("Provincia" = "character",
                   "Estadillo" = "character",
                   "Cla" = "character",
                   "Subclase" = "character",
                   "Tipo" = "character",
                   "Vuelo1" = "character",
                   "Vuelo2" = "character",
                   "MejVue1" = "character",
                   "MejVue2" = "character",
                   "MejSue1" = "character",
                   "MejSue2" = "character",
                   "Acceso" = "character",
                   "CortaReg" = "character",
                   "DistFoto" = "character",
                   "Pasada1" = "character",
                   "Pasada2" = "character",
                   "Foto1" = "character",
                   "Foto2" = "character",
                   "Nivel1" = "character",
                   "Nivel2" = "character",
                   "Nivel3" = "character",
                   "Resid" = "character",
                   "RumboF1" = "character",
                   "RumboF2" = "character",
                   "HoraPh" = "character",
                   "HoraIni" = "character",
                   "HoraFin" = "character",
                   "Estado" = "character",
                   "Ano" = "character",
                   "INE" = "character",
                   "Rocosid" = "character",
                   "DisEsp" = "character")

  colClasses4 <- c("Provincia" = "character",
                   "Estadillo" = "character",
                   "Cla" = "character",
                   "Subclase" = "character",
                   "Tipo" = "character",
                   "Vuelo1" = "character",
                   "Vuelo2" = "character",
                   "MejVue1" = "character",
                   "MejVue2" = "character",
                   "MejSue1" = "character",
                   "MejSue2" = "character",
                   "Acceso" = "character",
                   "CortaReg" = "character",
                   "DistFoto" = "character",
                   "Pasada1" = "character",
                   "Pasada2" = "character",
                   "Foto1" = "character",
                   "Foto2" = "character",
                   "Nivel1" = "character",
                   "Nivel2" = "character",
                   "Nivel3" = "character",
                   "RumboF1" = "character",
                   "RumboF2" = "character",
                   "HoraPh" = "character",
                   "HoraIni" = "character",
                   "HoraFin" = "character",
                   "Estado" = "character",
                   "Ano" = "character",
                   "INE" = "character",
                   "Rocosid" = "character",
                   "DisEsp" = "character")
  colClasses <- ifelse(ifn==3, colClasses3, colClasses4)
  f_date<- function(dateString) {
    date <- NULL
    try(date <- as.POSIXct(dateString), silent = TRUE)
    if(is.null(date)) {
      s <- strsplit(dateString, " ")
      dateString <- sapply(s, function(x) x[1])
      try(date <- as.POSIXct(dateString), silent = TRUE)
    }
    if(is.null(date)) {
      s <- strsplit(dateString, "/")
      d <- sapply(s, function(x) x[1])
      m <- sapply(s, function(x) {ifelse(length(x)>=2, x[[2]], NA)})
      y <- sapply(s, function(x) {ifelse(length(x)>=3, x[[3]], NA)})
      y[(!is.na(y)) & (nchar(y)==2)] <- paste0("19", y[(!is.na(y)) & (nchar(y)==2)])
      dateString <- paste0(y,"/", m, "/",d)
      dateString[is.na(y)] = NA
      tryCatch(date <- as.POSIXct(dateString))
    }
    return(date)
  }

  filenames <- character(0)
  if(!is.null(prov)) {
    for(i in 1:length(prov)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "p", prov[i]),"PCParcelas.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
    }
  }
  if(!is.null(ccaa)) {
    for(i in 1:length(ccaa)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "_", ccaa[i]),"PCParcelas.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
    }
  }
  df_list <- vector("list", length(filenames))
  for(i in 1:length(filenames)) {
    fn <- filenames[i]
    pd <- read.csv(fn, sep = "\t", dec = ".",
                   colClasses = colClasses,
                   na.strings = "")
    if("FechaPh" %in% names(pd)) pd$FechaPh<-f_date(pd$FechaPh)
    if("FechaIni" %in% names(pd)) pd$FechaIni<-f_date(pd$FechaIni)
    if("FechaFin" %in% names(pd)) pd$FechaFin<-f_date(pd$FechaFin)

    pd$Provincia[nchar(pd$Provincia)==1] <- paste0("0", pd$Provincia[nchar(pd$Provincia)==1])
    names(pd)[names(pd)=="Coory"]  <- "CoorY"
    if("CoorX" %in% names(pd)) pd$CoorX  <- as.numeric(pd$CoorX)
    if("CoorY" %in% names(pd)) pd$CoorY  <- as.numeric(pd$CoorY)

    # Plot selection
    if(!is.null(plotType)) {
      sc <- pd$Subclase
      sc[is.na(sc)]<-""
      pdtype <- paste(pd$Cla,sc, sep="")
      sel <- pdtype %in% plotType
      pd <- pd[sel,,drop = FALSE]
    }

    # Add ID and order variables
    pd$ID <- paste0(pd$Provincia, pd$Estadillo, "_", pd$Cla, pd$Subclase)
    vars <- c("Provincia", "Estadillo", "Cla", "Subclase", "ID")
    pd <- pd[,c(vars, names(pd)[!(names(pd) %in% vars)]), drop = FALSE]
    df_list[[i]]<- as_tibble(pd)
  }
  return(bind_rows(df_list))
}


