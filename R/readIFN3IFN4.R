#' Reads IFN3 and IFN4 data
#'
#' @param source_path Path to the location of BBDD-Campo
#' @param ifn Either 3 or 4 (for IFN3 or IFN4, respectively)
#' @param prov Character vector with province codes (e.g. c("03", "14", "25"))
#' @param ccaa Character vector with names of autonomous communities (e.g. "Catal)
#' @param plotType Subset of plot types to include:
#' \itemize{
#' \item{N - Se levantan por primera vez en el IFN-3 y son utilizadas para el calculo de existencias actuales.}
#' \item{A - Parcelas levantadas en el IFN-2 que volveran a ser apeadas en el IFN-3. Existen cinco casos:
#'   \itemize{
#'     \item{
#'        A1 . Parcelas en las que se localice el rejon y esta bien implantado. Se utilizan para la comparacion de inventarios y para el calculo de existencias actuales.
#'     }
#'     \item{
#'        A3 . Parcela apeada en IFN2, pinchazo IFN2 y coordenada UTM situadas en diferentes teselas. Se cumplimentan dos estadillos:
#'        \itemize{
#'          \item{
#'            A3C . Parcela para la Comparacion de Inventarios. Se levantan en el punto donde se encuentra el rejon del IFN-2.
#'          }
#'          \item{
#'            A3E. Parcelas para el calculo de Existencias Actuales. Se levantan en el emplazamiento del IFN-3 ( Coordenada UTM)
#'          }
#'        }
#'     }
#'     \item{
#'       A4 . Parcelas en las que no se localizo el rejon. Se utilizan para el calculo de existencias actuales.
#'     }
#'     \item{
#'       A4C . Parcelas en las que no se localice el rej?n por cambio totales en la cubierta forestal de la tesela que contiene la parcela (p?rdida total de la masa
#' muestreada en el IFN2 o incorporaci?n de una nueva masa forestal inexistente en el IFN2). Se utilizar?n para la comparaci?n de inventarios y para el c?lculo de existencias actuales.
#'     }
#'     \item{
#'       A6C . Parcelas en las que no se localice el rej?n por cambios totales en la
#' cubierta forestal de la tesela que contiene la parcela (p?rdida total de la masa
#' muestreada en el IFN2 e incorporaci?n de una nueva masa forestal). Se utilizar?n para la comparaci?n de inventarios y para el c?lculo de existencias actuales.
#'     }
#'   }
#' }
#' }
#'
#' @return a data frame with IFN data, depending on the function
#'
#' @name readIFN3IFN4
readDatosMap<-function(source_path, ifn = 3, prov = NULL, ccaa = NULL,
                       plotType = NULL){
  colClasses <- c("Provincia" = "character",
                  "Estadillo" = "character",
                  "Especie" = "character",
                  "Cla" = "character",
                  "Clase" = "character",
                  "Subclase" = "character",
                  "Hoja50" = "character",
                  "CoorX" = "numeric",
                  "CoorY" = "numeric",
                  "INE" = "character",
                  "Nivel1" = "character",
                  "Nivel2" = "character",
                  "Nivel3" = "character",
                  "Nivel4" = "character",
                  "Nivel5" = "character",
                  "Nivel6" = "character",
                  "FccTot" = "character",
                  "FccArb" = "character",
                  "DisEsp" = "character",
                  "ComEsp" = "character",
                  "Estado" = "character")

  filenames <- character(0)
  isProv <- logical(0)
  if(!is.null(prov)) {
    for(i in 1:length(prov)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "p", prov[i]),"PCDatosMap.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, TRUE)
    }
  }
  if(!is.null(ccaa)) {
    for(i in 1:length(ccaa)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "_", ccaa[i]),"PCDatosMap.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, FALSE)
    }
  }
  df_list <- vector("list", length(filenames))
  for(i in 1:length(filenames)) {
    fn <- filenames[i]
    pd <- read.csv(fn, sep = "\t", dec = ".",
                   colClasses = colClasses,
                   na.strings = c("", " ", "  ","NA"))


    names(pd)[names(pd)=="Cla"] <- "Clase"
    #Check Estadillo
    pd$Estadillo <- .checkEstadillo(pd$Estadillo)
    pd$Clase <- .checkClase(pd$Clase)
    pd$Subclase <- .checkSubclase(pd$Subclase)

    # Province
    if(isProv[i]) {
      if(!("Provincia" %in% names(pd))) pd$Provincia <- prov[i]
    }
    pd$Provincia[nchar(pd$Provincia)==1] <- paste0("0", pd$Provincia[nchar(pd$Provincia)==1])


    # Plot selection
    if(!is.null(plotType)) {
      sc <- pd$Subclase
      sc[is.na(sc)]<-""
      pdtype <- paste(pd$Clase,sc, sep="")
      sel <- pdtype %in% plotType
      pd <- pd[sel,,drop = FALSE]
    }

    # Add IDs and order variables
    pd$IDPARCELA <- paste0(pd$Provincia, pd$Estadillo)
    pd$IDCLASE <- paste0(pd$Clase, pd$Subclase)
    pd$ID = paste0(pd$IDPARCELA,"_", pd$IDCLASE)
    vars <- c("Provincia", "Estadillo", "Clase", "Subclase", "IDPARCELA", "IDCLASE" ,"ID")
    pd <- pd[,c(vars, names(pd)[!(names(pd) %in% vars)]), drop = FALSE]
    df_list[[i]]<- as_tibble(pd)
  }
  return(bind_rows(df_list))
}

#' @rdname readIFN3IFN4
readMatorral<-function(source_path, ifn = 3, prov = NULL, ccaa = NULL,
                       plotType = NULL){
  colClasses <- c("Estadillo" = "character",
                  "Cla" = "character",
                  "Subclase" = "character",
                  "Especie" = "character",
                  "Fcc" = "numeric",
                  "Hm" = "numeric")

  filenames <- character(0)
  isProv <- logical(0)
  if(!is.null(prov)) {
    for(i in 1:length(prov)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "p", prov[i]),"PCMatorral.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, TRUE)
    }
  }
  if(!is.null(ccaa)) {
    for(i in 1:length(ccaa)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "_", ccaa[i]),"PCMatorral.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, FALSE)
    }
  }
  df_list <- vector("list", length(filenames))
  for(i in 1:length(filenames)) {
    fn <- filenames[i]
    pd <- read.csv(fn, sep = "\t", dec = ".",
                   colClasses = colClasses,
                   na.strings = c("", " ","NA"))

    names(pd)[names(pd)=="Cla"] <- "Clase"
    #Check Estadillo
    pd$Estadillo <- .checkEstadillo(pd$Estadillo)
    pd$Clase <- .checkClase(pd$Clase)
    pd$Subclase <- .checkSubclase(pd$Subclase)

    # Province
    if(isProv[i]) {
      if(!("Provincia" %in% names(pd))) pd$Provincia <- prov[i]
    }
    pd$Provincia[nchar(pd$Provincia)==1] <- paste0("0", pd$Provincia[nchar(pd$Provincia)==1])


    # Species names
    pd$Especie <- .checkSpecies(pd$Especie)

    # Plot selection
    if(!is.null(plotType)) {
      sc <- pd$Subclase
      sc[is.na(sc)]<-""
      pdtype <- paste(pd$Clase,sc, sep="")
      sel <- pdtype %in% plotType
      pd <- pd[sel,,drop = FALSE]
    }

    # Add IDs and order variables
    pd$IDPARCELA <- paste0(pd$Provincia, pd$Estadillo)
    pd$IDCLASE <- paste0(pd$Clase, pd$Subclase)
    pd$ID = paste0(pd$IDPARCELA,"_", pd$IDCLASE)
    vars <- c("Provincia", "Estadillo", "Clase", "Subclase", "IDPARCELA", "IDCLASE" ,"ID")
    pd <- pd[,c(vars, names(pd)[!(names(pd) %in% vars)]), drop = FALSE]
    df_list[[i]]<- as_tibble(pd)
  }
  return(bind_rows(df_list))
}

#' @rdname readIFN3IFN4
readRegenera<-function(source_path, ifn = 3, prov = NULL, ccaa = NULL,
                       plotType = NULL){
  colClasses <- c("Estadillo" = "character",
                  "Especie" = "character",
                  "Cla" = "character",
                  "Clase" = "character",
                  "Subclase" = "character",
                  "CatDes" = "character",
                  "Tipo" = "character",
                  "Densidad" = "character",
                  "NumPies" = "numeric",
                  "Hm" = "numeric")

  filenames <- character(0)
  isProv <- logical(0)
  if(!is.null(prov)) {
    for(i in 1:length(prov)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "p", prov[i]),"PCRegenera.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, TRUE)
    }
  }
  if(!is.null(ccaa)) {
    for(i in 1:length(ccaa)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "_", ccaa[i]),"PCRegenera.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, FALSE)
    }
  }
  df_list <- vector("list", length(filenames))
  for(i in 1:length(filenames)) {
    fn <- filenames[i]
    pd <- read.csv(fn, sep = "\t", dec = ".",
                   colClasses = colClasses,
                   na.strings = c("", " ","NA"))


    names(pd)[names(pd)=="Cla"] <- "Clase"
    #Check Estadillo
    pd$Estadillo <- .checkEstadillo(pd$Estadillo)
    pd$Clase <- .checkClase(pd$Clase)
    pd$Subclase <- .checkSubclase(pd$Subclase)

    # Province
    if(isProv[i]) {
      if(!("Provincia" %in% names(pd))) pd$Provincia <- prov[i]
    }
    pd$Provincia[nchar(pd$Provincia)==1] <- paste0("0", pd$Provincia[nchar(pd$Provincia)==1])

    # Species names
    pd$Especie <- .checkSpecies(pd$Especie)

    # Plot selection
    if(!is.null(plotType)) {
      sc <- pd$Subclase
      sc[is.na(sc)]<-""
      pdtype <- paste(pd$Clase,sc, sep="")
      sel <- pdtype %in% plotType
      pd <- pd[sel,,drop = FALSE]
    }

    # Add IDs and order variables
    pd$IDPARCELA <- paste0(pd$Provincia, pd$Estadillo)
    pd$IDCLASE <- paste0(pd$Clase, pd$Subclase)
    pd$ID = paste0(pd$IDPARCELA,"_", pd$IDCLASE)
    vars <- c("Provincia", "Estadillo", "Clase", "Subclase", "IDPARCELA", "IDCLASE" ,"ID")
    pd <- pd[,c(vars, names(pd)[!(names(pd) %in% vars)]), drop = FALSE]
    df_list[[i]]<- as_tibble(pd)
  }
  return(bind_rows(df_list))
}

#' @rdname readIFN3IFN4
readPiesMayores<-function(source_path, ifn = 3, prov = NULL, ccaa = NULL,
                          plotType = NULL){
  colClasses3 <- c("Estadillo" = "character",
                   "Cla" = "character",
                   "Clase" = "character",
                   "Subclase" = "character",
                   "nArbol" = "numeric",
                   "OrdenIf3" = "character",
                   "OrdenIf2" = "character",
                   "Rumbo" = "numeric",
                   "Distanci" = "numeric",
                   "Especie" = "character",
                   "Dn1" = "numeric",
                   "Dn2" = "numeric",
                   "Ht" = "numeric",
                   "Calidad" = "character",
                   "Forma" = "character",
                   "ParEsp" = "character",
                   "Agente" = "character",
                   "Import" = "character",
                   "Elemento" = "character",
                   "Compara" = "character")

  colClasses4 <- c("Estadillo" = "character",
                   "Cla" = "character",
                   "Clase" = "character",
                   "Subclase" = "character",
                   "nArbol" = "numeric",
                   "OrdenIf4" = "character",
                   "OrdenIf3" = "character",
                   "Rumbo" = "numeric",
                   "Distanci" = "numeric",
                   "Especie" = "character",
                   "Dn1" = "numeric",
                   "Dn2" = "numeric",
                   "Ht" = "numeric",
                   "Calidad" = "character",
                   "Forma" = "character",
                   "ParEsp" = "character",
                   "Agente" = "character",
                   "Import" = "character",
                   "Elemento" = "character",
                   "Compara" = "character")
  colClasses <- colClasses3
  if(ifn == 4) colClasses <- colClasses4

  filenames <- character(0)
  isProv <- logical(0)
  if(!is.null(prov)) {
    for(i in 1:length(prov)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "p", prov[i]),"PCMayores.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, TRUE)
    }
  }
  if(!is.null(ccaa)) {
    for(i in 1:length(ccaa)){
      fn <- file.path(source_path,paste0("Ifn",ifn, "_", ccaa[i]),"PCMayores.csv")
      if(!file.exists(fn)) stop("File not found: '", fn,"'")
      filenames <- c(filenames, fn)
      isProv <-c(isProv, FALSE)
    }
  }
  df_list <- vector("list", length(filenames))
  for(i in 1:length(filenames)) {
    fn <- filenames[i]
    pd <- read.csv(fn, sep = "\t", dec = ".",
                   colClasses = colClasses,
                   na.strings = c("", " ","NA"))


    names(pd)[names(pd)=="Cla"] <- "Clase"
    #Check Estadillo
    pd$Estadillo <- .checkEstadillo(pd$Estadillo)
    pd$Clase <- .checkClase(pd$Clase)
    pd$Subclase <- .checkSubclase(pd$Subclase)


    # Province
    if(isProv[i]) {
      if(!("Provincia" %in% names(pd))) pd$Provincia <- prov[i]
    }
    pd$Provincia[nchar(pd$Provincia)==1] <- paste0("0", pd$Provincia[nchar(pd$Provincia)==1])

    # Species names
    pd$Especie <- .checkSpecies(pd$Especie)

    pd$Rumbo <- as.numeric(pd$Rumbo)
    pd$Distanci <- as.numeric(pd$Distanci)
    pd$Dn1 <- as.numeric(pd$Dn1)
    pd$Dn2 <- as.numeric(pd$Dn2)
    pd$Ht <- as.numeric(pd$Ht)

    # Plot selection
    if(!is.null(plotType)) {
      sc <- pd$Subclase
      sc[is.na(sc)]<-""
      pdtype <- paste(pd$Clase,sc, sep="")
      sel <- pdtype %in% plotType
      pd <- pd[sel,,drop = FALSE]
    }

    # Add IDs and order variables
    pd$IDPARCELA <- paste0(pd$Provincia, pd$Estadillo)
    pd$IDCLASE <- paste0(pd$Clase, pd$Subclase)
    pd$ID = paste0(pd$IDPARCELA,"_", pd$IDCLASE)
    vars <- c("Provincia", "Estadillo", "Clase", "Subclase", "IDPARCELA", "IDCLASE" ,"ID")
    pd <- pd[,c(vars, names(pd)[!(names(pd) %in% vars)]), drop = FALSE]
    df_list[[i]]<- as_tibble(pd)
  }
  return(bind_rows(df_list))
}


#' @rdname readIFN3IFN4
readPCParcela<-function(source_path, ifn = 3, prov = NULL, ccaa = NULL,
                        plotType = NULL){
  colClasses3 <- c("Provincia" = "character",
                   "Estadillo" = "character",
                   "Cla" = "character",
                   "Clase" = "character",
                   "Subclase" = "character",
                   "Tipo" = "character",
                   "Vuelo1" = "character",
                   "Pasada1" = "character",
                   "Foto1" = "character",
                   "Vuelo2" = "character",
                   "Pasada2" = "character",
                   "Foto2" = "character",
                   "Ano" = "character",
                   "INE" = "character",
                   "Nivel1" = "character",
                   "Nivel2" = "character",
                   "Nivel3" = "character",
                   "FccTot" = "numeric",
                   "FccArb" = "numeric",
                   "DisEsp" = "character",
                   "ComEsp" = "character",
                   "Rocosid" = "character",
                   "Textura" = "character",
                   "MatOrg" = "character",
                   "PhSuelo" = "character",
                   "FechaPh" = "character",
                   "HoraPh" = "character",
                   "TipSuelo1" = "character",
                   "TipSuelo2" = "character",
                   "TipSuelo3" = "character",
                   "MErosiva" = "character",
                   "EspCMue" = "character",
                   "PresReg" = "character",
                   "EfecReg" = "character",
                   "CortaReg" = "character",
                   "MejVue1" = "character",
                   "MejVue2" = "character",
                   "MejSue1" = "character",
                   "MejSue2" = "character",
                   "Orienta1" = "numeric",
                   "Orienta2" = "numeric",
                   "MaxPend1" = "numeric",
                   "MaxPend2" = "numeric",
                   "Localiza" = "character",
                   "Acceso" = "character",
                   "Levanta" = "character",
                   "Obser" = "character",
                   "Equipo" = "character",
                   "JefeEq" = "character",
                   "FechaIni" = "character",
                   "HoraIni" = "character",
                   "FechaFin" = "character",
                   "DistFoto" = "character",
                   "HoraFin" = "character",
                   "Tiempo" = "numeric",
                   "Resid" = "character",
                   "RumboF1" = "character",
                   "RumboF2" = "character",
                   "DistFoto" = "character",
                   "CarFoto1" = "character",
                   "NumFoto1" = "numeric",
                   "ConFoto1" = "numeric",
                   "CarFoto2" = "character",
                   "NumFoto2" = "numeric",
                   "ConFoto2" = "numeric",
                   "Estado" = "character",
                   "Tecnico" = "character")

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
                   "CortaReg" = "character",
                   "DistFoto" = "character",
                   "Pasada1" = "character",
                   "Pasada2" = "character",
                   "Foto1" = "character",
                   "Foto2" = "character",
                   "Nivel1" = "character",
                   "Nivel2" = "character",
                   "Nivel3" = "character",
                   "HoraPh" = "character",
                   "HoraIni" = "character",
                   "HoraFin" = "character",
                   "Rocosid" = "character",
                   "DisEsp" = "character")
  colClasses <- colClasses3
  if(ifn==4) colClasses <- colClasses4

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

    names(pd)[names(pd)=="Cla"] <- "Clase"
    #Check Estadillo
    pd$Estadillo <- .checkEstadillo(pd$Estadillo)
    pd$Clase <- .checkClase(pd$Clase)
    pd$Subclase <- .checkSubclase(pd$Subclase)


    # Plot selection
    if(!is.null(plotType)) {
      sc <- pd$Subclase
      sc[is.na(sc)]<-""
      pdtype <- paste(pd$Clase,sc, sep="")
      sel <- pdtype %in% plotType
      pd <- pd[sel,,drop = FALSE]
    }

    # Add IDs and order variables
    pd$IDPARCELA <- paste0(pd$Provincia, pd$Estadillo)
    pd$IDCLASE <- paste0(pd$Clase, pd$Subclase)
    pd$ID = paste0(pd$IDPARCELA,"_", pd$IDCLASE)
    vars <- c("Provincia", "Estadillo", "Clase", "Subclase", "IDPARCELA", "IDCLASE" ,"ID")
    pd <- pd[,c(vars, names(pd)[!(names(pd) %in% vars)]), drop = FALSE]
    df_list[[i]]<- as_tibble(pd)
  }
  return(bind_rows(df_list))
}

