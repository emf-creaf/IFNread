#' Reads the table of pies mayores
#'
#'
#' @name readPiesMayores
#'
#' @param prov Character vector with codes of provinces (e.g. \code{c("01","03")}) or \code{"all"} for all provinces.
#' @param DBFdir String with the path where provinces are stored
#' @param plotIDs String vector with a selection of plots
#' @param rem.nodensity Flag to remove records without density
#' @param rem.nospecies Flag to remove records without species identity
#' @param rem.noheight Flag to remove records without tree height
#' @param height.cm Flag to return height in 'cm' instead of 'meters'
#' @param subsetVars Flag to filter data columns
#'
#' @return a data frame
#'
#' @examples
#'
#' \dontrun{
#' accessfilesIFN3 = list.files("//SERVERPROCESS/Miquel/Datasets/IFN/IFN3/MSAccess/",
#'                              pattern = "*[.]accdb", full.names = T)
#'
#' treeData = readPiesMayoresIFN3(accessfilesIFN3[prov])
#'
#' }
readPiesMayoresIFN2<-function(prov, DBFdir = "DBF", plotIDs = NULL,
                              rem.nodensity=FALSE, rem.nospecies=FALSE, rem.noheight=FALSE,
                              height.cm = FALSE, subsetVars=TRUE){
  if(prov=="all") prov = .getSpainProv()
  provNum = as.numeric(prov)
  treeDataIFN2<-read.dbf(paste(DBFdir,"/",prov[1],"/PIESMA",prov[1],".dbf",sep=""))
  cat(paste(prov[1],".",sep=""))
  treeDataIFN2$PROVINCIA = provNum[1]
  if(length(prov)>1){
    for(i in 2:length(prov)){
      cat(paste(prov[i],".",sep=""))
      td<-read.dbf(paste(DBFdir,"/",prov[i],"/PIESMA",prov[i],".dbf",sep=""))
      td$PROVINCIA = provNum[i]
      treeDataIFN2 = merge(treeDataIFN2, td, all=TRUE, sort=FALSE)
    }
  }
  treeDataIFN2$ID<-as.character(treeDataIFN2$PROVINCIA*10000+as.numeric(as.character(treeDataIFN2$ESTADILLO)))
  #Selection
  sel = rep(TRUE, nrow(treeDataIFN2))
  if(!is.null(plotIDs)) sel = sel & (treeDataIFN2$ID %in% as.character(plotIDs))
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

#' @rdname readPiesMayores
#' @param accessFiles A character vector of access files to be read.
#' @param plotTypeIFN3 Subset of plot types to include:
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
#' @param include.IFN2 Flag to include diameter and height measured in IFN2
#' @param rem.codes32 set of tree codes in IFN2 corresponding to records to be removed:
#' \itemize{
#'   \item{'0' or '000' - Tree not tallied in IFN2 because of its small size.}
#'   \item{'999' - Tree that should have been tallied in IFN2 but was not.}
#' }
#' @param rem.codes33 set of tree codes in IFN3 corresponding to records to be removed
#' \itemize{
#'   \item{'000' - Tree tallied in IFN2 but cut between the two inventories.}
#'   \item{'444' - Tree tallied in IFN2 and the stump is not found or it is in advanced decomposition state.}
#'   \item{'777' - Cork oak tree tallied in IFN2 and scorched between the two inventories.}
#'   \item{'888' - Tree tallied in IFN2 and dead between the two inventories.}
#'   \item{'999' - Tree tallied in IFN2 but should have not been included.}
#' }
readPiesMayoresIFN3<-function(accessFiles, plotTypeIFN3=c("A1","NN"), plotIDs = NULL,
                              include.IFN2 = FALSE,
                              rem.nospecies=FALSE, rem.noheight=FALSE, rem.nodensity=FALSE,
                              rem.codes32 = character(0), rem.codes33 = character(0),
                              height.cm = FALSE, subsetVars=TRUE){

  readFromConnection<-function(ch) {
    findRow<-function(x) {
      n = which(treeDataIFN2$Estadillo==x[["Estadillo"]] & treeDataIFN2$NumOrden==x[["OrdenIf2"]])
      if(length(n)!=1) n = NA
      return(n)
    }

    treeDataIFN3<-RODBC::sqlFetch(ch,"PCMayores")
    treeDataIFN3$Provincia = ""
    toFill = (treeDataIFN3$Provincia=="")
    if(sum(toFill)>0) { #Get Provincia from PCparcelas if missing information in PCMayores
      pcDataIFN3<-RODBC::sqlFetch(ch,"PCParcelas")
      estunique = unique(treeDataIFN3$Estadillo[toFill])
      for(est in estunique) {
        treeDataIFN3$Provincia[treeDataIFN3$Estadillo==est] = pcDataIFN3$Provincia[pcDataIFN3$Estadillo==est][1]
      }
    }

    if(include.IFN2) {
      treeDataIFN3$Species2=NA
      treeDataIFN3$Ht2=NA
      treeDataIFN3$DBH2=NA
      treeDataIFN3$N2=NA
      treeDataIFN2 <-RODBC::sqlFetch(ch,"PCMayores2")
      if(nrow(treeDataIFN2)>0) {
        sel = !(treeDataIFN3$OrdenIf2 %in% c("0","888","999"))
        sel[is.na(sel)] = FALSE
        treeDataIFN2 = treeDataIFN2[!(treeDataIFN2[, "NumOrden"] %in% c("0","888","999")),] # Remove non-measured or dead trees (cannot be matched if there is more than one)
        treeDataIFN2$CodeTree = paste(treeDataIFN2[,"Estadillo"],
                                      treeDataIFN2[, "NumOrden"])
        codeTree = paste(treeDataIFN3[sel,"Estadillo"], treeDataIFN3[sel, "OrdenIf2"])

        rs <- rep(NA, length(codeTree))
        pb = txtProgressBar(1, length(codeTree), style=3)
        for(j in 1:length(codeTree)) {
          setTxtProgressBar(pb, j)
          r = which(treeDataIFN2$CodeTree==codeTree[j])
          if(length(r)>0) {
            rs[j] = r[1]
          }
        }
        cat("\n")
        #Do not look for values when the row is unknown
        sel[sel] = !is.na(rs)
        rs = rs[!is.na(rs)]
        treeDataIFN3$Species2[sel] = as.character(treeDataIFN2[rs,"Especie"])
        treeDataIFN3$Ht2[sel]<-treeDataIFN2[rs,"Altura"]
        treeDataIFN3$DBH2[sel]<-0.05*(treeDataIFN2[rs,"Diametro1"]+ treeDataIFN2[rs,"Diametro2"])
        treeDataIFN3$N2[sel]<-.densityFactor(treeDataIFN3$DBH2[sel])
        treeDataIFN3$Forma2[sel]<-treeDataIFN2[rs,"Forma"]
      }
    }
    return(treeDataIFN3)
  }

  cat(paste("Reading",accessFiles[1],"...\n"))
  ch<-RODBC::odbcConnectAccess2007(accessFiles[1])
  treeDataIFN3 = readFromConnection(ch)
  close(ch)

  if(length(accessFiles)>1){
    for(i in 2:length(accessFiles)){
      cat(paste("Reading",accessFiles[i],"...\n"))
      chi<-RODBC::odbcConnectAccess2007(accessFiles[i])
      tdi<-readFromConnection(chi)
      close(chi)
      treeDataIFN3 = merge(treeDataIFN3, tdi, all=TRUE, sort=FALSE)
    }
  }
  sc = as.character(treeDataIFN3$Subclase)
  sc[is.na(sc)]<-""
  #remove extra spaces
  nc = nchar(sc)
  sel = (substr(sc,nc, nc) == " ")
  sc[sel] = substr(sc[sel], 1, nc-1)

  pdtype = paste(treeDataIFN3$Cla,sc, sep="")
  treeDataIFN3$TYPE  = pdtype
  #ID
  treeDataIFN3$ID<-as.character(as.numeric(as.character(treeDataIFN3$Provincia))*10000+as.numeric(as.character(treeDataIFN3$Estadillo)))

  #Selection
  sel = rep(TRUE, nrow(treeDataIFN3))
  if(!is.null(plotTypeIFN3)) sel = sel & (pdtype %in% plotTypeIFN3)
  if(!is.null(plotIDs)) sel = sel & (treeDataIFN3$ID %in% as.character(plotIDs))
  treeDataIFN3 <-treeDataIFN3[sel,]


  if(nrow(treeDataIFN3)>0) {
    if(rem.nospecies) treeDataIFN3<-treeDataIFN3[!is.na(treeDataIFN3$Especie),]
    treeDataIFN3$Estadillo<-as.character(treeDataIFN3$Estadillo)
    treeDataIFN3$Especie<-as.character(treeDataIFN3$Especie)
    treeDataIFN3$Dn1 = as.character(treeDataIFN3$Dn1)
    treeDataIFN3$Dn1[treeDataIFN3$Dn1=="NA"]<-NA
    treeDataIFN3$Dn1 <- as.numeric(treeDataIFN3$Dn1)
    treeDataIFN3$Dn2 = as.character(treeDataIFN3$Dn2)
    treeDataIFN3$Dn2[treeDataIFN3$Dn2=="NA"]<-NA
    treeDataIFN3$Dn2 <- as.numeric(treeDataIFN3$Dn2)
    treeDataIFN3$DM<-(treeDataIFN3$Dn1+treeDataIFN3$Dn2)/(2*10) #To cm
    treeDataIFN3$N<-.densityFactor(treeDataIFN3$DM)
    treeDataIFN3$Ht = sub(",",".",as.character(treeDataIFN3$Ht))
    treeDataIFN3$Ht[treeDataIFN3$Ht=="NA"]<-NA
    treeDataIFN3$Ht <- as.numeric(treeDataIFN3$Ht)
    ### SPECIFY SELECTION CRITERIA
    ### remove tree when we lack N
    if(rem.nodensity) treeDataIFN3<-treeDataIFN3[!is.na(treeDataIFN3$N),]
    ### remove tree when we lack N
    if(rem.noheight) treeDataIFN3<-treeDataIFN3[!is.na(treeDataIFN3$Ht),]
    ### remove tree codes
    if(length(rem.codes32)>0){
      treeDataIFN3<-treeDataIFN3[!(treeDataIFN3$OrdenIf2 %in% rem.codes32),]
    }
    if(length(rem.codes33)>0){
      treeDataIFN3<-treeDataIFN3[!(treeDataIFN3$OrdenIf3 %in% rem.codes33),]
    }
    #Translate meters to cms
    if(height.cm) treeDataIFN3$Ht = treeDataIFN3$Ht*100 #meters to cms

    #Turn tree codes into numeric
    treeDataIFN3$OrdenIf2 = as.character(as.numeric(treeDataIFN3$OrdenIf2))
    treeDataIFN3$OrdenIf3 = as.character(as.numeric(treeDataIFN3$OrdenIf3))
  } else {
    treeDataIFN3$N<-numeric(0)
    treeDataIFN3$DM<-numeric(0)
  }

  if(subsetVars){
    vars = c("Provincia","Estadillo","ID","TYPE","Especie","N","DM","Ht","OrdenIf3","OrdenIf2", "Forma")
    if(include.IFN2) vars = c(vars, "Species2", "N2", "DBH2", "Ht2", "Forma2")
    treeDataIFN3 <- treeDataIFN3[,vars]
    nms <-c("Provincia","Estadillo","ID","Type","Species", "N", "DBH","H", "OIF3", "OIF2", "FC")
    if(include.IFN2) nms <- c(nms,"Species2", "N2","DBH2","H2", "FC2")
    names(treeDataIFN3)<-nms
  }
  return(treeDataIFN3)
}



#' @rdname readPiesMayores
#'
#' @param plotTypeIFN4 Subset of plot types to include:
#' @param include.IFN3 Flag to include diameter and height measured in IFN3
#' @param rem.codes43 set of tree codes in IFN3 corresponding to records to be removed:
#' \itemize{
#'   \item{'0' or '000' - Tree not tallied in IFN3 because of its small size.}
#'   \item{'999' - Tree that should have been tallied in IFN3 but was not for some reason.}
#' }
#' @param rem.codes44 set of tree codes in IFN4 corresponding to records to be removed
#' \itemize{
#'   \item{'000' - Tree tallied in IFN3 but cut between the two inventories (stump is found).}
#'   \item{'444' - Tree tallied in IFN3 and the stump is not found or it is in advanced decomposition state.}
#'   \item{'777' - Cork oak tree tallied in IFN3 and scorched between the two inventories.}
#'   \item{'888' - Tree tallied in IFN3 and dead between the two inventories.}
#'   \item{'999' - Tree tallied in IFN3 but should have not been included.}
#' }
#' @param vars.IFN3 String vector with column names of variables in the IFN3 table
#'
readPiesMayoresIFN4<-function(accessFiles, plotTypeIFN4=c("A1","NN"),
                              plotIDs = NULL, include.IFN3 = FALSE,
                              rem.nospecies=FALSE, rem.noheight=FALSE, rem.nodensity=FALSE,
                              rem.codes43 = character(0), rem.codes44 = character(0),
                              height.cm = FALSE, subsetVars=TRUE, vars.IFN3=c("OrdenIf3", "Especie","Dn1","Dn2","Ht", "Forma")){


  readFromConnection<-function(ch) {

    getPCParcelas<-function(ch) {
      pcDataIFN4<-RODBC::sqlFetch(ch,"PCParcelas")
      nc = nchar(as.character(pcDataIFN4$Estadillo))
      ncI = nc-3
      if(!("Provincia" %in% names(pcDataIFN4))) pcDataIFN4$Provincia = as.numeric(substr(as.character(pcDataIFN4$Estadillo),1,nc-4))
      pcDataIFN4$Estadillo = as.numeric(substr(as.character(pcDataIFN4$Estadillo),ncI,nc))
      return(pcDataIFN4)
    }

    treeDataIFN4<-RODBC::sqlFetch(ch,"PCMayores")

    nc = nchar(as.character(treeDataIFN4$Estadillo))
    ncI = nc-3
    if(!("Provincia" %in% names(treeDataIFN4))) treeDataIFN4$Provincia = as.numeric(substr(as.character(treeDataIFN4$Estadillo),1,nc-4))
    treeDataIFN4$Estadillo = as.numeric(substr(as.character(treeDataIFN4$Estadillo),ncI,nc))
    treeDataIFN4$Provincia[is.na(treeDataIFN4$Provincia)] = ""
    toFill = treeDataIFN4$Provincia==""
    if(sum(toFill)>0) { #Get Provincia from PCparcelas if missing information in PCMayores
      pcDataIFN4 = getPCParcelas(ch)
      estunique = unique(treeDataIFN4$Estadillo[toFill])
      for(est in estunique) {
        treeDataIFN4$Provincia[treeDataIFN4$Estadillo==est] = pcDataIFN4$Provincia[pcDataIFN4$Estadillo==est][1]
      }
    }
    if(include.IFN3) {
      #Define new fields
      treeDataIFN4$Species3=NA
      treeDataIFN4$N3=NA
      treeDataIFN4$Ht3=NA
      treeDataIFN4$DBH3=NA
      #Retrieve IFN3 table
      treeDataIFN3 <-RODBC::sqlFetch(ch,"PCMayores3")

      if(nrow(treeDataIFN3)>0) {
        #Fill province field
        nc = nchar(as.character(treeDataIFN3$Estadillo))
        ncI = nc-3
        if(!("Provincia" %in% names(treeDataIFN3))) treeDataIFN3$Provincia = as.numeric(substr(as.character(treeDataIFN3$Estadillo),1,nc-4))
        treeDataIFN3$Estadillo = as.numeric(substr(as.character(treeDataIFN3$Estadillo),ncI,nc))
        treeDataIFN3$Provincia[is.na(treeDataIFN3$Provincia)] = ""
        toFill = (treeDataIFN3$Provincia=="")
        #Get Provincia from PCparcelas if missing information in PCMayores3
        if(sum(toFill)>0) {
          pcDataIFN4 = getPCParcelas(ch)
          estunique = unique(treeDataIFN3$Estadillo[toFill])
          for(est in estunique) {
            treeDataIFN3$Provincia[treeDataIFN3$Estadillo==est] = pcDataIFN4$Provincia[pcDataIFN4$Estadillo==est][1]
          }
        }
        sel = !(treeDataIFN4$OrdenIf3 %in% c("0","888","999"))
        sel[is.na(sel)] = FALSE
        treeDataIFN3 = treeDataIFN3[!(treeDataIFN3[,vars.IFN3[1]] %in% c("0","888","999")),] # Remove non-measured or dead trees (cannot be matched if there is more than one)
        treeDataIFN3$CodeTree = paste(treeDataIFN3[, "Provincia"],treeDataIFN3[,"Estadillo"],
                                      treeDataIFN3[,vars.IFN3[1]])
        codeTree = paste(treeDataIFN4[sel, "Provincia"],treeDataIFN4[sel,"Estadillo"],
                         treeDataIFN4[sel, "OrdenIf3"])

        rs <- rep(NA, length(codeTree))
        pb = txtProgressBar(1, length(codeTree), style=3)
        for(j in 1:length(codeTree)) {
          setTxtProgressBar(pb, j)
          r = which(treeDataIFN3$CodeTree==codeTree[j])
          if(length(r)>0) {
            rs[j] = r[1]
          }
        }
        cat("\n")
        #Do not look for values when the row is unknown
        sel[sel] = !is.na(rs)
        rs = rs[!is.na(rs)]
        treeDataIFN4$Species3[sel] = as.character(treeDataIFN3[rs,vars.IFN3[2]])
        treeDataIFN4$DBH3[sel]<-0.05*(treeDataIFN3[rs,vars.IFN3[3]]+ treeDataIFN3[rs,vars.IFN3[4]])
        treeDataIFN4$Ht3[sel]<-treeDataIFN3[rs,vars.IFN3[5]]
        treeDataIFN4$N3[sel]<-.densityFactor(treeDataIFN4$DBH3[sel])
        treeDataIFN4$Forma3[sel] <- treeDataIFN3[rs,vars.IFN3[6]]
      }
    }
    return(treeDataIFN4)
  }
  cat(paste("Reading",accessFiles[1],"...\n"))
  ch<-RODBC::odbcConnectAccess2007(accessFiles[1])
  treeDataIFN4 <-readFromConnection(ch)
  close(ch)

  if(length(accessFiles)>1){
    for(i in 2:length(accessFiles)){
      cat(paste("Reading",accessFiles[i],"...\n"))
      ch<-RODBC::odbcConnectAccess2007(accessFiles[i])
      treeDataIFN4 = merge(treeDataIFN4, readFromConnection(ch), all=TRUE, sort=FALSE)
      close(ch)
    }
  }
  sc = as.character(treeDataIFN4$Subclase)
  sc[is.na(sc)]<-""
  #remove extra spaces
  nc = nchar(sc)
  sel = (substr(sc,nc, nc) == " ")
  sc[sel] = substr(sc[sel], 1, nc-1)
  pdtype = paste(treeDataIFN4$Cla,sc, sep="")
  treeDataIFN4$TYPE  = pdtype


  #Build ID (provincia + estadillo)
  treeDataIFN4$ID<-as.character(as.numeric(as.character(treeDataIFN4$Provincia))*10000+as.numeric(as.character(treeDataIFN4$Estadillo)))


  #Selection
  sel = rep(TRUE, nrow(treeDataIFN4))
  if(!is.null(plotTypeIFN4)) sel = sel & (pdtype %in% plotTypeIFN4)
  if(!is.null(plotIDs)) sel = sel & (treeDataIFN4$ID %in% as.character(plotIDs))
  treeDataIFN4 <-treeDataIFN4[sel,]

  if(rem.nospecies) treeDataIFN4<-treeDataIFN4[!is.na(treeDataIFN4$Especie),]
  treeDataIFN4$Especie<-as.character(treeDataIFN4$Especie)
  treeDataIFN4$Dn1 = as.character(treeDataIFN4$Dn1)
  treeDataIFN4$Dn1[treeDataIFN4$Dn1=="NA"]<-NA
  treeDataIFN4$Dn1 <- as.numeric(treeDataIFN4$Dn1)
  treeDataIFN4$Dn2 = as.character(treeDataIFN4$Dn2)
  treeDataIFN4$Dn2[treeDataIFN4$Dn2=="NA"]<-NA
  treeDataIFN4$Dn2 <- as.numeric(treeDataIFN4$Dn2)
  treeDataIFN4$Dm<-(treeDataIFN4$Dn1+treeDataIFN4$Dn2)/(2*10)
  treeDataIFN4$N<-.densityFactor(treeDataIFN4$Dm)
  treeDataIFN4$Ht = sub(",",".",as.character(treeDataIFN4$Ht))
  treeDataIFN4$Ht[treeDataIFN4$Ht=="NA"]<-NA
  treeDataIFN4$Ht <- as.numeric(treeDataIFN4$Ht)
  # SPECIFY SELECTION CRITERIA
  # remove tree when we lack N
  if(rem.nodensity) treeDataIFN4<-treeDataIFN4[!is.na(treeDataIFN4$N),]
  # remove tree when we lack N
  if(rem.noheight) treeDataIFN4<-treeDataIFN4[!is.na(treeDataIFN4$Ht),]
  ### remove tree codes
  if(length(rem.codes43)>0){
    treeDataIFN4<-treeDataIFN4[!(treeDataIFN4$OrdenIf3 %in% rem.codes43),]
  }
  if(length(rem.codes44)>0){
    treeDataIFN4<-treeDataIFN4[!(treeDataIFN4$OrdenIf4 %in% rem.codes44),]
  }
  #Translate meters to cms
  if(height.cm) treeDataIFN4$Ht = treeDataIFN4$Ht*100 #meters to cms
  if(subsetVars){
    vars = c("Provincia","Estadillo","ID","TYPE","Especie","N","Dm","Ht","Forma","OrdenIf4","OrdenIf3")
    if(include.IFN3) vars = c(vars, "Species3","N3","DBH3", "Ht3", "Forma3")
    treeDataIFN4 <- treeDataIFN4[,vars]
    nms <-c("Provincia","Estadillo","ID","Type","Species", "N", "DBH","H", "FC", "OIF4", "OIF3")
    if(include.IFN3) nms <- c(nms, "Species3", "N3", "DBH3","H3", "FC3")
    names(treeDataIFN4)<-nms
  }

  return(treeDataIFN4)
}
