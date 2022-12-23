.getSpainProv<-function() {
  SpainProv = c("01","02","03","04","05","06","07", "08","09","10",
                "11","12","13","14","15","16","17", "18","19","20",
                "21","22","23","24","25","26","27", "28","29","30",
                "31","32","33","34","35","36","37", "38","39","40",
                "41","42","43","44","45","46","47", "48","49","50")
  return(SpainProv)
}
.getProvinceFromID<-function(x) {
  return(substr(x,1, nchar(x)-4))
}
.densityFactor<-function(d) {
  factor<-c(127.3239546, 31.83098865,14.14710607, 5.092958185)
  FACTOREXP = rep(NA, length(d))
  FACTOREXP[which(d<12.5)] = 1
  FACTOREXP[which(d>=12.5 & d<22.5)] = 2
  FACTOREXP[which(d>=22.5 & d<42.5)] = 3
  FACTOREXP[which(d>=42.5)] = 4
  return(factor[FACTOREXP])
}
.checkSpecies <- function(x) {
  x <- as.character(x)
  x[nchar(x)==1 & !is.na(x)] <- paste0("000",x[nchar(x)==1 & !is.na(x)])
  x[nchar(x)==2 & !is.na(x)] <- paste0("00",x[nchar(x)==2 & !is.na(x)])
  x[nchar(x)==3 & !is.na(x)] <- paste0("0",x[nchar(x)==3 & !is.na(x)])
  return(x)
}
.checkEstadillo <- function(x) {
  x <- as.character(x)
  x[nchar(x)==1] <- paste0("0000",x[nchar(x)==1])
  x[nchar(x)==2] <- paste0("000",x[nchar(x)==2])
  x[nchar(x)==3] <- paste0("00",x[nchar(x)==3])
  x[nchar(x)==4] <- paste0("0",x[nchar(x)==4])
  return(x)
}
.readIFN2Tables<-function(prov, DBHdir, tablename = "PIESME") {
  df_list <- vector("list", length(prov))
  for(i in 1:length(prov)){
    td<-read.dbf(paste0(DBFdir,"/",prov[i],"/",tablename,prov[i],".DBF"))
    td$PROVINCIA <- prov[i]
    df_list[[i]] <- as_tibble(td)
  }
  df<-bind_rows(df_list)

  #Check Estadillo
  df$ESTADILLO <- .checkEstadillo(df$ESTADILLO)

  #Define ID
  df$ID<-paste0(df$PROVINCIA,df$ESTADILLO)

  return(df)
}
