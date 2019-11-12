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
