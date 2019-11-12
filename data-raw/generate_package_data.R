library(readxl)

df=as.data.frame(read_xls("D:/KnowledgeTransfer/2018_2019_Modelitzacio_INIA_CTFC/ProvinciasCCAA.xls"))
provincias = df[,-1]
row.names(provincias) = df$CODIGO
# provincias = provincias[order(as.numeric(row.names(provincias))),]
usethis::use_data(provincias, overwrite = TRUE)
