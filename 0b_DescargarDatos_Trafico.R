##########################################################################
# Miguel A. Fernandez
# TFM - Air Quality in Madrid

# Con este script se descargan los Datos de Tráfico de Google Drive 
##########################################################################

if(!file.exists("rawdata")){
  dir.create("rawdata")
}

if(!file.exists("rawdata/Trafico")){
  dir.create("rawdata/Trafico")
}

drive_download(as_id("1Z7w6r__kvrGMQc_uSiLTbNyZIrVr2o3h"), 
               path = "./rawdata/Trafico/2014.zip", 
               overwrite = TRUE)

drive_download(as_id("1zr7hBFr5pEKxbf7XOCy50V8RKNIcuI_u"),
               path = "./rawdata/Trafico/2015.zip",
               overwrite = TRUE)

drive_download(as_id("1Z9VWSr-ncH6EiDi267iY_K_Fa3incnc7"),
               path = "./rawdata/Trafico/2016.zip",
               overwrite = TRUE)

drive_download(as_id("1Enf_SVNvNfsdDzheux-zlarE7s2wX4_1"),
               path = "./rawdata/Trafico/2017.zip",
               overwrite = TRUE)

drive_download(as_id("1BXxJwN_155yNN_o7gTJUuKNEZqM94qE3"),
               path = "./rawdata/Trafico/2018.zip",
               overwrite = TRUE)

drive_download(as_id("1Io_5lyppb0D-U2XmhgJhDWEDqCcKRjs3"),
               path = "./rawdata/Trafico/2019.zip",
               overwrite = TRUE)

for (anio in seq(2014, 2019)) {
  unzip(paste0("./rawdata/Trafico/", anio, ".zip"), exdir = "./rawdata/Trafico/")
  file.remove(paste0("./rawdata/Trafico/", anio, ".zip"))  
}

if(file.exists("rawdata/Trafico/__MACOSX/")){
  system(paste0("rm -r ", "rawdata/Trafico/__MACOSX/"))
}

# Hago limpieza de objetos en memoria
rm(list = ls())    