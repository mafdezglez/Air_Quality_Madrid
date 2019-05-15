##########################################################################
# Miguel A. Fernandez
# TFM - Air Quality in Madrid

# En este script vamos a preparar los datos de la Intensidad del Tráfico
##########################################################################

# Función que aplicaremos para convertir los valores negativos en el dataframe de Trafico a valores NA,
# ya que expresa mejor el hecho de que no se hayan recogido valores, y así además podremos calcular mejor las
# medias.
haz.na <- function(x) {
  ifelse(x<0, NA, x)
}

estaciones_Trafico <- read_excel("rawdata/pmed_ubicacion_trafico.xlsx", 
                                 col_types = c("text", "numeric", "numeric", 
                                               "text", "text", "text", "text", "numeric", 
                                               "numeric"))

estaciones_Trafico <- select(estaciones_Trafico, 
                             -c(distrito, tipo_elem, utm_x, utm_y))

estaciones_CalidadAire <- read_excel("rawdata/pmed_ubicacion_calidad_aire.xlsx", 
                                     col_types = c("numeric", "text", "text", 
                                                   "numeric", "numeric"))

# Añado al dataframe de las estaciones de Tráfico 3 columnas donde calcular la distancia 
# de cada una de las estaciones de tráfico con las 3 estaciones de calidad del aire con las 
# que realizaremos las predicciones inicialmente
estaciones_Trafico <- mutate(estaciones_Trafico, dist_est8=0, dist_est18=0, dist_est24=0)

# Con este bucle calculo las distancias (en metros) entre estaciones utilizando 
# la función distVincentyEllipsoid
for (filaT in 1:nrow(estaciones_Trafico)) {
  loc_Trafico <- c(estaciones_Trafico$longitud[filaT], estaciones_Trafico$latitud[filaT])
  
  for (filaC in 1:nrow(estaciones_CalidadAire)) {
    loc_CalidadAire <- c(estaciones_CalidadAire$LONG[filaC], estaciones_CalidadAire$LAT[filaC])
    distanciaV <- distVincentyEllipsoid(loc_Trafico, loc_CalidadAire)
    estacion <- paste0("dist_est", estaciones_CalidadAire$NUMERO[filaC] )
    estaciones_Trafico[filaT, estacion] <- distanciaV
  } 
}

# Selecciono las 4 estaciones de Tráfico más próximas a las estaciones que miden la Calidad del Aire
proximas_est8  <- estaciones_Trafico %>% arrange(dist_est8) %>% select(id) %>% 
                  head(4) %>% mutate(estacion_CalidadAire=8)
proximas_est18 <- estaciones_Trafico %>% arrange(dist_est18) %>% select(id) %>% 
                  head(4) %>% mutate(estacion_CalidadAire=18)
proximas_est24 <- estaciones_Trafico %>% arrange(dist_est24) %>% select(id) %>% 
                  head(4) %>% mutate(estacion_CalidadAire=24)
estacionesT <- rbind(proximas_est8, proximas_est18, proximas_est24)


# Leemos los datos de la intensidad del Tráfico para las estaciones anteriores
df_Trafico <- data.frame()

# Obtener lista de archivos dentro de la carpeta
ruta <- "./rawdata/Trafico/"
lista_carpetas <- list.files(path=ruta)

# Recorro todos los ficheros en las distintas carpetas para añadirlos al dataframe df_Trafico
for (j in 1:length(lista_carpetas)) {
  subruta <- paste0(ruta, lista_carpetas[j])
  
  files <- list.files(subruta)

  for (file in files) {
    fileRuta <- paste0(subruta, "/", file)
    print(fileRuta)
    
    if (file == "03-2017.zip") {
      tmp <- read_csv(fileRuta, col_types = cols(fecha = col_character()))
    }
    else {
      tmp <- read_delim(fileRuta, ";", escape_double = FALSE, 
                        col_types = cols(fecha = col_character()), 
                        trim_ws = TRUE)
    }
    
    
    if (lista_carpetas[j]=='2014' & file <= "10-2014.zip") {
      tmp <- setnames(tmp, old="identif", new="id")
      tmp <- select(tmp, -c("tipo", "periodo_integracion"))
    }
    else if ((lista_carpetas[j]=='2015' & file >= "01-2015.zip" & file <= "03-2015.zip") |
             (lista_carpetas[j]=='2015' & file >= "05-2015.zip" & file <= "08-2015.zip") |
              file == "12-2014.zip") { 
      tmp <- setnames(tmp, old="idelem", new="id")
      tmp <- select(tmp, -c("identif", "tipo_elem", "tipo", "periodo_integracion"))
    }
    else if ((lista_carpetas[j]=='2015' & file >= "09-2015.zip") | lista_carpetas[j]=='2016' | 
             (lista_carpetas[j]=='2017' & file >= "01-2017.zip" & file <= "09-2017.zip") |
              file == "11-2014.zip" | file == "04-2015.zip") {
      tmp <- setnames(tmp, old="idelem", new="id")
      tmp <- select(tmp, -c("identif", "tipo_elem", "periodo_integracion"))
    }
    else if ((lista_carpetas[j]=='2017' & file >= "10-2017.zip") | lista_carpetas[j]=='2018') {
      tmp <- select(tmp, -c("tipo_elem", "periodo_integracion"))
    }
    else {
      print("Fichero con formato distinto")
    }
    
    tmp$id <- as.numeric(tmp$id)
    tmp <- tmp %>% inner_join(estacionesT) 
    
    if (nrow(tmp)>0) {
      tmp <- tmp %>% separate(fecha, c("fecha", "hora"), sep=" ")
      tmp$hora <- paste0(substr(tmp$hora, 1, 2), ":00:00")
      tmp <- tmp %>% unite("fecha", fecha, hora, sep=" ")
      tmp$fecha <- ymd_hms(tmp$fecha)
      tmp$intensidad <- sapply(tmp$intensidad, haz.na)
      tmp$ocupacion <- sapply(tmp$ocupacion, haz.na)
      tmp$carga <- sapply(tmp$carga, haz.na)
      tmp$vmed <- sapply(tmp$vmed, haz.na)
      tmp <- tmp %>% filter(error=="N")
      tmp <- tmp %>% group_by(id, fecha, estacion_CalidadAire) %>%
                     summarise(intensidad = mean(intensidad, na.rm = TRUE),
                               ocupacion = mean(ocupacion, na.rm = TRUE),
                               carga = mean(carga, na.rm = TRUE),
                               vmed = mean(vmed, na.rm = TRUE))
      
      df_Trafico <- rbind(df_Trafico, data.frame(tmp))
    }
  }
}

# Recorremos el dataframe corrigiendo los valores NA,
# para cada hora buscando el valor más próximo anterior válido en cada estación
df_Trafico <- df_Trafico %>% arrange(id, fecha)
nhoras <- 24 * 7
fila <- 1

while (fila <= nrow(df_Trafico)) {
  if (is.na(df_Trafico$intensidad[fila])) {
    if (df_Trafico$id[fila]==df_Trafico$id[fila-nhoras]) {
      df_Trafico$intensidad[fila] <- df_Trafico$intensidad[fila-nhoras]  
    }
  }
  if (is.na(df_Trafico$ocupacion[fila])) {
    if (df_Trafico$id[fila]==df_Trafico$id[fila-nhoras]) {
      df_Trafico$ocupacion[fila] <- df_Trafico$ocupacion[fila-nhoras]  
    }
  }
  if (is.na(df_Trafico$carga[fila])) {
    if (df_Trafico$id[fila]==df_Trafico$id[fila-nhoras]) {
      df_Trafico$carga[fila] <- df_Trafico$carga[fila-nhoras]  
    }
  }
  if (is.na(df_Trafico$vmed[fila])) {
    if (df_Trafico$id[fila]==df_Trafico$id[fila-nhoras]) {
      df_Trafico$vmed[fila] <- df_Trafico$vmed[fila-nhoras]  
    }
  }
  
  fila <- fila+1
}

# Calculamos la media para cada medición de las 4 estaciones más próximas a cada estación
# de Calidad del Aire
df_Trafico <- df_Trafico %>% 
              group_by(estacion_CalidadAire, fecha) %>% 
              summarise(intensidad = mean(intensidad), 
                        ocupacion = mean(ocupacion),
                        carga = mean(carga),
                        vmed = mean(vmed))

# Guardamos fichero maestro del Tráfico
if(!file.exists("data")){
  dir.create("data")
}

write.csv(df_Trafico, "data/Trafico.csv")

# Hago limpieza de objetos en memoria
rm(list = ls())    

