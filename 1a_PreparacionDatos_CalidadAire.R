##########################################################################
# Miguel A. Fernandez
# TFM - Air Quality in Madrid

# En este script se realiza la preparación de los datos de los ficheros 
# en "rawdata/CalidadAire/" dejando un único fichero maestro en "data/"
##########################################################################

# Función que lee los ficheros .csv 
leer_csv <- function(ruta, lista) {
  tmp <- read_delim(paste0(ruta, lista),  
                    ";", escape_double = FALSE, trim_ws = TRUE, 
                    col_types = cols(ANO = col_character(), 
                                     ESTACION = col_integer(), H01 = col_double(), 
                                     H02 = col_double(), H03 = col_double(), 
                                     H04 = col_double(), H05 = col_double(), 
                                     H06 = col_double(), H07 = col_double(), 
                                     H08 = col_double(), H09 = col_double(), 
                                     H10 = col_double(), H11 = col_double(), 
                                     H12 = col_double(), H13 = col_double(), 
                                     H14 = col_double(), H15 = col_double(), 
                                     H16 = col_double(), H17 = col_double(), 
                                     H18 = col_double(), H19 = col_double(), 
                                     H20 = col_double(), H21 = col_double(), 
                                     H22 = col_double(), H23 = col_double(), 
                                     H24 = col_double(), MAGNITUD = col_integer(), 
                                     MES = col_character(), PROVINCIA = col_character()))
  
  return(tmp)
}

tratar_csvs <- function(df) {
  tmp <- select(df, -PUNTO_MUESTREO)

  return(tmp)
}

# Función que lee los ficheros .txt 
leer_txt <- function(ruta, lista) {
  tmp <- read_fwf(paste0(ruta, lista),
                  fwf_cols(PROVINCIA=2,
                           MUNICIPIO=3,
                           ESTACION=3,
                           MAGNITUD=2,
                           TECNICA=2,
                           DATOHORARIO=2,
                           ANO=2,
                           MES=2,
                           DIA=2,
                           H01=5, V01=1, H02=5, V02=1, H03=5, V03=1, H04=5, V04=1,
                           H05=5, V05=1, H06=5, V06=1, H07=5, V07=1, H08=5, V08=1,
                           H09=5, V09=1, H10=5, V10=1, H11=5, V11=1, H12=5, V12=1,
                           H13=5, V13=1, H14=5, V14=1, H15=5, V15=1, H16=5, V16=1,
                           H17=5, V17=1, H18=5, V18=1, H19=5, V19=1, H20=5, V20=1,
                           H21=5, V21=1, H22=5, V22=1, H23=5, V23=1, H24=5, V24=1),
                  col_types = cols(ANO = col_character(),
                                   ESTACION = col_integer(), H01 = col_double(),
                                   H02 = col_double(), H03 = col_double(),
                                   H04 = col_double(), H05 = col_double(),
                                   H06 = col_double(), H07 = col_double(),
                                   H08 = col_double(), H09 = col_double(),
                                   H10 = col_double(), H11 = col_double(),
                                   H12 = col_double(), H13 = col_double(),
                                   H14 = col_double(), H15 = col_double(),
                                   H16 = col_double(), H17 = col_double(),
                                   H18 = col_double(), H19 = col_double(),
                                   H20 = col_double(), H21 = col_double(),
                                   H22 = col_double(), H23 = col_double(),
                                   H24 = col_double(), MAGNITUD = col_integer(),
                                   MES = col_character(), PROVINCIA = col_character()))
  
  return(tmp)
}

tratar_txts <- function(df) {
  df$ANO <- paste0("20", df$ANO)
  tmp <- select(df, -DATOHORARIO, -TECNICA)
  
  return(tmp)
}

###################################################################################################

df_CalidadAire <- data.frame()
# Obtener lista de archivos dentro de la carpeta
ruta <- "./rawdata/CalidadAire/"
lista_carpetas <- list.files(path=ruta)
tmp_csv <- data.frame()
tmp_txt <- data.frame()

# Recorro todos los ficheros en las distintas carpetas para añadirlos al dataframe df_CalidadAire
for (j in 1:length(lista_carpetas)) {
  subruta <- paste0(ruta, lista_carpetas[j], "/")
  
  lista_archivos <- list.files(path = subruta)
  
  for (i in 1:length(lista_archivos)) {
    extension <- substr(lista_archivos[i], nchar(lista_archivos[i])-2, nchar(lista_archivos[i]))
    
    if (extension == 'csv') {
      tmp <- leer_csv(subruta, lista_archivos[i])
      tmp_csv <- rbind(tmp_csv, tmp)
    }
    else {
      tmp <- leer_txt(subruta, lista_archivos[i])
      tmp_txt <- rbind(tmp_txt, tmp)
    }
  }
} 


tmp_csv <- tratar_csvs(tmp_csv)
tmp_txt <- tratar_txts(tmp_txt)
df_CalidadAire <- rbind(tmp_csv, tmp_txt)

# Elimino las columnas PROVINCIA y MUNICIPIO por no ser relevantes, ya que tienen un único valor
# para todo el dataframe. 
df_CalidadAire <- select(df_CalidadAire, -c("PROVINCIA", "MUNICIPIO"))

# Construyo la columna fecha en formato AAAA-MM-DD
df_CalidadAire <- unite(data=df_CalidadAire, col= FECHA, sep='-', ANO, MES, DIA)

# Seleccciona las magnitudes sobre las que vamos a realizar el estudio,
# y ordena la salida para poder realizar el recorrido del dataframe correcto a la hora
# de corregir valores inválidos

df_CalidadAire <- df_CalidadAire %>%
                  filter(MAGNITUD %in% c(1, 6, 8, 10, 14)) %>%
                  arrange(ESTACION, MAGNITUD, FECHA)

# Estas funciones buscan el valor anterior y posterior para cada valor inválido, 
# cogiendo el anterior o posterior más próximo en la misma fila,
# y sino lo encuentra en la fila anterior o posterior
buscar_valor_anterior <- function(df, fila, columna) {
  encontrado <- 0
  
  while (encontrado == 0 & columna>=7) {
    if (df[fila, columna-2]=='V') {
      encontrado <- 1
    }
    columna <- columna - 2
  }
  
  if (encontrado == 0 & columna == 5) {
    fila <- fila - 1
    columna <- ncol(df)
    
    posics <- buscar_valor_anterior(df, fila, columna)
  }
  
  return(list(pos_fila=fila, pos_columna=columna))
}

buscar_valor_posterior <- function(df, fila, columna) {
  encontrado <- 0
  
  while (encontrado == 0 & columna<=ncol(df)-2) {
    if (df[fila, columna+2]=='V') {
      encontrado <- 1
    }
    columna <- columna + 2
  }
  
  if (encontrado == 0 & columna == ncol(df)) {
    fila <- fila + 1
    columna <- 5
    
    posics <- buscar_valor_posterior(df, fila, columna)
  }
  
  return(list(pos_fila=fila, pos_columna=columna))
}

# Recorremos el dataframe corrigiendo los valores inválidos para cada hora 
# buscando el valor más próximo anterior válido.
fila <- 1

while (fila <= nrow(df_CalidadAire)) {
  columna <- 1
  for (columna in seq(from = 5, to = ncol(df_CalidadAire), by = 2)) {  
    if (df_CalidadAire[fila, columna]!='V') {
      posics <- buscar_valor_anterior(df_CalidadAire, fila, columna)
      if (df_CalidadAire$ESTACION[fila]==df_CalidadAire$ESTACION[posics$pos_fila] &
          df_CalidadAire$MAGNITUD[fila]==df_CalidadAire$MAGNITUD[posics$pos_fila]) {
        df_CalidadAire[fila, columna-1] <- df_CalidadAire[posics$pos_fila, posics$pos_columna-1]
        df_CalidadAire[fila, columna] <- 'V'  
      }
      else {
        posics <- buscar_valor_posterior(df_CalidadAire, fila, columna)
        if (df_CalidadAire$ESTACION[fila]==df_CalidadAire$ESTACION[posics$pos_fila] &
            df_CalidadAire$MAGNITUD[fila]==df_CalidadAire$MAGNITUD[posics$pos_fila]) {
          df_CalidadAire[fila, columna-1] <- df_CalidadAire[posics$pos_fila, posics$pos_columna-1]
          df_CalidadAire[fila, columna] <- 'V' 
        }
        else {
          print("Registro invalido")
        }
      }
    }
  }
  fila <- fila+1
}

#Contamos los valores inválidos
# cont_V <- 0
# cont_N <- 0
# for (fila in seq(1, nrow(df_CalidadAire))) {
#   for (columna in seq(5, 51, by=2)) {
#     if (df_CalidadAire[fila, columna]=="V") {
#       cont_V <- cont_V + 1
#     }
#     else {
#       cont_N <- cont_N +1
#     }
#   }
# }
# print(cont_N)
# 
# # Sacar las filas con valores inválidos
# invalidos <- data.frame()
# for (fila in seq(1, nrow(df_CalidadAire))) {
#   for (columna in seq(5, 51, by=2)) {
#     if (df_CalidadAire[fila, columna]=="N") {
#       invalidos <- rbind(invalidos, df_CalidadAire[fila,])
#     } 
#   }
# }
# 

df_CalidadAire <- select(df_CalidadAire,
                         -c(V01, V02, V03, V04, V05, V06, V07, V08, V09, V10, V11, V12,
                            V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24))

# Pivotamos la tabla tanto con gather como con spread para tener una observación por hora y
# una columna por cada magnitud.
df_CalidadAire <- gather(df_CalidadAire, "HORA", "CANTIDAD", seq(4, 27))
df_CalidadAire <- spread(df_CalidadAire, key = MAGNITUD, value = CANTIDAD)
setnames(df_CalidadAire, old="1", new="SO2")
setnames(df_CalidadAire, old="6", new="CO")
setnames(df_CalidadAire, old="8", new="NO2")
setnames(df_CalidadAire, old="10", new="PM10")
setnames(df_CalidadAire, old="14", new="O3")

# Construyo la columna fecha uniendo las columnas fecha y hora en formato 'AAAA-MM-DD HH:MM:SS'
df_CalidadAire$FECHA <- paste(df_CalidadAire$FECHA, paste0(str_sub(df_CalidadAire$HORA, 2),":00:00"))

# El intervalo de horas es de 01 a 24, voy a hacer que vayan de 00 a 23.
# De esta forma los datos registrados a las 00:00:00 horas corresponderan al intervalo 
# entre las 00:00:00 y 00:59:59 horas.
df_CalidadAire$FECHA <- ymd_hms(df_CalidadAire$FECHA) - 3600

# Después del gather para tener una columna por magnitud, aparecen nulos para NO2 y PM10.
# Realizamos el siguiente proceso para rellenar estos na's con el valor de la hora anterior más próxima.

#df_CalidadAire <- data.frame(na.omit(df_CalidadAire))
df_CalidadAire <- select(df_CalidadAire, -HORA) %>% arrange(ESTACION, FECHA)
# nhoras <- 1
# fila <- 1
# 
# while (fila <= nrow(df_CalidadAire)) {
#   print(fila-nhoras)
#   if (is.na(df_CalidadAire$NO2[fila])) {
#     if (df_CalidadAire$ESTACION[fila]==df_CalidadAire$ESTACION[fila-nhoras]) {
#       df_CalidadAire$NO2[fila] <- df_CalidadAire$NO2[fila-nhoras]  
#     }
#   }
#   if (is.na(df_CalidadAire$PM10[fila])) {
#     if (df_CalidadAire$ESTACION[fila]==df_CalidadAire$ESTACION[fila-nhoras]) {
#       df_CalidadAire$PM10[fila] <- df_CalidadAire$PM10[fila-nhoras]  
#     }
#   }
#   
#   fila <- fila+1
# }
# 
# 
df_CalidadAire <- df_CalidadAire %>% arrange(FECHA, ESTACION) 

# Guardamos fichero maestro de la Calidad del Aire
if(!file.exists("data")){
  dir.create("data")
}

write.csv(df_CalidadAire, "data/CalidadAire.csv")

# Hago limpieza de objetos en memoria
rm(list = ls())    

