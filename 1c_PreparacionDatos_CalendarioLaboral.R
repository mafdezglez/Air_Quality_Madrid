##########################################################################
# Miguel A. Fernandez
# TFM - Air Quality in Madrid

# En este script se realiza la preparacion de los datos del 
# fichero en "rawdata/CalendarioLaboral/calendario.xls" dejando la 
# version maestra en "data/"
##########################################################################

# Leo el excel del fichero en bruto del calendario laboral
calendario <- readxl::read_xls("rawdata/CalendarioLaboral/calendario.xls", 
                                col_types = c("date", "text", "text", "text", "text"))

# Cambio nombres de columnas a algo más legible y elimino espacios entre medias
setnames(calendario, old="laborable / festivo / domingo festivo", new="Tipo_dia")
setnames(calendario, old="Tipo de Festivo", new="Tipo_de_Festivo")

# COLUMNA Dia_Semana
calendario$Dia_semana <- str_trim(calendario$Dia_semana, side="both")
# Para los valores de la columna 'Dia_semana' elimino acentos
calendario$Dia_semana <- chartr('áéíóú','aeiou', calendario$Dia_semana)

# COLUMNA Tipo_dia
calendario$Tipo_dia <- str_trim(calendario$Tipo_dia, side="both")
# Los días laborables estaban como nulos en la columna 'Tipo día', elimino los nulos con 'laborable'
calendario$Tipo_dia[(calendario$Dia > '2019-01-01') & (is.na(calendario$Tipo_dia))] <- 'laborable'

# COLUMNA Tipo_de_Festivo
calendario$Tipo_de_Festivo <- str_trim(calendario$Tipo_de_Festivo, side="both")
calendario$Tipo_de_Festivo <- str_to_title(calendario$Tipo_de_Festivo, locale = "es")

# COLUMNA Festividad
calendario$Festividad <- str_trim(calendario$Festividad, side = "both")
calendario$Festividad <- str_to_title(calendario$Festividad, locale = "es")
calendario$Festividad <- chartr('áéíóú','aeiou', calendario$Festividad)
calendario$Festividad <- str_replace(calendario$Festividad, "Espania", "España")
calendario$Festividad <- str_replace(calendario$Festividad, "Constitucion Española", "Constitucion")
calendario$Festividad <- str_replace(calendario$Festividad, "Traslado De La", "Traslado")
calendario$Festividad <- str_replace(calendario$Festividad, "Traslado  San Jose", "Traslado San Jose")
calendario$Festividad <- str_replace(calendario$Festividad, "Festividad De", "")


# Guardamos fichero maestro del Calendario Laboral 
if(!file.exists("data")){
  dir.create("data")
}

write.csv(calendario, "data/CalendarioLaboral.csv")

# Hago limpieza de objetos en memoria
rm(list = ls())    

