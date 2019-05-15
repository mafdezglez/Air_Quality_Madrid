
# Funciones para mostrar las gráficas
hacer_grafica_bar <- function(datos, ejex, ejey, limite, labelx, labely, labelfill, labelLimite, titleGrafica) {

  grafica <- ggplot(datos, aes(x=ejex, y=ejey, fill=ejex)) + 
              geom_bar(stat='identity', show.legend = FALSE) +
              geom_hline(aes(yintercept=limite, linetype=labelLimite), color="red") +
              scale_linetype_discrete("") +
              labs(x=labelx, y=labely, fill=labelfill) +
              ggtitle(titleGrafica) +
              theme(text = element_text(size=16, family="Comic Sans MS")) +
              theme(plot.title = element_text(hjust = 0.5)) +
              theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
              theme(axis.title.y = element_text(vjust=1.5, size=rel(1))) + 
              theme(legend.justification = c("right", "top"))
            
  return(grafica)
}

hacer_grafica_point <- function(datos, ejex, ejey, limite, colores, labelx, labely, labelColour, 
                                labelLimite, titleGrafica) {
  
  grafica <- ggplot(datos, aes(x=ejex, y=ejey, colour=colores)) + 
              geom_point(stat='identity', show.legend = FALSE) +
              geom_hline(aes(yintercept=limite, linetype=labelLimite), color="red") +
              scale_linetype_discrete("") +
              labs(x=labelx, y=labely, colour=labelColour) +
              ggtitle(titleGrafica) +
              theme(text = element_text(size=16, family="Comic Sans MS")) +
              theme (plot.title = element_text(hjust = 0.5)) +
              theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
              theme(axis.title.y = element_text(vjust=1.5, size=rel(1))) +
              theme(legend.justification = c("right", "top"))
  
    return(grafica)
}

# Lectura de datos
CalidadAire <- read_csv("data/CalidadAire.csv", col_types = cols(X1 = col_skip())) 
estaciones_CalidadAire <- read_excel("rawdata/pmed_ubicacion_calidad_aire.xlsx")

estaciones_CalidadAire <- select(estaciones_CalidadAire, -c(DIRECCION, LONG, LAT))
CalidadAire <- CalidadAire %>% 
               inner_join(estaciones_CalidadAire, by = c("ESTACION" = "NUMERO")) %>% 
               select(-c(ESTACION)) 

setnames(CalidadAire, old="ESTACION.y", new="ESTACION")

###########################################################################################################
# MEDIAS ANUALES POR CONTAMINANTE
###########################################################################################################

CalidadAire_MediaAnual <- CalidadAire %>% 
                          mutate(ANIO=year(FECHA), ESTACION=as.factor(ESTACION)) %>% 
                          group_by(ANIO, ESTACION) %>% 
                          summarise(SO2  = mean(SO2),
                                    CO   = mean(CO),
                                    NO2  = mean(NO2),
                                    PM10 = mean(PM10),
                                    O3   = mean(O3))

# PM10 - Media anual
datos <- CalidadAire_MediaAnual %>% select(ANIO, ESTACION, PM10) %>% na.omit()
ejex  <- datos$ESTACION
ejey  <- datos$PM10
limite <- 40
labelx <- "Estación"
labely <- "PM10 (μg/m3)"
labelfill <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valor medio anual de PM10"
direction <- "h"

if (nrow(datos)) {
  grafAnual_PM10 <- hacer_grafica_bar(datos, ejex, ejey, limite, labelx, labely, 
                                      labelfill, labelLimite, titleGrafica) +
                    coord_flip() +
                    facet_wrap(~ANIO, dir = direction)
  grafAnual_PM10
} else {
  print("No hay datos para esta gráfica")
}

# NO2 - Media anual
datos <- CalidadAire_MediaAnual %>% select(ANIO, ESTACION, NO2) %>% na.omit()
ejex  <- datos$ESTACION
ejey  <- datos$NO2
limite <- 40
labelx <- "Estación"
labely <- "NO2 (μg/m3)"
labelfill <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valor medio anual de NO2"
direction <- "h"

if (nrow(datos)) {
  grafAnual_NO2 <- hacer_grafica_bar(datos, ejex, ejey, limite, labelx, labely, 
                                     labelfill, labelLimite, titleGrafica) +
                   coord_flip() +
                   facet_wrap(~ANIO, dir = direction)
  grafAnual_NO2  
} else {
  print("No hay datos para esta gráfica")
}

###########################################################################################################
# MEDIAS DIARIAS POR CONTAMINANTE
###########################################################################################################

CalidadAire_MediaDiaria <- CalidadAire %>% 
                           mutate(DIA=date(FECHA), ESTACION=as.factor(ESTACION)) %>%
                           group_by(DIA, ESTACION) %>% 
                           summarise(SO2  = mean(SO2),
                                     CO   = mean(CO),
                                     NO2  = mean(NO2),
                                     PM10 = mean(PM10),
                                     O3   = mean(O3))

# PM10 - Media diaria
datos <- CalidadAire_MediaDiaria %>% select(DIA, ESTACION, PM10) %>% na.omit()
ejex  <- datos$DIA
ejey  <- datos$PM10
limite <- 50
colores <- datos$ESTACION
labelx <- "Día"
labely <- "PM10 (μg/m3)"
labelColour <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valor medio diario de PM10"
direction <- "v"

if (nrow(datos)) {
  grafDiaria_PM10 <- hacer_grafica_point(datos, ejex, ejey, limite, colores, labelx, labely, labelColour, 
                                         labelLimite, titleGrafica) +
    facet_wrap(~ESTACION, dir = direction) 
  grafDiaria_PM10  
} else {
  print("No hay datos para esta gráfica")
}

# PM10 - Nº de días que se ha superado la media diaria legal
datos <- CalidadAire_MediaDiaria %>% 
         select(DIA, ESTACION, PM10) %>% 
         filter(PM10 > 50) %>% 
         mutate(ANIO=year(DIA)) %>% 
         group_by(ESTACION, ANIO) %>% 
         summarise(NumDias = n()) %>% 
         na.omit()

ejex  <- datos$ESTACION
ejey  <- datos$NumDias
limite <- 35
labelx <- "Estación"
labely <- "Nº Días"
labelfill <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Nº de días que se ha superado la media diaria legal de PM10"
direction <- "h"

if (nrow(datos)) {
  grafNumDias_PM10 <- hacer_grafica_bar(datos, ejex, ejey, limite, labelx, labely, 
                                        labelfill, labelLimite, titleGrafica) +
                      coord_flip() +
                      facet_wrap(~ANIO, dir = direction)
  grafNumDias_PM10  
} else {
  print("No hay datos para esta gráfica")
}

# Guardamos fichero maestro del Nº de días que se ha superado la media diaria legal
write.csv(datos, "data/CalidadAire_NumDiasSuperadosPM10.csv")

# SO2 - Media diaria
datos <- CalidadAire_MediaDiaria %>% select(DIA, ESTACION, SO2) %>% na.omit()
ejex  <- datos$DIA
ejey  <- datos$SO2
limite <- 125
colores <- datos$ESTACION
labelx <- "Día"
labely <- "SO2 (μg/m3)"
labelColour <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valor medio diario de SO2"
direction <- "v"

if (nrow(datos)) {
  grafDiaria_SO2 <- hacer_grafica_point(datos, ejex, ejey, limite, colores, labelx, labely, labelColour, 
                                        labelLimite, titleGrafica) +
                    facet_wrap(~ESTACION, dir = direction) 
  grafDiaria_SO2  
} else {
  print("No hay datos para esta gráfica")
}

# SO2 - Nº de días que se ha superado la media diaria legal
datos <- CalidadAire_MediaDiaria %>% 
         select(DIA, ESTACION, SO2) %>% 
         filter(SO2 > 125) %>% 
         mutate(ANIO=year(DIA)) %>% 
         group_by(ESTACION, ANIO) %>% 
         summarise(NumDias = n()) %>% 
         na.omit()

ejex  <- datos$ESTACION
ejey  <- datos$NumDias
limite <- 3
labelx <- "Estación"
labely <- "Nº Días"
labelfill <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Nº de días que se ha superado la media diaria legal de SO2"
direction <- "h"

if (nrow(datos)) {
  grafNumDias_SO2 <- hacer_grafica_bar(datos, ejex, ejey, limite, labelx, labely, 
                                       labelfill, labelLimite, titleGrafica) +
                     facet_wrap(~ANIO, dir = direction)
  grafNumDias_SO2  
} else {
  print("No hay datos para esta gráfica")
}

# Guardamos fichero maestro del Nº de días que se ha superado la media diaria legal
write.csv(datos, "data/CalidadAire_NumDiasSuperadosDiariaSO2.csv")

###########################################################################################################
# MEDIAS HORARIAS POR CONTAMINANTE
###########################################################################################################

CalidadAire_MediaHoraria <- CalidadAire %>% 
                            mutate(ESTACION=as.factor(ESTACION))

# SO2 - Media horaria
datos <- CalidadAire_MediaHoraria %>% select(FECHA, ESTACION, SO2) %>% na.omit()
ejex  <- datos$FECHA
ejey  <- datos$SO2
limite <- 350
colores <- datos$ESTACION
labelx <- "Hora"
labely <- "SO2 (μg/m3)"
labelColour <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valor medio horario de SO2"
direction <- "v"

if (nrow(datos)) {
  grafHoraria_SO2 <- hacer_grafica_point(datos, ejex, ejey, limite, colores, labelx, labely, labelColour, 
                                         labelLimite, titleGrafica) +
                     facet_wrap(~ESTACION, dir = direction) 
  grafHoraria_SO2  
} else {
  print("No hay datos para esta gráfica")
}

# SO2 - Nº de días que se ha superado la media horaria legal
datos <- CalidadAire_MediaHoraria %>% 
         select(FECHA, ESTACION, SO2) %>% 
         filter(SO2 > 350) %>% 
         mutate(ANIO=year(FECHA)) %>% 
         group_by(ESTACION, ANIO) %>% 
         summarise(NumDias = n()) %>% 
         na.omit()

ejex  <- datos$ESTACION
ejey  <- datos$NumDias
limite <- 24
labelx <- "Estación"
labely <- "Nº Días"
labelfill <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Nº de días que se ha superado la media horaria legal de SO2"
direction <- "h"

if (nrow(datos)) {
  grafNumDiasHoraria_SO2 <- hacer_grafica_bar(datos, ejex, ejey, limite, labelx, labely, 
                                        labelfill, labelLimite, titleGrafica) +
                            coord_flip() +
                            facet_wrap(~ANIO, dir = direction)
  grafNumDiasHoraria_SO2  
} else {
  print("No hay datos para esta gráfica")
}

# Guardamos fichero maestro del Nº de días que se ha superado la media horaria legal
write.csv(datos, "data/CalidadAire_NumDiasSuperadosHorariaSO2.csv")

# NO2 - Media horaria
datos <- CalidadAire_MediaHoraria %>% select(FECHA, ESTACION, NO2) %>% na.omit()
ejex  <- datos$FECHA
ejey  <- datos$NO2
limite <- 200
colores <- datos$ESTACION
labelx <- "Hora"
labely <- "NO2 (μg/m3)"
labelColour <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valor medio horario de NO2"
direction <- "v"

if (nrow(datos)) {
  grafHoraria_NO2 <- hacer_grafica_point(datos, ejex, ejey, limite, colores, labelx, labely, labelColour, 
                                         labelLimite, titleGrafica) +
                     facet_wrap(~ESTACION, dir = direction) 
  grafHoraria_NO2
} else {
  print("No hay datos para esta gráfica")
}

# NO2 - Nº de días que se ha superado la media horaria legal
datos <- CalidadAire_MediaHoraria %>% 
         select(FECHA, ESTACION, NO2) %>% 
         filter(NO2 > 200) %>% 
         mutate(ANIO=year(FECHA)) %>% 
         group_by(ESTACION, ANIO) %>% 
         summarise(NumDias = n()) %>% 
         na.omit()

ejex  <- datos$ESTACION
ejey  <- datos$NumDias
limite <- 18
labelx <- "Estación"
labely <- "Nº Días"
labelfill <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Nº de días que se ha superado la media horaria legal de NO2"
direction <- "h"

if (nrow(datos)) {
  grafNumDias_NO2 <- hacer_grafica_bar(datos, ejex, ejey, limite, labelx, labely, 
                                       labelfill, labelLimite, titleGrafica) +
                     coord_flip() +
                     facet_wrap(~ANIO, dir = direction)
  grafNumDias_NO2
} else {
  print("No hay datos para esta gráfica")
}

# Guardamos fichero maestro del Nº de días que se ha superado la media horaria legal
write.csv(datos, "data/CalidadAire_NumDiasSuperadosNO2.csv")

###########################################################################################################
# MÁXIMA DIARIA DE LAS MEDIAS MÓVILES OCTOHORARIAS POR CONTAMINANTE
###########################################################################################################

CalidadAire_OctoHoraria <- CalidadAire %>% 
                           select(-c("SO2", "NO2", "PM10")) 

tmp <- data.frame()
tmp_final <- data.frame()

for (estacion in estaciones_CalidadAire$ESTACION) {
  tmp <- CalidadAire_OctoHoraria %>% 
         filter(ESTACION==estacion) 
  
  tmp <- tmp %>% 
         mutate(CO_OctH = rollmean(tmp$CO,   8, fill=NA, align="right"),
                O3_OctH = rollmean(tmp$O3,   8, fill=NA, align="right")) %>% 
         group_by(date(FECHA), ESTACION) %>% 
         summarise(CO_Max = max(CO_OctH, na.rm = TRUE),
                   O3_Max = max(O3_OctH, na.rm = TRUE))
  
  tmp <- data.frame(tmp)
  tmp_final <- rbind(tmp_final, tmp)
}

CalidadAire_OctoHoraria <- tmp_final %>% 
                           mutate(CO=CO_Max, O3=O3_Max, FECHA=date.FECHA.,
                                  ESTACION=as.factor(ESTACION)) %>% 
                           select(-c("CO_Max", "O3_Max", "date.FECHA.")) 

CalidadAire_OctoHoraria[mapply(is.infinite, CalidadAire_OctoHoraria)] <- NA

# Guardamos fichero maestro de las Máximas diarias Octohorarias del CO y O3 de la Calidad del Aire
write.csv(CalidadAire_OctoHoraria, "data/CalidadAire_DatosOctohorarios.csv")

# CO - Máximos diarios de las medias móviles octohorarias
datos <- CalidadAire_OctoHoraria %>% select(FECHA, ESTACION, CO) %>% na.omit()
ejex  <- datos$FECHA
ejey  <- datos$CO
limite <- 10
colores <- datos$ESTACION
labelx <- "Día"
labely <- "CO (mg/m3)"
labelColour <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valores máximos diarios de las medias móviles octohorarias de CO"
direction <- "v"

if (nrow(datos)) {
  grafHoraria_NO2 <- hacer_grafica_point(datos, ejex, ejey, limite, colores, labelx, labely, labelColour, 
                                         labelLimite, titleGrafica) +
    facet_wrap(~ESTACION, dir = direction) 
  grafHoraria_NO2
} else {
  print("No hay datos para esta gráfica")
}

# O3 - Máximos diarios de las medias móviles octohorarias
datos <- CalidadAire_OctoHoraria %>% select(FECHA, ESTACION, O3) %>% na.omit()
ejex  <- datos$FECHA
ejey  <- datos$O3
limite <- 120
colores <- datos$ESTACION
labelx <- "Día"
labely <- "O3 (μg/m3)"
labelColour <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Valores máximos diarios de las medias móviles octohorarias de O3"
direction <- "v"

if (nrow(datos)) {
  grafHoraria_O3 <- hacer_grafica_point(datos, ejex, ejey, limite, colores, labelx, labely, labelColour, 
                                        labelLimite, titleGrafica) +
                    facet_wrap(~ESTACION, dir = direction) 
  grafHoraria_O3
} else {
  print("No hay datos para esta gráfica")
}

# O3 - Nº de días que se ha superado la máxima diaria de las medias móviles octohorarias legal
datos <- CalidadAire_OctoHoraria %>% 
         select(-c(CO)) %>% 
         filter(O3 > 120) %>% 
         mutate(ANIO=year(FECHA)) %>% 
         group_by(ESTACION, ANIO) %>% 
         summarise(NumDias = n()) %>% 
         na.omit()

ejex  <- datos$ESTACION
ejey  <- datos$NumDias
limite <- 25
labelx <- "Estación"
labely <- "Nº Días"
labelfill <- "Estaciones"
labelLimite <- "Límite legal"
titleGrafica <- "Nº de días que se ha superado la máxima diaria de las medias móviles octohorarias legal de O3"
direction <- "h"

if (nrow(datos)) {
  grafNumDias_O3 <- hacer_grafica_bar(datos, ejex, ejey, limite, labelx, labely, 
                                      labelfill, labelLimite, titleGrafica) +
                    coord_flip() +
                    facet_wrap(~ANIO, dir = direction)
  grafNumDias_O3
} else {
  print("No hay datos para esta gráfica")
}

# Guardamos fichero maestro del Nº de días que se ha superado la máxima diaria de las medias móviles octohorarias
write.csv(datos, "data/CalidadAire_NumDiasSuperadosO3.csv")

# Hago limpieza de objetos en memoria
rm(list = ls())    
