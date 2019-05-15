
CalidadAire <- read_csv("data/CalidadAire.csv", 
                        col_types = cols(X1 = col_skip(), FECHA = col_character()))

Trafico <- read_csv("data/Trafico.csv", 
                    col_types = cols(X1 = col_skip(), fecha = col_character()))

df_Analisis <- inner_join(CalidadAire, Trafico, 
                          by = c("ESTACION" = "estacion_CalidadAire" , 
                                 "FECHA" = "fecha")) 
               

###############################################################################
# SELECCIÓN DE LOS PREDICTORES
###############################################################################

df_Analisis <- df_Analisis %>% 
                  filter(ESTACION==8) %>% 
                  select(-c("ESTACION", "FECHA", "vmed")) %>% 
                  mutate_all(as.numeric) %>% 
                  cor(use="pairwise")

df_Analisis <- data.frame(df_Analisis)

###### Matriz de correlación de las variables 

ggpairs(df_Analisis, lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")

