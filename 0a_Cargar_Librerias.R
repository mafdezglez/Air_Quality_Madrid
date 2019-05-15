##########################################################################
# Miguel A. Fernandez
# TFM - Air Quality in Madrid

# En este script se instalaran y cargaran las librerias necesarias
# para el proyecto
##########################################################################

list.of.packages <- c("R.utils", "tidyverse", "doParallel", "foreach", "sqldf", "geosphere", 
                      "dplyr", "data.table", "stringr", "readr", "readxl", "tydir", "lubridate",
                      "ggplot2", "zoo", "GGally", "googledrive")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(readxl)
library(tidyr)
library(geosphere)
library(lubridate)
library(ggplot2)
library(zoo)
library(GGally)
library(googledrive)
