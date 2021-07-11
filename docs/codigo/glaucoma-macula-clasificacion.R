# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Clasificación del glaucoma en estadíso según el cubo macular

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "reshape2", "GGally", "gridExtra", "ggpubr", "DT", "psych", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos (ver fichero glaucoma-preprocesamiento-datos.r)
data.anillos <- read.csv(file="data/datos-anillos-preprocesados-estadios.csv", header=T, sep=",")
data.macula <- read.csv(file="data/datos-macula-preprocesados.csv", header=T, sep=",")
# Seleccionar ojos izquierdos de la mácula
data.macula %<>% filter(Ojo=="L")

# Añadir estadios al conjunto de datos de la mácula
data <- left_join(data.macula, data.anillos[, c("Id", "Estadio")], by="Id") %>%
  # Mover la columna del Estadio al principio
  dplyr::select(Id, Capa, Estadio, everything())

# Seleccionar casos completos
data <- data[complete.cases(data), ]
