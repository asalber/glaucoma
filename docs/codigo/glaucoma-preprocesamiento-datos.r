# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Clusters de pacientes con  glaucoma

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "magrittr")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga data anillos
data.anillos <- read.csv(file="datos/datos.csv", header=T, sep=",")
# Convertir fechas
data.anillos$ExamDate <- as.Date(data.anillos$ExamDate, format="%m/%d/%Y")
# Crear clave
data.anillos %<>% dplyr::mutate(Id=paste(Patient.Id, ExamDate, Eye, sep="-"))
# Reordenar columnas
data.anillos %<>% dplyr::select(Id, everything())
# Eliminación data erroneos 
data.anillos %<>% filter(!Id %in% c("2587-2018-02-14-L",  "16091-2016-11-11-R", "1008-2017-07-26-L",  "1796-2016-10-04-L",  "1796-2016-10-04-R"))
# Eliminación data atipicos
data.atipicos <- read.csv(file="datos/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- data.atipicos[is.na(data.atipicos$SUMA.V) | data.atipicos$SUMA.V>10, "Lastname"]
data.anillos %<>% filter(!Patient.Id %in% id.atipicos)
# Normalización de data
# El grosos de la capa nerviosa depende de la edad y del área del BMO, por lo que hay que aplicar un modelo de regresión para obtener la media y la desviación típica a utilizar en la normalización. 
coef.norm <- read.csv(file="datos/tabla-normalizacion.csv", encoding = "UTF-8", header=T, row.names=1, sep=",")
age.mean <- 52.17
bmo.area.mean <- 1.781
for (i in colnames(data.anillos)[12:ncol(data.anillos)]){
  data.anillos[[i]] <- (data.anillos[[i]]-coef.norm[i,"Media"]-coef.norm[i,"Pendiente.edad"]*(data.anillos$Age-age.mean)-coef.norm[i,"Pendiente.area.bmo"]*(data.anillos$BMO.Area-bmo.area.mean))/coef.norm[i,"Desviacion"]
  # Descomentar la siguiente línea para trabajar con percentiles
  # data[[i]] <- pnorm(data[[i]])*100
}
# Renombrar variables
names(data.anillos) <- gsub("Rim", "RNFL", names(data.anillos))
data.anillos %<>% dplyr::rename(Ojo=Eye, Sexo=Gender, Edad=Age)
# Convertir glaucoma, ojo y sexo en factores
data.anillos$Glaucoma <- factor(data.anillos$Glaucoma, levels=c("N","Y"))
data.anillos$Ojo <- factor(data.anillos$Ojo, levels = c("L", "R"))
data.anillos$Sexo <- factor(data.anillos$Sexo, levels = c("F", "M"))
# Renombrar los niveles del sexo
levels(data.anillos$Sexo) <- list(H="M", M="F")

# Carga datos macula
col.types = c(rep("text",3),"date", rep("text",3), rep("date",2), rep("numeric",2), rep("text",4), rep("numeric",64)) 
# Grosor RNFL
data.RNFL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_RNFL", col_types = col.types)
data.RNFL$Layer <- "RNFL"
# Grosor GCL
data.GCL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_GCL", col_types = col.types)
data.GCL$Layer <- "GCL"
# Grosor IPL
data.IPL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_IPL", col_types = col.types)
data.IPL$Layer <- "IPL"
# Grosor INL
data.INL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_INL", col_types = col.types)
data.INL$Layer <- "INL"
# Grosor OPL
data.OPL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_OPL", col_types = col.types)
data.OPL$Layer <- "OPL"
# Grosor ONL
data.ONL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_ONL", col_types = col.types)
data.ONL$Layer <- "ONL"

# Grosor PRL (ATENCIÓN CAPA LEÍDA A POSTERIORI CON DATOS DE OTRO FICHERO)
data.PRL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_PRL", col_types = col.types)
data.PRL$Layer <- "PRL"
# Eliminar pacientes que no están en las otras capas (esta capa se leyó a posteriori en un fichero excel distinto y contiene más pacientes de que las otras)
# Crear claves de pacientes a partir de cualquiera de las otras capas
id <- data.RNFL %>% dplyr::mutate(Id=paste(PatientID, ExamDate, Eye, sep="-"))
# Crear claves en la capa PRL
data.PRL <- data.PRL %>% 
  dplyr::mutate(Id=paste(PatientID, ExamDate, Eye, sep="-")) %>% 
  distinct(Id, .keep_all = TRUE) %>% 
  filter(Id %in% id$Id) %>%
  # Eliminar columna Glaucoma (vacía)
  select(-Glaucoma) %>%
  # Añadir columna Glaucoma combinando con otra capa 
  left_join(select(id, c(Glaucoma, Id))) %>%
  # Eliminar la columna Id
  select(-Id)
# Pacientes que no están 17289, DB836, DB548 y 28852
# Pacientes repetidos: 3485, 8228, 10373, 12770, 16756, 16941, 17150, 17213, DB160

# Grosor RPE
data.RPE <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_RPE", col_types = col.types)
data.RPE$Layer <- "RPE"
# Grosor total
data.FULL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_FULL", col_types = col.types)
data.FULL$Layer <- "FULL"
# Fusión de datos
data.macula <- rbind(data.RNFL, data.GCL, data.IPL, data.INL, data.OPL, data.ONL, data.PRL, data.RPE, data.FULL)
# Crear clave
data.macula %<>% dplyr::mutate(Id=paste(PatientID, ExamDate, Eye, sep="-"))
# Mover la clave y la capa al las primeras columnas
data.macula %<>% dplyr::select(Id, Layer, everything())
# Eliminación datos atípicos
data.atipicos <- read.csv(file="datos/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- data.atipicos[is.na(data.atipicos$SUMA.V) | data.atipicos$SUMA.V>10, "Lastname"]
data.macula %<>% filter(!Lastname %in% id.atipicos)
# Eliminación de datos innecesarios
remove <- c("PatientID", "Lastname", "Firstname", "DOB", "ImageID", "ExamDate", "ExamTime", "Quality", "ARTMean", "ImageType", "Upper Layer", "Lower Layer", "GridType") 
data.macula %<>% dplyr::select(-remove)
# Normalización de nombres de variables
colnames(data.macula) <- make.names(colnames(data.macula))
colnames(data.macula)[5:68] <- paste0("Celda",substr(colnames(data.macula)[5:68],6,8))
# Convertir en factores el glaucoma y el ojo y la capa
data.macula$Glaucoma <- factor(data.macula$Glaucoma, levels=c("N","Y"))
data.macula$Eye <- factor(data.macula$Eye, levels = c("L", "R"))
data.macula$Layer <- factor(data.macula$Layer, levels = c("RNFL", "GCL", "IPL", "INL", "OPL", "ONL", "PRL", "RPE", "FULL"))
# Expandir capas a columnas 
data.macula.expandido <- gather(data.macula, cell, value, -c(Glaucoma, Eye, Layer, Id)) %>%
  unite(temp, Layer, cell, sep = ".") %>%
  spread(temp, value)
# Renombrar variables
data.macula %<>% dplyr::rename(Ojo=Eye, Capa=Layer)
data.macula.expandido %<>% dplyr::rename(Ojo=Eye)

# Fusionar data mácula y anillos
data <- inner_join(data.anillos, data.macula.expandido)

# Exportar data 
write.csv(file = "datos/datos-preprocesados.csv", data, row.names=F)
save(data, file = "datos/datos-preprocesados.RData")
write.csv(file = "datos/datos-anillos-preprocesados.csv", data.anillos, row.names=F)
save(data.anillos, file = "datos/datos-anillos-preprocesados.RData")
write.csv(file = "datos/datos-macula-preprocesados.csv", data.macula, row.names=F)
save(data.macula, file = "datos/datos-macula-preprocesados.RData")
write.csv(file = "datos/datos-macula-preprocesados-expandido.csv", data.macula.expandido, row.names=F)
save(data.macula.expandido, file = "datos/datos-macula-preprocesados-expandido.RData")
