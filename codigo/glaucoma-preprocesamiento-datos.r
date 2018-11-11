# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Clusters de pacientes con  glaucoma

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "magrittr")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga datos anillos
datos.anillos <- read.csv(file="datos/datos.csv", header=T, sep=",")
# Convertir fechas
datos.anillos$ExamDate <- as.Date(datos.anillos$ExamDate, format="%m/%d/%Y")
# Crear clave
datos.anillos %<>% dplyr::mutate(Id=paste(Patient.Id, ExamDate, Eye, sep="-"))
# Reordenar columnas
datos.anillos %<>% dplyr::select(Id, everything())
# Eliminación datos erroneos
datos.anillos %<>% filter(!Id %in% c("2587-2018-02-14-L",  "16091-2016-11-11-R", "1008-2017-07-26-L",  "1796-2016-10-04-L",  "1796-2016-10-04-R"))
# Eliminación datos atipicos
datos.atipicos <- read.csv(file="datos/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- datos.atipicos[is.na(datos.atipicos$SUMA.V) | datos.atipicos$SUMA.V>10, "Lastname"]
datos.anillos %<>% filter(!Patient.Id %in% id.atipicos)
# Normalización de datos
# El grosos de la capa nerviosa depende de la edad y del área del BMO, por lo que hay que aplicar un modelo de regresión para obtener la media y la desviación típica a utilizar en la normalización. 
coef.norm <- read.csv(file="datos/tabla-normalizacion.csv", encoding = "UTF-8", header=T, row.names=1, sep=",")
age.mean <- 52.17
bmo.area.mean <- 1.781
for (i in colnames(datos.anillos)[12:ncol(datos.anillos)]){
  datos.anillos[[i]] <- (datos.anillos[[i]]-coef.norm[i,"Media"]-coef.norm[i,"Pendiente.edad"]*(datos.anillos$Age-age.mean)-coef.norm[i,"Pendiente.area.bmo"]*(datos.anillos$BMO.Area-bmo.area.mean))/coef.norm[i,"Desviacion"]
  # Descomentar la siguiente línea para trabajar con percentiles
  # datos[[i]] <- pnorm(datos[[i]])*100
}
# Renombrar variables
names(datos.anillos) <- gsub("Rim", "Anillo", names(datos.anillos))
names(datos.anillos) <- gsub("BMO", "AnilloBMO", names(datos.anillos))
datos.anillos %<>% dplyr::rename(Ojo=Eye, Sexo=Gender, Edad=Age)



# Carga datos macula
col.types = c(rep("text",3),"date", rep("text",3), rep("date",2), rep("numeric",2), rep("text",4), rep("numeric",64)) 
# Grosor RNFL
datos.RNFL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_RNFL", col_types = col.types)
datos.RNFL$Layer <- "RNFL"
# Grosor GCL
datos.GCL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_GCL", col_types = col.types)
datos.GCL$Layer <- "GCL"
# Grosor IPL
datos.IPL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_IPL", col_types = col.types)
datos.IPL$Layer <- "IPL"
# Grosor INL
datos.INL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_INL", col_types = col.types)
datos.INL$Layer <- "INL"
# Grosor OPL
datos.OPL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_OPL", col_types = col.types)
datos.OPL$Layer <- "OPL"
# Grosor ONL
datos.ONL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_ONL", col_types = col.types)
datos.ONL$Layer <- "ONL"
# Grosor RPE
datos.RPE <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_EPR", col_types = col.types)
datos.RPE$Layer <- "RPE"
# Grosor total
datos.FULL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_FULL_Thickness", col_types = col.types)
datos.FULL$Layer <- "FULL"
# Fusión de datos
datos.macula <- rbind(datos.RNFL, datos.GCL, datos.IPL, datos.INL, datos.OPL, datos.ONL, datos.RPE, datos.FULL)
# Crear clave
datos.macula %<>% dplyr::mutate(Id=paste(PatientID, ExamDate, Eye, sep="-"))
# Mover la clave y la capa al las primeras columnas
datos.macula %<>% dplyr::select(Id, Layer, everything())
# Eliminación datos atípicos
datos.atipicos <- read.csv(file="datos/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- datos.atipicos[is.na(datos.atipicos$SUMA.V) | datos.atipicos$SUMA.V>10, "Lastname"]
datos.macula %<>% filter(!Lastname %in% id.atipicos)
# Eliminación de datos innecesarios
remove <- c("PatientID", "Lastname", "Firstname", "DOB", "ImageID", "ExamDate", "ExamTime", "Quality", "ARTMean", "ImageType", "Upper Layer", "Lower Layer", "GridType") 
datos.macula %<>% dplyr::select(-remove)
# Normalización de nombres de variables
colnames(datos.macula) <- make.names(colnames(datos.macula))
colnames(datos.macula)[5:68] <- paste0("Celda",substr(colnames(datos.macula)[5:68],6,8))
# Convertir en factores el glaucoma y el ojo y la capa
datos.macula$Glaucoma <- factor(datos.macula$Glaucoma, levels=c("N","Y"))
datos.macula$Eye <- factor(datos.macula$Eye, levels = c("L", "R"))
datos.macula$Layer <- factor(datos.macula$Layer, levels = c("RNFL", "GCL", "IPL", "INL", "OPL", "ONL", "RPE", "FULL"))
# Expandir capas a columnas 
datos.macula.expandido <- gather(datos.macula, cell, value, -c(Glaucoma, Eye, Layer, Id)) %>%
  unite(temp, Layer, cell, sep = ".") %>%
  spread(temp, value)
# Renombrar variables
datos.macula %<>% dplyr::rename(Ojo=Eye, Capa=Layer)
datos.macula.expandido %<>% dplyr::rename(Ojo=Eye)


# Fusionar datos mácula y anillos
datos <- inner_join(datos.anillos, datos.macula.expandido)

# Exportar datos 
write.csv(file="datos/datos-preprocesados.csv", datos, row.names=F)
write.csv(file="datos/datos-anillos-preprocesados.csv", datos.anillos, row.names=F)
write.csv(file="datos/datos-macula-preprocesados.csv", datos.macula, row.names=F)
write.csv(file="datos/datos-macula-preprocesados-expandido.csv", datos.macula.expandido, row.names=F)
