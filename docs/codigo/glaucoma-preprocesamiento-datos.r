# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Clusters de pacientes con  glaucoma

# Carga de paquetes
.packages <- c("tidyverse", "readxl")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, library, character.only=T)

# Carga data anillos
df.anillos <- read_csv("data/datos.csv", col_types = paste0("cfffc",paste0(rep("d",33), collapse = ""))) %>%
  # Convertir fechas
  mutate(ExamDate = as.Date(ExamDate, format="%m/%d/%Y")) %>%
  # Crear clave
  mutate(Id = paste(Patient.Id, ExamDate, Eye, sep="-")) %>%
  # Reordenar columnas
  select(Id, everything())


# Carga de datos atipicos 
df.atipicos <- read.csv(file="data/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- df.atipicos[is.na(df.atipicos$SUMA.V) | df.atipicos$SUMA.V>10, "Lastname"]
# Eliminación de datos atípicos
df.anillos <- df.anillos %>% 
  filter(!Patient.Id %in% id.atipicos) %>%
  # Eliminación data erroneos 
  filter(!Id %in% c("2587-2018-02-14-L",  "16091-2016-11-11-R", "1008-2017-07-26-L",  "1796-2016-10-04-L",  "1796-2016-10-04-R"))
    
# Normalización de datos
# El grosos de la capa nerviosa depende de la edad y del área del BMO, por lo que hay que aplicar un modelo de regresión para obtener la media y la desviación típica a utilizar en la normalización. 
coef.norm <- read.csv(file="data/tabla-normalizacion.csv", encoding = "UTF-8", header=T, row.names=1, sep=",")
age.mean <- 52.17
bmo.area.mean <- 1.781
for (i in colnames(df.anillos)[12:ncol(df.anillos)]){
  df.anillos[[i]] <- (df.anillos[[i]]-coef.norm[i,"Media"]-coef.norm[i,"Pendiente.edad"]*(df.anillos$Age-age.mean)-coef.norm[i,"Pendiente.area.bmo"]*(df.anillos$BMO.Area-bmo.area.mean))/coef.norm[i,"Desviacion"]
  # Descomentar la siguiente línea para trabajar con percentiles
  # data[[i]] <- pnorm(data[[i]])*100
}

# Renombrar variables
names(df.anillos) <- gsub("Rim", "RNFL", names(df.anillos))
df.anillos <- df.anillos %>% 
  rename(Ojo = Eye, Sexo = Gender, Edad = Age)
# Renombrar los niveles del sexo
levels(df.anillos$Sexo) <- list(H = "M", M = "F")

# Carga datos macula
col.types = c(rep("text",3),"date", rep("text",3), rep("date",2), rep("numeric",2), rep("text",4), rep("numeric",64)) 
# Grosor RNFL
df.RNFL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_RNFL", col_types = col.types) %>%
  mutate(Layer = "RNFL")
# Grosor GCL
df.GCL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_GCL", col_types = col.types) %>%
  mutate(Layer = "GCL")
# Grosor IPL
df.IPL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_IPL", col_types = col.types) %>%
  mutate(Layer = "IPL")
# Grosor INL
df.INL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_INL", col_types = col.types) %>%
  mutate(Layer = "INL")
# Grosor OPL
df.OPL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_OPL", col_types = col.types) %>%
  mutate(Layer = "OPL")
# Grosor ONL
df.ONL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_ONL", col_types = col.types) %>%
  mutate(Layer = "ONL")
# Grosor PRL (ATENCIÓN CAPA LEÍDA A POSTERIORI CON DATOS DE OTRO FICHERO)
df.PRL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_PRL", col_types = col.types) %>%
  mutate(Layer = "PRL")
# Eliminar pacientes que no están en las otras capas (esta capa se leyó a posteriori en un fichero excel distinto y contiene más pacientes de que las otras)
# Crear claves de pacientes a partir de cualquiera de las otras capas
id <- df.RNFL %>% mutate(Id=paste(PatientID, ExamDate, Eye, sep="-"))
# Crear claves en la capa PRL
df.PRL <- df.PRL %>% 
  mutate(Id=paste(PatientID, ExamDate, Eye, sep="-")) %>% 
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
df.RPE <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_RPE", col_types = col.types) %>%
  mutate(Layer = "RPE")
# Grosor total
df.FULL <- read_excel("data/datos-glaucoma.xlsx", sheet = "P_P_FULL", col_types = col.types) %>%
  mutate(Layer = "FULL")
# Fusión de datos
df.macula <- rbind(df.RNFL, df.GCL, df.IPL, df.INL, df.OPL, df.ONL, df.PRL, df.RPE, df.FULL)
# Crear clave
df.macula <- df.macula %>% dplyr::mutate(Id=paste(PatientID, ExamDate, Eye, sep="-")) %>%
  # Mover la clave y la capa al las primeras columnas
  select(Id, Layer, everything()) %>%
  # Eliminación datos atípicos
  filter(!Lastname %in% id.atipicos) %>%
  # Eliminación de datos innecesarios
  select(-c("PatientID", "Lastname", "Firstname", "DOB", "ImageID", "ExamDate", "ExamTime", "Quality", "ARTMean", "ImageType", "Upper Layer", "Lower Layer", "GridType"))
# Normalización de nombres de variables
colnames(df.macula) <- make.names(colnames(df.macula))
colnames(df.macula)[5:68] <- paste0("Celda",substr(colnames(df.macula)[5:68],6,8))
# Convertir en factores el glaucoma y el ojo y la capa
df.macula <- df.macula %>% 
  mutate_at(vars(Glaucoma, Layer, Eye), factor) %>%
  mutate(Layer = fct_relevel(Layer, "RNFL", "GCL", "IPL", "INL", "OPL", "ONL", "PRL", "RPE", "FULL"))

# Expandir capas a columnas 
df.macula.expandido <- gather(df.macula, cell, value, -c(Glaucoma, Eye, Layer, Id)) %>%
  unite(temp, Layer, cell, sep = ".") %>%
  spread(temp, value)
# Renombrar variables
df.macula <- df.macula %>% rename(Ojo = Eye, Capa = Layer)
df.macula.expandido <- df.macula.expandido %>% rename(Ojo = Eye, )

# Fusionar data mácula y anillos
df <- inner_join(df.anillos, df.macula.expandido)

# Exportar data 
write_csv(df, file = "data/datos-preprocesados.csv")
save(df, file = "data/datos-preprocesados.RData")
write_csv(df.anillos, file = "data/data-rims-preprocesed.csv")
save(df.anillos, file = "data/data-rims-preprocesed.RData")
write_csv(df.macula, file = "data/datos-macula-preprocesados.csv")
save(df.macula, file = "data/datos-macula-preprocesados.RData")
write_csv(df.macula.expandido, file = "data/datos-macula-preprocesados-expandido.csv")
save(df.macula.expandido, file = "data/datos-macula-preprocesados-expandido.RData")
