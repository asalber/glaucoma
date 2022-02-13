# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Glaucoma staging system 
# df preprocessing

# Load packages
.packages <- c("tidyverse")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, library, character.only=T)

# Load rim data
load("data/data-rims-preprocesed.RData")
df <- df.anillos

# Set of variables
varBMO <- startsWith(colnames(df),"BMO")
varBMO[11] <- F
varBMO <- colnames(df)[varBMO]
var3.5 <- startsWith(colnames(df),"RNFL3.5")
var3.5 <- colnames(df)[var3.5]
var4.1 <- startsWith(colnames(df),"RNFL4.1")
var4.1 <- colnames(df)[var4.1]
var4.7 <- startsWith(colnames(df),"RNFL4.7")
var4.7 <- colnames(df)[var4.7]
varG <- endsWith(colnames(df), ".G")
varG <- colnames(df[varG])
varTI <- endsWith(colnames(df), ".TI")
varTI <- colnames(df[varTI])
varT <- endsWith(colnames(df), ".T")
varT <- colnames(df[varT])
varTS <- endsWith(colnames(df), ".TS")
varTS <- colnames(df[varTS])
varNS <- endsWith(colnames(df), ".NS")
varNS <- colnames(df[varNS])
varN <- endsWith(colnames(df), ".N")
varN <- colnames(df[varN])
varNI <- endsWith(colnames(df), ".NI")
varNI <- colnames(df[varNI])
varRims <- c(varBMO, var3.5, var4.1, var4.7)
varSectors <- c(varG, varTI, varT, varTS, varNS, varN, varNI)

# Create a new column concatenating the Patient.Id and the date to identify the measurement id
df <- df %>% unite("Measurement.Id", Patient.Id, ExamDate, sep = "-", remove = F)

# Variables selection
df <- dplyr::select(df, c("Id", "Measurement.Id", "Glaucoma", "Edad" , "Sexo", "Ojo", "FoBMO.Angle", "Displacement", "BMO.Area", all_of(varRims)))
# Select complete cases
df <- df[complete.cases(df), ]
# Filter left eyes
df.L <- df %>% filter(Ojo == "L")
# Filter right eyes
df.R <- df %>% filter(Ojo == "R")
# Filter glaucoma eyes
df.glaucoma <- df %>% filter(Glaucoma == "Y")
# Filter healthy eyes
df.health <- df %>% filter(Glaucoma == "N")

