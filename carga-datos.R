# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Glaucoma staging system 
# df preprocessing

# Load packages
.packages <- c("tidyverse")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# df loading (see file df-preprocessing.R)
load("data/datos-RNFLs-preprocesados.Rdf")
df <- df.RNFLs

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

# Variables selection
df <- dplyr::select(df, c("Id", "Ojo", "Glaucoma", varRims))
# Select complete cases
df <- df[complete.cases(df), ]


