# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)

# Carga de paquetes
.packages <- c("dplyr", "ggplot2", "GGally", "gridExtra", "ggpubr", "DT", "psych", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos
datos <- read.csv(file="datos/datos.csv", header=T, sep=",")

# Normalización de datos
# El grosos de la capa nerviosa depende de la edad y del área del BMO, por lo que hay que aplicar un modelo de regresión para obtener la media y la desviación típica a utilizar en la normalización. 
coef.norm <- read.csv(file="datos/tabla-normalizacion.csv", encoding = "UTF-8", header=T, row.names=1, sep=",")
age.mean <- 52.17
bmo.area.mean <- 1.781
for (i in colnames(datos)[11:ncol(datos)]){
  datos[[i]] <- (datos[[i]]-coef.norm[i,"Media"]-coef.norm[i,"Pendiente.edad"]*(datos$Age-age.mean)-coef.norm[i,"Pendiente.area.bmo"]*(datos$BMO.Area-bmo.area.mean))/coef.norm[i,"Desviacion"]
  # Descomentar la siguiente línea para trabajar con percentiles
  # datos[[i]] <- pnorm(datos[[i]])*100
}

# Selección de datos
# Ojo izquierdo
datos.L <- datos[datos$Eye=="L",]
datos.L$Eye <- NULL
colnames(datos.L)[6:ncol(datos.L)] <- paste0(colnames(datos.L)[6:ncol(datos.L)],rep(".L",ncol(datos.L)-5)) 
# Ojo derecho
datos.R <- datos[datos$Eye=="R",]
datos.R$Eye <- NULL
colnames(datos.R)[6:ncol(datos.R)] <- paste0(colnames(datos.R)[6:ncol(datos.R)],rep(".R",ncol(datos.R)-5)) 
# Unión de ojos
datos.tidy <- inner_join(datos.L, datos.R[,-c(2,3,4,5)], by="Patient.Id")
datos.tidy <- na.omit(datos.tidy)

# Comparación de medias pareadas de sectores entre ojos
fun <- function(i) {
  unlist(t.test(datos.tidy[,i], datos.tidy[,i+32], paired = T)[c("estimate","conf.int", "p.value")])
}
result <- t(sapply(10:37, fun))

fun <- function(i) {
  paste(colnames(datos.tidy)[i], "-", colnames(datos.tidy)[i+32])
}
rownames(result) <- sapply(10:37, fun)
result


# Análisis discriminante
datos.BMO <- select(datos, starts_with("BMO"))[-1]
datos.lda <- datos.BMO
datos.lda$Glaucoma <- datos$Glaucoma
datos.lda$Age <- datos$Age
datos.lda <- na.omit(datos.lda)
lda <- MASS::lda(formula=Glaucoma ~ ., data=datos.lda, na.action="na.omit",  CV=TRUE)
#datatable(lda$scaling, options = list(pageLength=100, scrollY=200, dom = 'ft')) %>% formatRound(colnames(lda$scaling), 4)
#plot(lda)
ct <- table(datos.lda$Glaucoma, lda$class)
prop.table(ct,1)
diag(prop.table(ct, 1))

datos.3.5 <- select(datos, starts_with("Rim3.5"))
datos.lda <- datos.3.5
datos.lda$Glaucoma <- datos$Glaucoma
datos.lda <- na.omit(datos.lda)
lda <- MASS::lda(formula=Glaucoma ~ ., data=datos.lda, na.action="na.omit",  CV=TRUE)
#datatable(lda$scaling, options = list(pageLength=100, scrollY=200, dom = 'ft')) %>% formatRound(colnames(lda$scaling), 4)
#plot(lda)
ct <- table(datos.lda$Glaucoma, lda$class)
prop.table(ct,1)
diag(prop.table(ct, 1))

datos.4.1 <- select(datos, starts_with("Rim4.1"))
datos.lda <- datos.4.1
datos.lda$Glaucoma <- datos$Glaucoma
datos.lda <- na.omit(datos.lda)
lda <- MASS::lda(formula=Glaucoma ~ ., data=datos.lda, na.action="na.omit",  CV=TRUE)
#datatable(lda$scaling, options = list(pageLength=100, scrollY=200, dom = 'ft')) %>% formatRound(colnames(lda$scaling), 4)
#plot(lda)
ct <- table(datos.lda$Glaucoma, lda$class)
prop.table(ct,1)
diag(prop.table(ct, 1))

datos.4.7 <- select(datos, starts_with("Rim4.7"))
datos.lda <- datos.4.7
datos.lda$Glaucoma <- datos$Glaucoma
datos.lda <- na.omit(datos.lda)
lda <- MASS::lda(formula=Glaucoma ~ ., data=datos.lda, na.action="na.omit",  CV=TRUE)
#datatable(lda$scaling, options = list(pageLength=100, scrollY=200, dom = 'ft')) %>% formatRound(colnames(lda$scaling), 4)
#plot(lda)
ct <- table(datos.lda$Glaucoma, lda$class)
prop.table(ct,1)
diag(prop.table(ct, 1))

datos.lda <- cbind(datos.BMO, datos.3.5, datos.4.1, datos.4.7)
datos.lda$Glaucoma <- datos$Glaucoma
datos.lda <- na.omit(datos.lda)
lda <- MASS::lda(formula=Glaucoma ~ ., data=datos.lda, na.action="na.omit",  CV=TRUE)
#datatable(lda$scaling, options = list(pageLength=100, scrollY=200, dom = 'ft')) %>% formatRound(colnames(lda$scaling), 4)
#plot(lda)
ct <- table(datos.lda$Glaucoma, lda$class)
prop.table(ct,1)
diag(prop.table(ct, 1))


# Componentes principales
data <- na.omit(datos.BMO)
glaucoma <- data$Glaucoma
data <- data[-ncol(data)]
pca <- prcomp(data) 
print(pca)
plot(pca, type="l")
summary(pca)

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              groups = glaucoma, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
