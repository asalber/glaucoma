# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Análisis del cubo macular de pacientes con glaucoma y sanos

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "reshape2", "GGally", "gridExtra", "ggpubr", "DT", "psych", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos
# Grosor RPE
col.types = c(rep("text",3),"date", rep("text",3), rep("date",2), rep("numeric",2), rep("text",4), rep("numeric",64)) 
datos.RPE <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_EPR", col_types = col.types)
datos.RPE$Layer <- "RPE"
# Grosor GCL
datos.GCL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_GCL", col_types = col.types)
datos.GCL$Layer <- "GCL"
# Grosor INL
datos.INL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_INL", col_types = col.types)
datos.INL$Layer <- "INL"
# Grosor IPL
datos.IPL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_IPL", col_types = col.types)
datos.IPL$Layer <- "IPL"
# Grosor ONL
datos.ONL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_ONL", col_types = col.types)
datos.ONL$Layer <- "ONL"
# Grosor OPL
datos.OPL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_OPL", col_types = col.types)
datos.OPL$Layer <- "OPL"
# Grosor RNFL
datos.RNFL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_RNFL", col_types = col.types)
datos.RNFL$Layer <- "RNFL"
# Grosor total
datos.FULL <- read_excel("datos/datos-glaucoma.xlsx", sheet = "P_P_FULL_Thickness", col_types = col.types)
datos.FULL$Layer <- "FULL"
# Fusión de datos
datos <- rbind(datos.RPE, datos.GCL, datos.INL, datos.IPL, datos.ONL, datos.OPL, datos.RNFL, datos.FULL)
# Normalización de nombres de variables
colnames(datos) <- make.names(colnames(datos))
colnames(datos)[16:79] <- paste0("Cell.",substr(colnames(datos)[16:79],6,8))

# Eliminación datos erroneos
datos.atipicos <- read.csv(file="datos/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- datos.atipicos[is.na(datos.atipicos$SUMA.V) | datos.atipicos$SUMA.V>10, "Lastname"]
datos <- datos[!(datos$Lastname %in% id.atipicos), ]

# Conjuntos de variables
varCells <- startsWith(colnames(datos),"Cell")
varCells <- colnames(datos)[varCells]
varCells <- NULL
for (i in 1:8){
  for (j in 1:8){
    varCells <- c(varCells, paste0("Cell.",j,".",i))
  }
}
matrixCells <- matrix(varCells, nrow=8, ncol=8, byrow = T)
submatrixCells1 <- matrixCells[1:4,1:4]
submatrixCells2 <- matrixCells[1:4,5:8]
submatrixCells3 <- matrixCells[5:8,1:4]
submatrixCells4 <- matrixCells[5:8,5:8]
submatrixCells <- list(submatrixCells1, submatrixCells2, submatrixCells3, submatrixCells4) 

# Normalización de datos

# Sustitución datos atípicos
removeOutliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75, 0.05, 0.95), na.rm = T)
  iqr <- qnt[2]-qnt[1]
  fence1 <- qnt[1]-1.5*iqr
  fence2 <- qnt[2]+1.5*iqr
  x[x < fence1] <- qnt[3]
  x[x > fence2] <- qnt[4]
  return(x)
}
 
datos[datos$Glaucoma=="Y" & datos$Layer=="RPE", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="RPE", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="RPE", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="RPE", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Layer=="GCL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="GCL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="GCL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="GCL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Layer=="INL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="INL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="INL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="INL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Layer=="IPL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="IPL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="IPL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="IPL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Layer=="ONL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="ONL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="ONL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="ONL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Layer=="OPL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="OPL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="OPL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="OPL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Layer=="RNFL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="RNFL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="RNFL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="RNFL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Layer=="FULL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Layer=="FULL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Layer=="FULL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Layer=="FULL", varCells], removeOutliers)

# Selección de datos
# Ojo izquierdo 
datos.L <- datos[datos$Eye=="L",]
# Ojo derecho
datos.R <- datos[datos$Eye=="R",]
# Datos Glaucoma
datos.Glaucoma = datos[datos$Glaucoma=="Y",]
# Datos Sanos
datos.Sanos = datos[datos$Glaucoma=="N",]


# ANÁLISIS DESCRIPTIVO
# Funciones para dibujar distribuciones por grupos
# Resumen estadístico
describir <- function(vars, eye, layer){
  result <- describeBy(datos[datos$Eye==eye & datos$Layer==layer, varCells], datos[datos$Eye==eye & datos$Layer==layer, "Glaucoma"], mat = T, digits = 4)
  result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
  colnames(result)[1] <- "Glaucoma"
  return(result)
}
# Histogramas de las variables vars de la capa layer según glaucoma y ojos
overHist <- function(vars, layer){
  p <- ggplot(datos[datos$Layer==layer,], aes_string(x = vars)) + geom_histogram(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + facet_grid(Eye~.)
  return(p)
}

cellHist <- function(matrix, layer){
  for (i in 1:length(matrix)){
    p <- lapply(unlist(matrix[i]), overHist, layer=layer)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Densidad de las variables vars de la capa layer según glaucoma y ojos
overDensity <- function(vars, layer){
  p <- ggplot(datos[datos$Layer==layer,], aes_string(x = vars)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + facet_grid(Eye~.)
  return(p)
}

cellDensity <- function(matrix, layer){
  for (i in 1:length(matrix)){
    p <- lapply(unlist(matrix[i]), overDensity, layer=layer)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Diagrama de las variables vars de la capa layer según glaucoma y ojos
overBoxplot <- function(vars, layer){
  p <- ggplot(datos[datos$Layer==layer,], aes_string(x="Eye", y = vars)) + geom_boxplot(aes(fill=Glaucoma))
  return(p)
}

cellBoxplot <- function(matrix, layer){
  for (i in 1:length(matrix)){
    p <- lapply(unlist(matrix[i]), overBoxplot, layer=layer)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

cellCorrelation <- function (eye, layer){
  cormat <- round(cor(datos[datos$Eye==eye & datos$Layer==layer, varCells], use="complete.obs"),2)
  datatable(cormat, rownames = T, escape=F, options = list(pageLength = 8, autoWidth = T, language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
  melted.cormat <- melt(cormat)
  colnames(melted.cormat) <- c("Cell1", "Cell2", "Valor")
  melted.cormat$Cell1 <- substr(melted.cormat$Cell1, 6, 8)
  melted.cormat$Cell2 <- substr(melted.cormat$Cell2, 6, 8)
  print(ggplot(data = melted.cormat, aes(x=Cell1, y=Cell2, fill=Valor)) + geom_tile())
}

# Capa RPE
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "FULL"))

# Correlación entre las celdas
cellCorrelation("L", "FULL")

# Ojo derecho
# Estadísticos descriptivos
print(describir(varCells, "R", "FULL"))

# Correlación entre las celdas
cellCorrelation("R", "FULL")

# Histogramas
cellHist(submatrixCells, "FULL")

# Densidad
cellDensity(submatrixCells, "FULL")

# Diagrama de cajas
cellBoxplot(submatrixCells, "FULL")


# Capa GCL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "GCL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Layer=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

# Ojo derecho
# Estadísticos descriptivos
print(describir(varCells, "R", "GCL"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatrixCells, "GCL")

# Densidad
cellDensity(submatrixCells, "GCL")

# Diagrama de cajas
cellBoxplot(submatrixCells, "GCL")

# Comparación de medias del grosor de las celdas del cubo macular de enfermos y sanos según ojos
comparar.medias <- function (layer, eye){
  datos.layer.eye <- datos[datos$Eye==eye & datos$Layer==layer, ]
  result <- data.frame(t(sapply(datos.layer.eye[, varCells], function(x) unlist(t.test(x~datos.layer.eye$Glaucoma)[c("estimate","conf.int", "p.value")]))))
  result$difference <- result$estimate.mean.in.group.N-result$estimate.mean.in.group.Y
  result<- result[c(1,2,6,3,4,5)]
  colnames(result) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
  return(format(result, nsmall=2))
}

# Ojo izquierdo
medias.L <- comparar.medias("FULL", "L")
medias.R <- comparar.medias("FULL", "R")
# Intervalos de confianza para la diferencia de medias
medias.L$Cell <- substr(row.names(medias.L),6,8)
medias.L$Cell <- factor(medias.L$Cell, levels = medias.L$Cell)
medias.L$Eye <- "L"
medias.R$Cell <- substr(row.names(medias.R),6,8)
medias.R$Cell <- factor(medias.R$Cell, levels = medias.R$Cell)
medias.R$Eye <- "R"
result <- rbind(medias.L, medias.R)
ggplot(result, aes(x = Cell, y = Diferencia.medias, colour=Eye)) + 
  geom_errorbar(aes(ymax = lim.sup.int.conf, ymin = lim.inf.int.conf), position = position_dodge(width = 0.5), width=.2) +
  geom_point(size = 2, position = position_dodge(0.5)) 

# Anillo 3.5 ojo izquierdo
result.L <- data.frame(t(sapply(datos.L[,var3.5], function(x) unlist(t.test(x~datos.L$Glaucoma)[c("estimate","conf.int", "p.value")]))))
result.L$difference <- result.L$estimate.mean.in.group.N-result.L$estimate.mean.in.group.Y
result.L <- result.L[c(1,2,6,3,4,5)]
colnames(result.L) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result.L)

# Anillo 3.5 ojo derecho
result.R <- data.frame(t(sapply(datos.R[,var3.5], function(x) unlist(t.test(x~datos.R$Glaucoma)[c("estimate","conf.int", "p.value")]))))
result.R$difference <- result.R$estimate.mean.in.group.N-result.R$estimate.mean.in.group.Y
result.R <- result.R[c(1,2,6,3,4,5)]
colnames(result.R) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result.R)

# Intervalos de confianza para la diferencia de medias
result.L$Sector <- row.names(result.L)
result.L$Eye <- "L"
result.R$Sector <- row.names(result.R)
result.R$Eye <- "R"
result <- rbind(result.L, result.R)
ggplot(result, aes(x = Sector, y = Diferencia.medias, colour=Eye)) + 
  geom_errorbar(aes(ymax = lim.sup.int.conf, ymin = lim.inf.int.conf), position = position_dodge(width = 0.5), width=.2) +
  geom_point(size = 2, position = position_dodge(0.5)) 






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
