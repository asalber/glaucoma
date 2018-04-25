# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)

# Carga de paquetes
.packages <- c("dplyr", "ggplot2", "GGally", "gridExtra", "ggpubr", "DT", "psych", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos
datos <- read.csv(file="datos/datos.csv", header=T, sep=",")

# Conjuntos de variables
varBMO <- startsWith(colnames(datos),"BMO")
varBMO[10] <- F
varBMO <- colnames(datos)[varBMO]
var3.5 <- startsWith(colnames(datos),"Rim3.5")
var3.5 <- colnames(datos)[var3.5]
var4.1 <- startsWith(colnames(datos),"Rim4.1")
var4.1 <- colnames(datos)[var4.1]
var4.7 <- startsWith(colnames(datos),"Rim4.7")
var4.7 <- colnames(datos)[var4.7]

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

# Exportar datos a excel
# library(xlsx)
# write.xlsx(datos, "datos-percentiles.xlsx", sheetName="Percentiles")

# Eliminación datos atípicos
# removeOutliers <- function(x) {
#   qnt <- quantile(x, probs=c(.25, .75, 0.05, 0.95), na.rm = T)
#   iqr <- qnt[2]-qnt[1]
#   fence1 <- qnt[1]-1.5*iqr
#   fence2 <- qnt[2]+1.5*iqr
#   x[x < fence1] <- qnt[3]
#   x[x > fence2] <- qnt[4]
#   return(x)
# }
# 
# datos[datos$Glaucoma=="Y", 11:ncol(datos)] <- sapply(datos[datos$Glaucoma=="Y",11:ncol(datos)], removeOutliers)
# datos[datos$Glaucoma=="N", 11:ncol(datos)] <- sapply(datos[datos$Glaucoma=="N",11:ncol(datos)], removeOutliers)

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
# Ojo izquierdo 
datos.L <- datos[datos$Eye=="L",]
# Ojo derecho
datos.R <- datos[datos$Eye=="R",]
# Datos Glaucoma
datos.Glaucoma = datos[datos$Glaucoma=="Y",]
# Datos Sanos
datos.Sanos = datos[datos$Glaucoma=="N",]

# Evolución del glaucoma con la edad
ggplot(datos.Glaucoma, aes(x = Age)) + geom_histogram(breaks=seq(20, 100, by = 10), fill=I("red"), colour=I("white"), position="identity", alpha=.5) + scale_x_continuous(breaks=seq(20,100,10))


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

# ANÁLISIS DESCRIPTIVO
# Funciones para dibujar distribuciones por grupos
# Histogramas según glaucoma y ojos
overHist <- function(i){
  p <- ggplot(datos, aes_string(x = i)) + geom_histogram(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + facet_grid(Eye~.)
  return(p)
}

# Densidad según glaucoma y ojos 
overDensity <- function(i){
  p <- ggplot(datos, aes_string(x = i)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + facet_grid(Eye~.)
  return(p)
}

# Diagrama de cajas según glaucoma y ojos
overBoxplot <- function(i){
  p <- ggplot(datos, aes_string(x="Eye", y = i)) + geom_boxplot(aes(fill=Glaucoma))
  return(p)
}

# FoBMO.Angle, Displacement y BMO.Area
# Ojo izquierdo
# Estadísticos descriptivos
result <- describeBy(datos.L[,c("FoBMO.Angle", "Displacement", "BMO.Area")], datos.L$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)
# datatable(result, rownames = T, escape=F, options = list( paging=F, autoWidth = T, dom="t", language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

# Gráfico descriptivo
ggpairs(datos.L[,c("Glaucoma", "FoBMO.Angle", "Displacement", "BMO.Area")], mapping=aes(color=Glaucoma, alpha=0.5), columns =  c("FoBMO.Angle", "Displacement", "BMO.Area"))

# Ojo derecho
# Estadísticos descriptivos
result <- describeBy(datos.R[,c("FoBMO.Angle", "Displacement", "BMO.Area")], datos.R$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)
# datatable(result, rownames = T, escape=F, options = list( paging=F, autoWidth = T, dom="t", language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

# Gráfico descriptivo
ggpairs(datos.R[,c("Glaucoma", "FoBMO.Angle", "Displacement", "BMO.Area")], mapping=aes(color=Glaucoma, alpha=0.5), columns =  c("FoBMO.Angle", "Displacement", "BMO.Area"))

# Histogramas
p <- lapply(c("FoBMO.Angle", "Displacement", "BMO.Area"), overHist)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Densidad
p <- lapply(c("FoBMO.Angle", "Displacement", "BMO.Area"), overDensity)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Diagramas de caja
p <- lapply(c("FoBMO.Angle", "Displacement", "BMO.Area"), overBoxplot)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))


# BMO ojo izquierdo
# Estadísticos descriptivos
result <- describeBy(datos.L[,varBMO], datos.L$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.L[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# BMO ojo derecho
# Estadísticos descriptivos
result <- describeBy(datos.R[,varBMO], datos.R$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
p <- lapply(colnames(datos[,varBMO])[-1], overHist)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Densidad
p <- lapply(colnames(datos[,varBMO])[-1], overDensity)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Diagrama de cajas
p <- lapply(colnames(datos[,varBMO])[-1], overBoxplot)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))


# Anillo 3.5 ojo izquierdo
result <- describeBy(datos.L[,var3.5], datos.L$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.L[,c("Glaucoma",var3.5)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  var3.5)

# Anillo 3.5 ojo derecho
result <- describeBy(datos.R[,var3.5], datos.R$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.L[,c("Glaucoma",var3.5)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  var3.5)

# Histograma
p <- lapply(colnames(datos[,var3.5]), overHist)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Densidad
p <- lapply(colnames(datos[,var3.5]), overDensity)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Diagrama de caja
p <- lapply(colnames(datos[,var3.5]), overBoxplot)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))


# Anillo 4.1 ojo izquierdo
result <- describeBy(datos.L[,var4.1], datos.L$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.L[,c("Glaucoma",var4.1)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  var4.1)

# Anillo 4.1 ojo derecho
result <- describeBy(datos.R[,var4.1], datos.R$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.L[,c("Glaucoma",var4.1)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  var4.1)

# Histogramas
p <- lapply(colnames(datos[,var4.1]), overHist)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Densidad
p <- lapply(colnames(datos[,var4.1]), overDensity)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Diagrama de caja
p <- lapply(colnames(datos[,var4.1]), overBoxplot)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))


# Anillo 4.7 ojo izquierdo
result <- describeBy(datos.L[,var4.7], datos.L$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.L[,c("Glaucoma",var4.7)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  var4.7)

# Anillo 4.7 ojo derecho
result <- describeBy(datos.R[,var4.7], datos.R$Glaucoma, mat = T, digits = 4)
result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
colnames(result)[1] <- "Glaucoma"
print(result)

# Gráficos descriptivos
ggpairs(datos.L[,c("Glaucoma",var4.7)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  var4.7)

# Histogramas
p <- lapply(colnames(datos[,var4.7]), overHist)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Densidad
p <- lapply(colnames(datos[,var4.7]), overDensity)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))

# Diagrama de cajas
p <- lapply(colnames(datos[,var4.7]), overBoxplot)
do.call(ggarrange, c(p, common.legend = TRUE, legend = "top"))


# COMPARACIÓN DE POBLACIONES
# Comparación de proporciones de hombres y mujeres
x <- factor(datos$Glaucoma, levels(datos$Glaucoma)[c(2,1)])
y <- datos$Gender
result <- unlist(prop.test(table(x,y))[c("estimate","conf.int", "p.value")])
result$difference <- result[["estimate.prop 1"]]-result[["estimate.prop 2"]]
result <- result[c(1,2,6,3,4,5)]
result <- data.frame(result)
colnames(result) <- c("Proporción.Mujeres", "Proporción.Hombres","Diferencia.proporciones", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result)
# datatable(format(result,5), rownames = T, escape=F, options = list( paging=F, autoWidth = T, dom="t", language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

# Comparación pareada de las proporciones de ojos con glaucoma
result <- unlist(mcnemar.test(table(datos$Glaucoma, datos$Eye))[c("statistic", "p.value")])
result <- data.frame(t(result))
colnames(result) <- c("Estadístico de McNemar", "p-valor")
print(result)

# Comparación de las edades medias
result <- data.frame(t(sapply(datos[c("Age")], function(x) unlist(t.test(x~datos$Glaucoma)[c("estimate","conf.int", "p.value")]))))
result$difference <- result$estimate.mean.in.group.N-result$estimate.mean.in.group.Y
result <- result[c(1,2,6,3,4,5)]
colnames(result) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result)

# Comparación de las edades medias de hombres y mujeres con glaucoma
result <- data.frame(t(unlist(t.test(Age~Gender, data=datos[datos$Glaucoma=="Y",])[c("estimate","conf.int", "p.value")])))
result$difference <- result$estimate.mean.in.group.F-result$estimate.mean.in.group.M
result <- result[c(1,2,6,3,4,5)]
colnames(result) <- c("Media.Hombres", "Media.Mujeres","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result)

# Comparación de medias del grosor de los sectores de los anillos de enfermos y sanos según ojos
# BMO.Area ojo izquierdo
result <- data.frame(t(unlist(t.test(BMO.Area~Glaucoma, data=datos.L)[c("estimate","conf.int", "p.value")])))
result$difference <- result$estimate.mean.in.group.N-result$estimate.mean.in.group.Y
result <- result[c(1,2,6,3,4,5)]
colnames(result) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result)

# BMO.Area ojo derecho
result <- data.frame(t(unlist(t.test(BMO.Area~Glaucoma, data=datos.R)[c("estimate","conf.int", "p.value")])))
result$difference <- result$estimate.mean.in.group.N-result$estimate.mean.in.group.Y
result <- result[c(1,2,6,3,4,5)]
colnames(result) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result)

# BMO ojo izquierdo
result.L <- data.frame(t(sapply(datos.L[,varBMO], function(x) unlist(t.test(x~datos.L$Glaucoma)[c("estimate","conf.int", "p.value")]))))
result.L$difference <- result.L$estimate.mean.in.group.N-result.L$estimate.mean.in.group.Y
result.L <- result.L[c(1,2,6,3,4,5)]
colnames(result.L) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
print(result.L)

# BMO ojo derecho
result.R <- data.frame(t(sapply(datos.R[,varBMO], function(x) unlist(t.test(x~datos.R$Glaucoma)[c("estimate","conf.int", "p.value")]))))
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
