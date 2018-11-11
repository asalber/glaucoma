# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Análisis del cubo macular de pacientes con glaucoma y sanos

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "reshape2", "GGally", "gridExtra", "ggpubr", "DT", "psych", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos (ver fichero glaucoma-preprocesamiento-datos.r)
datos <- read.csv(file="datos/datos-macula-preprocesados.csv", header=T, sep=",")

# Conjuntos de variables
# varCells <- startsWith(colnames(datos),"Cell")
# varCells <- colnames(datos)[varCells]
varCells <- NULL
for (i in 1:8){
  for (j in 8:1){
    varCells <- c(varCells, paste0("Celda",j,".",i))
  }
}
matrixCells <- matrix(varCells, nrow=8, ncol=8)
submatrixCells1 <- t(matrixCells[5:8,1:4])
submatrixCells2 <- t(matrixCells[5:8,5:8])
submatrixCells3 <- t(matrixCells[1:4,1:4])
submatrixCells4 <- t(matrixCells[1:4,5:8])
submatrixCells <- list(submatrixCells1, submatrixCells2, submatrixCells3, submatrixCells4)
varCells <- NULL
for (i in 1:8){
  for (j in 1:8){
    varCells <- c(varCells, paste0("Celda",j,".",i))
  }
}
# Celdas hemisferio inferior
cellsHI <- c(as.vector(submatrixCells1),as.vector(submatrixCells2))
# Celdas hemisferio superior
cellsHS <- c(as.vector(submatrixCells3),as.vector(submatrixCells4))

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
 
datos[datos$Glaucoma=="Y" & datos$Capa=="RPE", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="RPE", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="RPE", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="RPE", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Capa=="GCL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="GCL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="GCL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="GCL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Capa=="INL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="INL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="INL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="INL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Capa=="IPL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="IPL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="IPL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="IPL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Capa=="ONL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="ONL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="ONL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="ONL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Capa=="OPL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="OPL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="OPL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="OPL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Capa=="RNFL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="RNFL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="RNFL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="RNFL", varCells], removeOutliers)
datos[datos$Glaucoma=="Y" & datos$Capa=="FULL", varCells] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="FULL", varCells], removeOutliers)
datos[datos$Glaucoma=="N" & datos$Capa=="FULL", varCells ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="FULL", varCells], removeOutliers)

# Añadir capas externas OPL+ONL+RPE
datosOPLONLRPE <- cbind(datos[datos$Capa=="OPL", 1:4], datos[datos$Capa=="OPL", varCells] + datos[datos$Capa=="ONL", varCells] + datos[datos$Capa=="RPE", varCells])
datosOPLONLRPE$Capa <- "OPL+ONL+RPE" 
datos <- rbind(datos, datosOPLONLRPE)

# Añadir capas internas RNFL+GCL+IPL
datosRNFLGCLIPL <- cbind(datos[datos$Capa=="RNFL", 1:4], datos[datos$Capa=="RNFL", varCells] + datos[datos$Capa=="GCL", varCells] + datos[datos$Capa=="IPL", varCells])
datosRNFLGCLIPL$Capa <- "RNFL+GCL+IPL" 
datos <- rbind(datos, datosRNFLGCLIPL)

# Añadir capas internas RNFL+GCL+IPL+INL
datosRNFLGCLIPLINL <- cbind(datos[datos$Capa=="RNFL", 1:4], datos[datos$Capa=="RNFL", varCells] + datos[datos$Capa=="GCL", varCells] + datos[datos$Capa=="IPL", varCells])
datosRNFLGCLIPLINL$Capa <- "RNFL+GCL+IPL+INL" 
datos <- rbind(datos, datosRNFLGCLIPLINL)

# Datos hemisferio superior e inferior
datos$Celda.HI <- apply(datos[,cellsHI], 1, mean, na.rm=T)
datos$Celda.HS <- apply(datos[,cellsHS], 1, mean, na.rm=T)

# Selección de datos
# Ojo izquierdo 
datos.L <- datos[datos$Ojo=="L",]
# Ojo derecho
datos.R <- datos[datos$Ojo=="R",]
# Datos Glaucoma
datos.Glaucoma = datos[datos$Glaucoma=="Y",]
# Datos Sanos
datos.Sanos = datos[datos$Glaucoma=="N",]
# Datos hemisferios
datosH <- gather(datos[, c("Capa", "Glaucoma", "Ojo", "Celda.HI", "Celda.HS")], Hemisferio, Grosor, -Capa, -Glaucoma, -Ojo)



# ANÁLISIS DESCRIPTIVO
# Funciones para dibujar distribuciones por grupos
# Resumen estadístico
describir <- function(vars, ojo, capa){
  if (ojo=="L"){
    result <- describeBy(datos[datos$Ojo=="L" & datos$Capa==capa, varCells], datos[datos$Ojo=="L" & datos$Capa==capa, "Glaucoma"], mat = T, digits = 4)
  } else {
    result <- describeBy(datos[datos$Ojo=="R" & datos$Capa==capa, varCells], datos[datos$Ojo=="R" & datos$Capa==capa, "Glaucoma"], mat = T, digits = 4)
  }
  result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
  colnames(result)[1] <- "Glaucoma"
  return(result)
}
# Histogramas de las variables vars de la capa Capa según glaucoma y ojos
overHist <- function(vars, capa){
  p <- ggplot(datos[datos$Capa==capa,], aes_string(x = vars)) + geom_histogram(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + facet_grid(Ojo~.)
  return(p)
}

cellHist <- function(matrix, capa){
  for (i in 1:length(matrix)){
    p <- lapply(unlist(matrix[i]), overHist, capa=capa)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Densidad de las variables vars de la capa Capa según glaucoma y ojos
overDensity <- function(vars, capa){
  p <- ggplot(datos[datos$Capa==capa,], aes_string(x = vars)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + facet_grid(Ojo~.)
  return(p)
}

cellDensity <- function(matrix, capa){
  for (i in 1:length(matrix)){
    p <- lapply(unlist(matrix[i]), overDensity, capa=capa)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Diagrama de las variables vars de la capa Capa según glaucoma y ojos
overBoxplot <- function(vars, capa){
  p <- ggplot(datos[datos$Capa==capa,], aes_string(x="Ojo", y = vars)) + geom_boxplot(aes(fill=Glaucoma))
  return(p)
}

cellBoxplot <- function(matrix, capa){
  for (i in 1:length(matrix)){
    p <- lapply(unlist(matrix[i]), overBoxplot, capa=capa)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Matriz de correlación entre las celdas de una capa y un ojo
cellCorrelation <- function (ojo, capa){
  cormat <- round(cor(datos[datos$Ojo==ojo & datos$Capa==capa, varCells], use="complete.obs"),2)
  print(datatable(cormat, rownames = T, escape=F, options = list(pageLength = 8, autoWidth = T, language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))))
  melted.cormat <- melt(cormat)
  colnames(melted.cormat) <- c("Celda1", "Celda2", "R")
  melted.cormat$Celda1 <- substr(melted.cormat$Celda1, 6, 8)
  melted.cormat$Celda2 <- substr(melted.cormat$Celda2, 6, 8)
  print(ggplot(data = melted.cormat, aes(x=Celda1, y=Celda2, fill=R)) + 
          geom_tile() +
          theme(axis.text.x=element_text(size=8, angle = 90, vjust = 0.5)))
}

# Comparación de medias del grosor de las celdas del cubo macular de enfermos y sanos según ojos
comparar.medias <- function (vars, capa, ojo){
  datos.capa.ojo <- datos[datos$Ojo==ojo & datos$Capa==capa, ]
  result <- data.frame(t(sapply(datos.capa.ojo[, vars], function(x) unlist(t.test(x~datos.capa.ojo$Glaucoma)[c("estimate","conf.int", "p.value")]))))
  result$difference <- result$estimate.mean.in.group.N-result$estimate.mean.in.group.Y
  result<- result[c(1,2,6,3,4,5)]
  colnames(result) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
  return(result)
}

# Intervalos de confianza para la diferencia de medias del grosor de las celdas del cubo macular de enfermos y sanos
intervalos.comparacion.medias <- function (vars, capa){
  medias.L <- comparar.medias(vars, capa, "L")
  medias.L$Celdas <- substr(row.names(medias.L),6,8)
  medias.L$Celdas <- factor(medias.L$Celdas, levels = medias.L$Celdas)
  medias.L$Ojo <- "L"
  medias.R <- comparar.medias(vars, capa, "R")
  medias.R$Celdas <- substr(row.names(medias.R),6,8)
  medias.R$Celdas <- factor(medias.R$Celdas, levels = medias.R$Celdas)
  medias.R$Ojo <- "R"
  medias <- rbind(medias.L, medias.R)
  print(ggplot(medias, aes(x = Celdas, y = Diferencia.medias, colour=Ojo)) + 
          geom_errorbar(aes(ymax = lim.sup.int.conf, ymin = lim.inf.int.conf), position = position_dodge(width = 0.5), width=.2) +
          geom_point(size = 2, position = position_dodge(0.5)) + 
          ggtitle(paste("Intervalos de confianza del 95% de la diferencia de medias\n del grosor de la capa", capa, "del cubo macular")) +
          theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(size=8, angle = 90, vjust = 0.5))
  )
}

# Grafico comparación de medias del grosor de las celdas del cubo macular de enfermos y sanos
matriz.comparacion.medias <- function (capa){
  medias.L <- comparar.medias(varCells, capa, "L")
  medias.L$Ojo <- "L"
  medias.R <- comparar.medias(varCells, capa, "R")
  medias.R$Ojo <- "R"
  medias <- rbind(medias.L, medias.R) 
  medias <- mutate(medias, Ojo = factor(Ojo, levels=c("R", "L")),
                   fila = factor(rep(rep(seq(1,8),8),2), levels=as.character(1:8)), 
                   col = rep(unlist(lapply(seq(1,8), rep, 8)), 2), 
                   columna = factor(paste0(col, Ojo), levels = c(paste0(1:8, "R"), paste0(8:1,"L"))),
                   sig = ifelse(medias$`p-valor`<0.01,"<0.01", ifelse(medias$`p-valor`<0.05, "<0.05", ">0.05"))
  )
  print(ggplot(data = medias, aes(x=columna, y=fila, fill=Diferencia.medias)) + 
          geom_tile() + 
          scale_fill_continuous(low = "indianred1", high = "indianred4") +
          geom_text(aes(label=round(Diferencia.medias,2), colour=sig), size=3) + 
          scale_colour_manual("p valor", values=c("green", "yellow","black")) + 
          scale_x_discrete(label = function(str){substring(str, 1, 1)}) +
          facet_grid(.~Ojo, scales = "free_x") +
          ggtitle(paste("Diferencia de medias del grosor de la capa", capa, "del cubo macular")) +
          theme(plot.title = element_text(hjust = 0.5))
  )
}


# CAPA RNFL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "RNFL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

# Ojo derecho
# Estadísticos descriptivos
print(describir(varCells, "R", "RNFL"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatrixCells, "RNFL")

# Densidad
cellDensity(submatrixCells, "RNFL")

# Diagrama de cajas
cellBoxplot(submatrixCells, "RNFL")

# Comparación de medias
medias.L <- comparar.medias(varCells, "RNFL", "L")
medias.R <- comparar.medias(varCells, "RNFL", "R")

# Intervalo de comparación de medias
svg(filename="img/intervalos confianza comparacion medias capa RNFL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(varCells, varCells, "RNFL")
dev.off()

# Matriz de comparación de medias
svg(filename="img/diferencia medias cubo macular capa RNFL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RNFL")
dev.off()

# Diagrama de cajas hemisferios
datosH %>% filter(Capa=="RNFL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RNFL")


# CAPA GCL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "GCL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

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

# Comparación de medias
medias.L <- comparar.medias(varCells, "GCL", "L")
medias.R <- comparar.medias(varCells, "GCL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa GCL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(varCells, "GCL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa GCL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("GCL")
dev.off()

# Diagrama de cajas hemisferios
datosH %>% filter(Capa=="GCL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "GCL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "GCL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "GCL")


# CAPA INL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "INL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

# Ojo derecho
# Estadísticos descriptivos
print(describir(varCells, "R", "INL"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatrixCells, "INL")

# Densidad
cellDensity(submatrixCells, "INL")

# Diagrama de cajas
cellBoxplot(submatrixCells, "INL")

# Comparación de medias
medias.L <- comparar.medias(varCells, "INL", "L")
medias.R <- comparar.medias(varCells, "INL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa INL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(varCells, "INL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa INL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("INL")
dev.off()

# Diagrama de cajas hemisferios
datosH %>% filter(Capa=="INL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "INL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "INL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "INL")


# CAPA RPE
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "RPE"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

# Ojo derecho
# Estadísticos descriptivos
print(describir(varCells, "R", "RPE"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatrixCells, "RPE")

# Densidad
cellDensity(submatrixCells, "RPE")

# Diagrama de cajas
cellBoxplot(submatrixCells, "RPE")

# Comparación de medias
medias.L <- comparar.medias(varCells, "RPE", "L")
medias.R <- comparar.medias(varCells, "RPE", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa RPE.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(varCells, "RPE")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa RPE.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RPE")
dev.off()

# Diagrama de cajas hemisferios
datosH %>% filter(Capa=="RPE") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RPE", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RPE", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RPE")


# CAPA RNFL + GCL + IPL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "RNFL+GCL+IPL"))

# Correlación entre las celdas
cellCorrelation("L", "RNFL+GCL+IPL")

# Ojo derecho
# Estadísticos descriptivos
print(describir(varCells, "R", "RNFL+GCL+IPL"))

# Correlación entre las celdas
cellCorrelation("R", "RNFL+GCL+IPL")

# Histogramas
cellHist(submatrixCells, "RNFL+GCL+IPL")

# Densidad
cellDensity(submatrixCells, "RNFL+GCL+IPL")

# Diagrama de cajas
cellBoxplot(submatrixCells, "RNFL+GCL+IPL")

# Comparación de medias
medias.L <- comparar.medias(varCells, "RNFL+GCL+IPL", "L")
medias.R <- comparar.medias(varCells, "RNFL+GCL+IPL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(varCells, "RNFL+GCL+IPL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RNFL+GCL+IPL")
dev.off()

# Diagrama de cajas hemisferios
datosH %>% filter(Capa=="RNFL+GCL+IPL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL")


# CAPA RNFL + GCL + IPL + INL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "RNFL+GCL+IPL+INL"))

# Correlación entre las celdas
cellCorrelation("L", "RNFL+GCL+IPL+INL")

# Ojo derecho
# Estadísticos descriptivos
print(describir(varCells, "R", "RNFL+GCL+IPL+INL"))

# Correlación entre las celdas
cellCorrelation("R", "RNFL+GCL+IPL+INL")

# Histogramas
cellHist(submatrixCells, "RNFL+GCL+IPL+INL")

# Densidad
cellDensity(submatrixCells, "RNFL+GCL+IPL+INL")

# Diagrama de cajas
cellBoxplot(submatrixCells, "RNFL+GCL+IPL+INL")

# Comparación de medias
medias.L <- comparar.medias(varCells, "RNFL+GCL+IPL+INL", "L")
medias.R <- comparar.medias(varCells, "RNFL+GCL+IPL+INL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(varCells, "RNFL+GCL+IPL+INL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RNFL+GCL+IPL+INL")
dev.off()

# Diagrama de cajas hemisferios
datosH %>% filter(Capa=="RNFL+GCL+IPL+INL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL+INL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL+INL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL+INL")


# CAPA FULL
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

# Comparación de medias
medias.L <- comparar.medias(varCells, "FULL", "L")
medias.R <- comparar.medias(varCells, "FULL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa FULL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(varCells, "FULL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa FULL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("FULL")
dev.off()

# Diagrama de cajas hemisferios
datosH %>% filter(Capa=="FULL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "FULL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "FULL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "FULL")


