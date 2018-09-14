# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Análisis del cubo macular de pacientes con glaucoma y sanos

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "reshape2", "GGally", "gridExtra", "ggpubr", "DT", "psych", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos
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
datos <- rbind(datos.RNFL, datos.GCL, datos.IPL, datos.INL, datos.OPL, datos.ONL, datos.RPE, datos.FULL)
# Normalización de nombres de variables
colnames(datos) <- make.names(colnames(datos))
colnames(datos)[16:79] <- paste0("Cell.",substr(colnames(datos)[16:79],6,8))
# Convertir en factores el glaucoma y el ojo y la capa
datos$Glaucoma <- factor(datos$Glaucoma, levels=c("Y","N"))
datos$Eye <- factor(datos$Eye, levels = c("R", "L"))
datos$Layer <- factor(datos$Layer, levels = c("RNFL", "GCL", "IPL", "INL", "OPL", "ONL", "RPE", "FULL"))

# Eliminación datos erroneos
datos.atipicos <- read.csv(file="datos/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- datos.atipicos[is.na(datos.atipicos$SUMA.V) | datos.atipicos$SUMA.V>10, "Lastname"]
datos <- datos[!(datos$Lastname %in% id.atipicos), ]

# Conjuntos de variables
# varCells <- startsWith(colnames(datos),"Cell")
# varCells <- colnames(datos)[varCells]
varCells <- NULL
for (i in 1:8){
  for (j in 8:1){
    varCells <- c(varCells, paste0("Cell.",j,".",i))
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
    varCells <- c(varCells, paste0("Cell.",j,".",i))
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

# Añadir capas externas OPL+ONL+RPE
datosOPLONLRPE <- cbind(datos[datos$Layer=="OPL", 1:15], datos[datos$Layer=="OPL", varCells] + datos[datos$Layer=="ONL", varCells] + datos[datos$Layer=="RPE", varCells])
datosOPLONLRPE$Layer <- "OPL+ONL+RPE" 
datos <- rbind(datos, datosOPLONLRPE)

# Añadir capas internas RNFL+GCL+IPL
datosRNFLGCLIPL <- cbind(datos[datos$Layer=="RNFL", 1:15], datos[datos$Layer=="RNFL", varCells] + datos[datos$Layer=="GCL", varCells] + datos[datos$Layer=="IPL", varCells])
datosRNFLGCLIPL$Layer <- "RNFL+GCL+IPL" 
datos <- rbind(datos, datosRNFLGCLIPL)

# Añadir capas internas RNFL+GCL+IPL+INL
datosRNFLGCLIPLINL <- cbind(datos[datos$Layer=="RNFL", 1:15], datos[datos$Layer=="RNFL", varCells] + datos[datos$Layer=="GCL", varCells] + datos[datos$Layer=="IPL", varCells])
datosRNFLGCLIPLINL$Layer <- "RNFL+GCL+IPL+INL" 
datos <- rbind(datos, datosRNFLGCLIPLINL)

# Datos hemisferio superior e inferior
datos$Cells.HI <- apply(datos[,cellsHI],1, mean, na.rm=T)
datos$Cells.HS <- apply(datos[,cellsHS],1, mean, na.rm=T)

# Selección de datos
# Ojo izquierdo 
datos.L <- datos[datos$Eye=="L",]
# Ojo derecho
datos.R <- datos[datos$Eye=="R",]
# Datos Glaucoma
datos.Glaucoma = datos[datos$Glaucoma=="Y",]
# Datos Sanos
datos.Sanos = datos[datos$Glaucoma=="N",]
# Datos hemisferios
datosH <- gather(datos[, c("Layer", "Glaucoma", "Eye", "Cells.HI", "Cells.HS")], Hemisferio, Grosor, -Layer, -Glaucoma, -Eye)



# ANÁLISIS DESCRIPTIVO
# Funciones para dibujar distribuciones por grupos
# Resumen estadístico
describir <- function(vars, eye, layer){
  result <- describeBy(datos[datos$Eye==eye & datos$Layer==layer, vars], datos[datos$Eye==eye & datos$Layer==layer, "Glaucoma"], mat = T, digits = 4)
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

# Correlación celdas del cubo macular por ojos y capas
cellCorrelation <- function (eye, layer){
  cormat <- round(cor(datos[datos$Eye==eye & datos$Layer==layer, varCells], use="complete.obs"),2)
  datatable(cormat, rownames = T, escape=F, options = list(pageLength = 8, autoWidth = T, language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
  melted.cormat <- melt(cormat)
  colnames(melted.cormat) <- c("Cell1", "Cell2", "Valor")
  melted.cormat$Cell1 <- substr(melted.cormat$Cell1, 6, 8)
  melted.cormat$Cell2 <- substr(melted.cormat$Cell2, 6, 8)
  print(ggplot(data = melted.cormat, aes(x=Cell1, y=Cell2, fill=Valor)) + geom_tile())
}

# Comparación de medias del grosor de las celdas del cubo macular de enfermos y sanos según ojos
comparar.medias <- function (vars, layer, eye){
  datos.layer.eye <- datos[datos$Eye==eye & datos$Layer==layer, ]
  result <- data.frame(t(sapply(datos.layer.eye[, vars], function(x) unlist(t.test(x~datos.layer.eye$Glaucoma)[c("estimate","conf.int", "p.value")]))))
  result$difference <- result$estimate.mean.in.group.N-result$estimate.mean.in.group.Y
  result<- result[c(1,2,6,3,4,5)]
  colnames(result) <- c("Media.Sanos", "Media.Glaucoma","Diferencia.medias", "lim.inf.int.conf", "lim.sup.int.conf", "p-valor")
  return(result)
}

# Intervalos de confianza para la diferencia de medias del grosor de las celdas del cubo macular de enfermos y sanos
intervalos.comparacion.medias <- function (vars, layer){
  medias.L <- comparar.medias(vars, layer, "L")
  medias.L$Cell <- substr(row.names(medias.L),6,8)
  medias.L$Cell <- factor(medias.L$Cell, levels = medias.L$Cell)
  medias.L$Eye <- "L"
  medias.R <- comparar.medias(vars, layer, "R")
  medias.R$Cell <- substr(row.names(medias.R),6,8)
  medias.R$Cell <- factor(medias.R$Cell, levels = medias.R$Cell)
  medias.R$Eye <- "R"
  medias <- rbind(medias.L, medias.R)
  print(ggplot(medias, aes(x = Cell, y = Diferencia.medias, colour=Eye)) + 
          geom_errorbar(aes(ymax = lim.sup.int.conf, ymin = lim.inf.int.conf), position = position_dodge(width = 0.5), width=.2) +
          geom_point(size = 2, position = position_dodge(0.5)) + 
          ggtitle(paste("Intervalos de confianza del 95% de la diferencia de medias\n del grosor de la capa", layer, "del cubo macular")) +
          theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(size=8, angle = 90, vjust = 0.5))
  )
}

# Grafico comparación de medias del grosor de las celdas del cubo macular de enfermos y sanos
matriz.comparacion.medias <- function (layer){
  medias.L <- comparar.medias(varCells, layer, "L")
  medias.L$Eye <- "L"
  medias.R <- comparar.medias(varCells, layer, "R")
  medias.R$Eye <- "R"
  medias <- rbind(medias.L, medias.R) 
  medias <- mutate(medias, Eye = factor(Eye, levels=c("R", "L")),
                   fila = factor(rep(rep(seq(1,8),8),2), levels=as.character(1:8)), 
                   col = rep(unlist(lapply(seq(1,8), rep, 8)), 2), 
                   columna = factor(paste0(col, Eye), levels = c(paste0(1:8, "R"), paste0(8:1,"L"))),
                   sig = ifelse(medias$`p-valor`<0.01,"<0.01", ifelse(medias$`p-valor`<0.05, "<0.05", ">0.05"))
  )
  print(ggplot(data = medias, aes(x=columna, y=fila, fill=Diferencia.medias)) + 
          geom_tile() + 
          scale_fill_continuous(low = "indianred1", high = "indianred4") +
          geom_text(aes(label=round(Diferencia.medias,2), colour=sig), size=3) + 
          scale_colour_manual("p valor", values=c("green", "yellow","black")) + 
          scale_x_discrete(label = function(str){substring(str, 1, 1)}) +
          facet_grid(.~Eye, scales = "free_x") +
          ggtitle(paste("Diferencia de medias del grosor de la capa", layer, "del cubo macular")) +
          theme(plot.title = element_text(hjust = 0.5)) 
  )
}


# CAPA RNFL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "RNFL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Layer=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

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
datosH %>% filter(Layer=="RNFL") %>% ggplot(aes(x=Eye, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RNFL")


# CAPA GCL
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
datosH %>% filter(Layer=="GCL") %>% ggplot(aes(x=Eye, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "GCL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "GCL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "GCL")


# CAPA INL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "INL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Layer=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

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
datosH %>% filter(Layer=="INL") %>% ggplot(aes(x=Eye, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "INL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "INL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "INL")


# CAPA RPE
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(varCells, "L", "RPE"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Layer=="FULL",c("Glaucoma",varCells)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varCells)

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
datosH %>% filter(Layer=="RPE") %>% ggplot(aes(x=Eye, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

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
datosH %>% filter(Layer=="RNFL+GCL+IPL") %>% ggplot(aes(x=Eye, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

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
datosH %>% filter(Layer=="RNFL+GCL+IPL+INL") %>% ggplot(aes(x=Eye, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

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
datosH %>% filter(Layer=="FULL") %>% ggplot(aes(x=Eye, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "FULL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "FULL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "FULL")


