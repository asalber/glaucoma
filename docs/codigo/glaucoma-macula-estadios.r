# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Análisis del cubo macular según estadio de glaucoma

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "reshape2", "GGally", "gridExtra", "ggpubr", "DT", "psych", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos (ver fichero glaucoma-preprocesamiento-datos.r)
datos.macula <- read.csv(file="datos/datos-macula-preprocesados.csv", header=T, sep=",")
datos.anillos <- read.csv(file="datos/datos-anillos-preprocesados.csv", header=T, sep=",")

varRims <- startsWith(colnames(datos.anillos), "Anillo")
varRims <- colnames(datos.anillos[varRims])[-1]
varG <- endsWith(colnames(datos.anillos), ".G")
varG <- colnames(datos.anillos[varG])

# Selección de variables
datos.anillos %<>% dplyr::select(c("Id", "Ojo", "Glaucoma", varRims))
# Casos completos
datos.anillos <- datos.anillos[complete.cases(datos.anillos), ]
# Ojo izquierdo 
datos.anillos.L <- filter(datos.anillos, Ojo=="L")
# Ojo derecho
datos.anillos.R <- filter(datos.anillos, Ojo=="R")

# Creación de los clusters de los estadios mediante k medias utilizando los sectores globales de todos los anillos (ver fichero glaucoma-clusters.r)
seed = 123
labels=c("I", "II", "III", "IV")
n = length(labels)
# Ojo izquierdo
clusters <- kmeans(datos.anillos.L[datos.anillos.L$Glaucoma=="Y", varG], centers = n, nstart = 25)
centers <- clusters$centers[order(clusters$centers[,1], decreasing = T),]
clusters <- kmeans(datos.anillos.L[datos.anillos.L$Glaucoma=="Y", varG], centers = centers)
# Convertir el cluster en un factor
clusters$cluster <- as.factor(clusters$cluster)
# Asignar etiquetas a los niveles del factor
levels(clusters$cluster) <- labels
# Añadir el cluster al conjunto de datos
datos.anillos.L$Estadio<- factor("Sano", levels=c("Sano", labels))
datos.anillos.L[datos.anillos.L$Glaucoma=="Y", "Estadio"] <- clusters$cluster

# Ojo derecho
clusters <- kmeans(datos.anillos.R[datos.anillos.R$Glaucoma=="Y", varG], centers = n, nstart = 25)
centers <- clusters$centers[order(clusters$centers[,1], decreasing = T),]
clusters <- kmeans(datos.anillos.R[datos.anillos.R$Glaucoma=="Y", varG], centers = centers)
# Convertir el cluster en un factor
clusters$cluster <- as.factor(clusters$cluster)
# Asignar etiquetas a los niveles del factor
levels(clusters$cluster) <- labels
# Añadir el cluster al conjunto de datos
datos.anillos.R$Estadio<- factor("Sano", levels=c("Sano", labels))
datos.anillos.R[datos.anillos.R$Glaucoma=="Y", "Estadio"] <- clusters$cluster

datos.anillos[datos.anillos$Ojo=="L","Estadio"] <- datos.anillos.L$Estadio
datos.anillos[datos.anillos$Ojo=="R","Estadio"] <- datos.anillos.R$Estadio
 
# Añadir estadios al conjunto de datos de la mácula
datos <- left_join(datos.macula, datos.anillos[, c("Id", "Estadio")], by="Id") %>%
  # Mover la columna del Estadio al principio
  dplyr::select(Id, Capa, Estadio, everything())
  
# Conjuntos de variables
# celdas <- startsWith(colnames(datos),"Cell")
# celdas <- colnames(datos)[celdas]
celdas <- NULL
for (i in 1:8){
  for (j in 8:1){
    celdas <- c(celdas, paste0("Celda",j,".",i))
  }
}
matriz.celdas <- matrix(celdas, nrow=8, ncol=8)
submatriz.celdas1 <- t(matriz.celdas[5:8,1:4])
submatriz.celdas2 <- t(matriz.celdas[5:8,5:8])
submatriz.celdas3 <- t(matriz.celdas[1:4,1:4])
submatriz.celdas4 <- t(matriz.celdas[1:4,5:8])
submatriz.celdas <- list(submatriz.celdas1, submatriz.celdas2, submatriz.celdas3, submatriz.celdas4)
celdas <- NULL
for (i in 1:8){
  for (j in 1:8){
    celdas <- c(celdas, paste0("Celda",j,".",i))
  }
}
# Celdas hemisferio inferior
celdas.HI <- c(as.vector(submatriz.celdas1),as.vector(submatriz.celdas2))
# Celdas hemisferio superior
celdas.HS <- c(as.vector(submatriz.celdas3),as.vector(submatriz.celdas4))

# Sustitución datos atípicos
eliminar.datos.atipicos <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75, 0.05, 0.95), na.rm = T)
  iqr <- qnt[2]-qnt[1]
  fence1 <- qnt[1]-1.5*iqr
  fence2 <- qnt[2]+1.5*iqr
  x[x < fence1] <- qnt[3]
  x[x > fence2] <- qnt[4]
  return(x)
}
 
datos[datos$Glaucoma=="Y" & datos$Capa=="RPE", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="RPE", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="RPE", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="RPE", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="Y" & datos$Capa=="GCL", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="GCL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="GCL", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="GCL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="Y" & datos$Capa=="INL", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="INL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="INL", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="INL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="Y" & datos$Capa=="IPL", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="IPL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="IPL", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="IPL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="Y" & datos$Capa=="ONL", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="ONL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="ONL", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="ONL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="Y" & datos$Capa=="OPL", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="OPL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="OPL", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="OPL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="Y" & datos$Capa=="RNFL", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="RNFL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="RNFL", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="RNFL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="Y" & datos$Capa=="FULL", celdas] <- sapply(datos[datos$Glaucoma=="Y" & datos$Capa=="FULL", celdas], eliminar.datos.atipicos)
datos[datos$Glaucoma=="N" & datos$Capa=="FULL", celdas ] <- sapply(datos[datos$Glaucoma=="N" & datos$Capa=="FULL", celdas], eliminar.datos.atipicos)

# Añadir capas externas OPL+ONL+RPE
datos.OPLONLRPE <- cbind(datos[datos$Capa=="OPL", 1:5], datos[datos$Capa=="OPL", celdas] + datos[datos$Capa=="ONL", celdas] + datos[datos$Capa=="RPE", celdas])
datos.OPLONLRPE$Capa <- "OPL+ONL+RPE" 
datos <- rbind(datos, datos.OPLONLRPE)

# Añadir capas internas RNFL+GCL+IPL
datos.RNFLGCLIPL <- cbind(datos[datos$Capa=="RNFL", 1:5], datos[datos$Capa=="RNFL", celdas] + datos[datos$Capa=="GCL", celdas] + datos[datos$Capa=="IPL", celdas])
datos.RNFLGCLIPL$Capa <- "RNFL+GCL+IPL" 
datos <- rbind(datos, datos.RNFLGCLIPL)

# Añadir capas internas RNFL+GCL+IPL+INL
datos.RNFLGCLIPLINL <- cbind(datos[datos$Capa=="RNFL", 1:5], datos[datos$Capa=="RNFL", celdas] + datos[datos$Capa=="GCL", celdas] + datos[datos$Capa=="IPL", celdas])
datos.RNFLGCLIPLINL$Capa <- "RNFL+GCL+IPL+INL" 
datos <- rbind(datos, datos.RNFLGCLIPLINL)

# Datos hemisferio superior e inferior
datos$Celda.HI <- apply(datos[,celdas.HI], 1, mean, na.rm=T)
datos$Celda.HS <- apply(datos[,celdas.HS], 1, mean, na.rm=T)

# Selección de datos
# Casos completos
datos %<>% na.omit
# Ojo izquierdo 
datos.L <- datos[datos$Ojo=="L",]
# Ojo derecho
datos.R <- datos[datos$Ojo=="R",]
# Datos Glaucoma
datos.Glaucoma = datos[datos$Glaucoma=="Y",]
# Datos Sanos
datos.Sanos = datos[datos$Glaucoma=="N",]
# Datos hemisferios
datos.Hemisferios <- gather(datos[, c("Capa", "Estadio", "Glaucoma", "Ojo", "Celda.HI", "Celda.HS")], Hemisferio, Grosor, -Capa, -Estadio, -Glaucoma, -Ojo)



# ANÁLISIS DESCRIPTIVO
# Funciones para dibujar distribuciones por grupos
# Resumen estadístico
describir <- function(vars, ojo, capa){
  if (ojo=="L"){
    result <- describeBy(datos[datos$Ojo=="L" & datos$Capa==capa, celdas], datos[datos$Ojo=="L" & datos$Capa==capa, "Glaucoma"], mat = T, digits = 4)
  } else {
    result <- describeBy(datos[datos$Ojo=="R" & datos$Capa==capa, celdas], datos[datos$Ojo=="R" & datos$Capa==capa, "Glaucoma"], mat = T, digits = 4)
  }
  result[,c("item","vars","trimmed", "mad","range","se")] <- NULL
  colnames(result)[1] <- "Glaucoma"
  return(result)
}
# Histogramas de las variables vars de la capa Capa según Estadío y Ojos
histograma.celdas.capa <- function(celdas, capa){
  p <- ggplot(datos[datos$Capa==capa,], aes_string(x = celdas)) + geom_histogram(aes(fill=Estadio), colour=I("white"), position="identity", alpha=.5) + facet_grid(Ojo~.)
  return(p)
}

matriz.histogramas.capa <- function(matriz, capa){
  for (i in 1:length(matriz)){
    p <- lapply(unlist(matriz[i]), histograma.celdas.capa, capa=capa)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Densidad de las variables vars de la capa Capa según glaucoma y ojos
densidad.celdas.capa <- function(celdas, capa){
  p <- ggplot(datos[datos$Capa==capa,], aes_string(x = celdas)) + geom_density(aes(fill=Estadio), colour=I("white"), position="identity", alpha=.5) + facet_grid(Ojo~.)
  return(p)
}

matriz.densidad.capa <- function(matriz, capa){
  for (i in 1:length(matriz)){
    p <- lapply(unlist(matriz[i]), densidad.celdas.capa, capa=capa)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Diagrama de las variables vars de la capa Capa según glaucoma y ojos
cajas.celdas.capa <- function(celdas, capa){
  p <- ggplot(datos[datos$Capa==capa,], aes_string(x="Ojo", y = celdas)) + geom_boxplot(aes(fill=Estadio))
  return(p)
}

matriz.cajas.capa <- function(matriz, capa){
  for (i in 1:length(matriz)){
    p <- lapply(unlist(matriz[i]), cajas.celdas.capa, capa=capa)
    print(do.call(ggarrange, c(p, common.legend = TRUE, legend = "top")))
  }
}

# Matriz de correlación entre las celdas de una capa y un ojo
cellCorrelation <- function (ojo, capa){
  cormat <- round(cor(datos[datos$Ojo==ojo & datos$Capa==capa, celdas], use="complete.obs"),2)
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
  medias.L <- comparar.medias(celdas, capa, "L")
  medias.L$Ojo <- "L"
  medias.R <- comparar.medias(celdas, capa, "R")
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
print(describir(celdas, "L", "RNFL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",celdas)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  celdas)

# Ojo derecho
# Estadísticos descriptivos
print(describir(celdas, "R", "RNFL"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatriz.celdas, "RNFL")

# Densidad
cellDensity(submatriz.celdas, "RNFL")

# Diagrama de cajas
cellBoxplot(submatriz.celdas, "RNFL")

# Comparación de medias
medias.L <- comparar.medias(celdas, "RNFL", "L")
medias.R <- comparar.medias(celdas, "RNFL", "R")

# Intervalo de comparación de medias
svg(filename="img/intervalos confianza comparacion medias capa RNFL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(celdas, celdas, "RNFL")
dev.off()

# Matriz de comparación de medias
svg(filename="img/diferencia medias cubo macular capa RNFL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RNFL")
dev.off()

# Diagrama de cajas hemisferios
datos.Hemisferios %>% filter(Capa=="RNFL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RNFL")


# CAPA GCL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(celdas, "L", "GCL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",celdas)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  celdas)

# Ojo derecho
# Estadísticos descriptivos
print(describir(celdas, "R", "GCL"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatriz.celdas, "GCL")

# Densidad
cellDensity(submatriz.celdas, "GCL")

# Diagrama de cajas
cellBoxplot(submatriz.celdas, "GCL")

# Comparación de medias
medias.L <- comparar.medias(celdas, "GCL", "L")
medias.R <- comparar.medias(celdas, "GCL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa GCL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(celdas, "GCL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa GCL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("GCL")
dev.off()

# Diagrama de cajas hemisferios
datos.Hemisferios %>% filter(Capa=="GCL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "GCL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "GCL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "GCL")


# CAPA INL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(celdas, "L", "INL"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",celdas)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  celdas)

# Ojo derecho
# Estadísticos descriptivos
print(describir(celdas, "R", "INL"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatriz.celdas, "INL")

# Densidad
cellDensity(submatriz.celdas, "INL")

# Diagrama de cajas
cellBoxplot(submatriz.celdas, "INL")

# Comparación de medias
medias.L <- comparar.medias(celdas, "INL", "L")
medias.R <- comparar.medias(celdas, "INL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa INL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(celdas, "INL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa INL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("INL")
dev.off()

# Diagrama de cajas hemisferios
datos.Hemisferios %>% filter(Capa=="INL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "INL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "INL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "INL")


# CAPA RPE
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(celdas, "L", "RPE"))

# Gráficos descriptivos
# ggpairs(datos.L[datos.L$Capa=="FULL",c("Glaucoma",celdas)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  celdas)

# Ojo derecho
# Estadísticos descriptivos
print(describir(celdas, "R", "RPE"))

# Gráficos descriptivos
# ggpairs(datos.R[,c("Glaucoma",varBMO)], mapping=aes(color=Glaucoma, alpha=0.5), columns =  varBMO)

# Histogramas
cellHist(submatriz.celdas, "RPE")

# Densidad
cellDensity(submatriz.celdas, "RPE")

# Diagrama de cajas
cellBoxplot(submatriz.celdas, "RPE")

# Comparación de medias
medias.L <- comparar.medias(celdas, "RPE", "L")
medias.R <- comparar.medias(celdas, "RPE", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa RPE.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(celdas, "RPE")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa RPE.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RPE")
dev.off()

# Diagrama de cajas hemisferios
datos.Hemisferios %>% filter(Capa=="RPE") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RPE", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RPE", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RPE")


# CAPA RNFL + GCL + IPL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(celdas, "L", "RNFL+GCL+IPL"))

# Correlación entre las celdas
cellCorrelation("L", "RNFL+GCL+IPL")

# Ojo derecho
# Estadísticos descriptivos
print(describir(celdas, "R", "RNFL+GCL+IPL"))

# Correlación entre las celdas
cellCorrelation("R", "RNFL+GCL+IPL")

# Histogramas
cellHist(submatriz.celdas, "RNFL+GCL+IPL")

# Densidad
cellDensity(submatriz.celdas, "RNFL+GCL+IPL")

# Diagrama de cajas
cellBoxplot(submatriz.celdas, "RNFL+GCL+IPL")

# Comparación de medias
medias.L <- comparar.medias(celdas, "RNFL+GCL+IPL", "L")
medias.R <- comparar.medias(celdas, "RNFL+GCL+IPL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(celdas, "RNFL+GCL+IPL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RNFL+GCL+IPL")
dev.off()

# Diagrama de cajas hemisferios
datos.Hemisferios %>% filter(Capa=="RNFL+GCL+IPL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL")


# CAPA RNFL + GCL + IPL + INL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(celdas, "L", "RNFL+GCL+IPL+INL"))

# Correlación entre las celdas
cellCorrelation("L", "RNFL+GCL+IPL+INL")

# Ojo derecho
# Estadísticos descriptivos
print(describir(celdas, "R", "RNFL+GCL+IPL+INL"))

# Correlación entre las celdas
cellCorrelation("R", "RNFL+GCL+IPL+INL")

# Histogramas
cellHist(submatriz.celdas, "RNFL+GCL+IPL+INL")

# Densidad
cellDensity(submatriz.celdas, "RNFL+GCL+IPL+INL")

# Diagrama de cajas
cellBoxplot(submatriz.celdas, "RNFL+GCL+IPL+INL")

# Comparación de medias
medias.L <- comparar.medias(celdas, "RNFL+GCL+IPL+INL", "L")
medias.R <- comparar.medias(celdas, "RNFL+GCL+IPL+INL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(celdas, "RNFL+GCL+IPL+INL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa RNFL+GCL+IPL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("RNFL+GCL+IPL+INL")
dev.off()

# Diagrama de cajas hemisferios
datos.Hemisferios %>% filter(Capa=="RNFL+GCL+IPL+INL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL+INL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL+INL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "RNFL+GCL+IPL+INL")


# CAPA FULL
# Ojo izquierdo
# Estadísticos descriptivos
print(describir(celdas, "L", "FULL"))

# Correlación entre las celdas
cellCorrelation("L", "FULL")

# Ojo derecho
# Estadísticos descriptivos
print(describir(celdas, "R", "FULL"))

# Correlación entre las celdas
cellCorrelation("R", "FULL")

# Histogramas
cellHist(submatriz.celdas, "FULL")

# Densidad
cellDensity(submatriz.celdas, "FULL")

# Diagrama de cajas
cellBoxplot(submatriz.celdas, "FULL")

# Comparación de medias
medias.L <- comparar.medias(celdas, "FULL", "L")
medias.R <- comparar.medias(celdas, "FULL", "R")

# Intervalo comparación medias
svg(filename="img/intervalos confianza comparacion medias capa FULL.svg", 
    width=10, height=8, pointsize=12)
intervalos.comparacion.medias(celdas, "FULL")
dev.off()

# Matriz comparación medias
svg(filename="img/diferencia medias cubo macular capa FULL.svg", 
    width=10, height=8, pointsize=12)
matriz.comparacion.medias("FULL")
dev.off()

# Diagrama de cajas hemisferios
datos.Hemisferios %>% filter(Capa=="FULL") %>% ggplot(aes(x=Ojo, y=Grosor, fill=Glaucoma)) + geom_boxplot() + facet_grid(.~Hemisferio)

# Comparación de medias hemisferios
medias.L <- comparar.medias(c("Cells.HI","Cells.HS"), "FULL", "L")
medias.R <- comparar.medias(c("Cells.HI","Cells.HS"), "FULL", "R")
intervalos.comparacion.medias(c("Cells.HI","Cells.HS"), "FULL")


