# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Clusters de pacientes con  glaucoma

# Carga de paquetes
.packages <- c("readxl", "tidyverse", "cluster", "FactoMineR", "factoextra", "corrplot", "ggpubr", "RColorBrewer", "flipMultivariates", "caret", "MASS", "pander", "reshape")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Carga de la base de datos
datos <- read.csv(file="datos/datos.csv", header=T, sep=",")

# Eliminación de datos erróneos
datos <- datos[-c(127, 928, 1669, 1677, 1678), ]

# Eliminación datos atípicos
datos.atipicos <- read.csv(file="datos/datos-confrontacion.csv", header=T, sep=",")
id.atipicos <- datos.atipicos[is.na(datos.atipicos$SUMA.V) | datos.atipicos$SUMA.V>10, "Lastname"]
datos <- datos[!(datos$Patient.Id %in% id.atipicos), ]

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

# Renombrar variables
names(datos) <- gsub("Rim", "Anillo", names(datos))
names(datos) <- gsub("BMO", "AnilloBMO", names(datos))

# Conjuntos de variables
varBMO <- startsWith(colnames(datos),"AnilloBMO")
varBMO[10] <- F
varBMO <- colnames(datos)[varBMO]
var3.5 <- startsWith(colnames(datos),"Anillo3.5")
var3.5 <- colnames(datos)[var3.5]
var4.1 <- startsWith(colnames(datos),"Anillo4.1")
var4.1 <- colnames(datos)[var4.1]
var4.7 <- startsWith(colnames(datos),"Anillo4.7")
var4.7 <- colnames(datos)[var4.7]
varG <- endsWith(colnames(datos), ".G")
varG <- colnames(datos[varG])
varTI <- endsWith(colnames(datos), ".TI")
varTI <- colnames(datos[varTI])
varT <- endsWith(colnames(datos), ".T")
varT <- colnames(datos[varT])
varTS <- endsWith(colnames(datos), ".TS")
varTS <- colnames(datos[varTS])
varNS <- endsWith(colnames(datos), ".NS")
varNS <- colnames(datos[varNS])
varN <- endsWith(colnames(datos), ".N")
varN <- colnames(datos[varN])
varNI <- endsWith(colnames(datos), ".NI")
varNI <- colnames(datos[varNI])
# Seleccionamos sólo las variables de los sectores del anillo BMO
varRims <- c(varBMO, var3.5, var4.1, var4.7)
varSectors <- c(varG, varTI, varT, varTS, varNS, varN, varNI)

# Selección de datos
# Datos Glaucoma
datos.Mayores <- datos[datos$Age>40, c("Eye", "Glaucoma", varRims)]
datos.Mayores <- datos.Mayores[complete.cases(datos.Mayores), ]
datos <- datos[, c("Eye", "Glaucoma", varRims)]
datos <- datos[complete.cases(datos), ]
datos.Sanos <- datos[datos$Glaucoma=="N", ]
datos.Glaucoma <- datos[datos$Glaucoma=="Y", ]

# Análisis de componentes principales de sanos y enfermos
result.pca <- PCA(datos[, c(varRims)], scale.unit = F, graph=F)
print(result.pca)
eigenvalues <- get_eigenvalue(result.pca)
eigenvalues
fviz_eig(result.pca, main = "Variabilidad explicada por las dimensiones de los componentes principales", ylab = "Porcentaje de la variabilidad explicada", xlab = "Dimensiones")
fviz_cos2(result.pca, choice = "var", axes=1, title="Correlación de cada variable con el primer componente principal") + ylab("Coef. Determinación r²")
var$contrib
fviz_contrib(result.pca, choice="var", axes=1, top = 10)

# Correlación entre todos los sectores de los anillos
cormat <- round(cor(datos[, varRims], use="complete.obs"),2)
melted.cormat <- melt(cormat)
melted.cormat$X1 <- factor(melted.cormat$X1, levels=varSectors)
melted.cormat$X2 <- factor(melted.cormat$X2, levels=varSectors)
print(ggplot(data = melted.cormat, aes(x=X1, y=X2, fill=value)) + geom_tile()) + xlab('') + ylab('') + ggtitle("Correlaciones entre los sectores de los anillos neuroretinianos") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
# Correlación entre los sectores de los anillos más importantes
varMain <- c(varG, varTI)
cormat <- round(cor(datos[, varMain], use="complete.obs"),2)
melted.cormat <- melt(cormat)
melted.cormat$X1 <- factor(melted.cormat$X1, levels=varMain)
melted.cormat$X2 <- factor(melted.cormat$X2, levels=varMain)
print(ggplot(data = melted.cormat, aes(x=X1, y=X2, fill=value)) + geom_tile()) + geom_text(aes(label=value), color="white") + xlab('') + ylab('') + ggtitle("Correlaciones entre los sectores de los anillos neuroretinianos") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + scale_fill_gradient(limits=c(0,1))

# Crear conjunto de datos con los componentes principales
datos.pca <- cbind(result.pca$ind$coord, datos[,"Glaucoma",drop=F])
# Dibujar enfermos y sanos sobre los dos primeros componentes principales
fviz_pca_ind(result.pca, geom = c("point"), axes = c(1,2), col.ind=datos.pca$Glaucoma, ddEllipses = T, ellipse.type="t", title = "Grupos de ojos según estadio del Glaucoma sobre los dos primeros componentes principales")

# Análisis discriminane lineal de sanos y enfermos
result.lda <- lda(Glaucoma~., data=datos.pca, CV=T)
# Evaluación del modelo discriminante
confusionMatrix(result.lda$class, datos.pca$Glaucoma, positive = "Y")
pander(clusters$performance$table, "Matriz de confusión en la clasificación mediante análisis discriminante de los estadios del Glaucoma")
pander(clusters$performance$byClass[,c("Sensitivity","Specificity","Balanced Accuracy")])

# Distribución de sanos y enfermos según las principales variables
# Añadir las coordenadas de los componentes principales al conjunto de datos
datos <- cbind(datos, result.pca$ind$coord)
p1 <- ggplot(datos, aes(x = Dim.1)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones del primer componente principal en Sanos y Enfermos") 
p2 <- ggplot(datos, aes(x = Dim.2)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones del segundo componente principal en Sanos y Enfermos") 
p3 <- ggplot(datos, aes(x = Anillo3.5.G)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo 3.5 G en Sanos y Enfermos") 
p4 <- ggplot(datos, aes(x = Anillo4.1.G)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo 4.1 G en Sanos y Enfermos")
p5 <- ggplot(datos, aes(x = Anillo4.7.G)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo 4.7 G en Sanos y Enfermos") 
p6 <- ggplot(datos, aes(x = AnilloBMO.G)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo BMO G en Sanos y Enfermos") 
p7 <- ggplot(datos, aes(x = Anillo3.5.TI)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo 3.5 TI en Sanos y Enfermos") 
p8 <- ggplot(datos, aes(x = Anillo4.1.TI)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo 4.1 TI en Sanos y Enfermos")
p9 <- ggplot(datos, aes(x = Anillo4.7.TI)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo 4.7 TI en Sanos y Enfermos") 
p10 <- ggplot(datos, aes(x = AnilloBMO.TI)) + geom_density(aes(fill=Glaucoma), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones anillo BMO TI en Sanos
y Enfermos")
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol=2, nrow=5, common.legend = TRUE, legend="top")



# Análisis de componentes principales de enfermos
result.pca <- PCA(datos.Glaucoma[, c(varRims)], scale.unit = F, graph=F)
print(result.pca)
eigenvalues <- get_eigenvalue(result.pca)
eigenvalues
fviz_eig(result.pca, main = "Variabilidad explicada por las dimensiones de los componentes principales", ylab = "Porcentaje de la variabilidad explicada", xlab = "Dimensiones")
var <- get_pca_var(result.pca)
var$coord
var$cor
fviz_pca_var(result.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel=T, title="Coordenadas de las variables en los dos primeros componentes principales")
var$cos2
corrplot(var$cos2, is.corr = F, title = "Peso de las variables en cada dimensión")
fviz_cos2(result.pca, choice = "var", axes=1, title="Correlación de cada variable con la dimensión 1") + ylab("Coef. Determinación r²")
var$contrib
corrplot(var$contrib, is.corr=F)
fviz_contrib(result.pca, choice="var", axes=1:2, top = 10)

result.desc <- dimdesc(result.pca, axes = 1:2, proba = 0.05)
result.desc$Dim.1

# Añadir las coordenadas de los componentes principales al conjunto de datos
datos <- cbind(datos, result.pca$ind$coord)


##### K-MEANS
# Determinación del número de clusters
fviz_nbclust(datos.Glaucoma[, c(varBMO, var3.5)], kmeans, method = "wss") + 
  xlab("Numero de grupos") + 
  ylab("Suma de cuadrados intra-grupo") + 
  ggtitle("Determinación del número de clusters\nReducción de la variabilidad intra-grupos en función del número de clusters")

# Función para la creación de clusters basados en las variables originales y clasificación mediante análisis discriminante lineal sobre los componentes principales 
crear.clusters <- function(data, vars, labels){
  # Cálculo de los componentes principales
  result.pca <- PCA(data[, sapply(data,is.numeric)], scale.unit = F, graph=F) 
  # Crear conjunto de datos con los componentes principales
  data.pca <- as.data.frame(result.pca$ind$coord)
  require(caret)
  seed = 123
  # Número de clusters
  n = length(labels)
  # Paleta de colores para los clusters (gradiente de colores)
  colfunc <- colorRampPalette(c("#F8766D", "#00BFC4"))
  palette <- colfunc(n+1)
  # k-medias
  clusters <- kmeans(data[data$Glaucoma=="Y", vars], centers = n, nstart = 25)
  centers <- clusters$centers[order(clusters$centers[,1], decreasing = T),]
  clusters <- kmeans(data[data$Glaucoma=="Y", vars], centers = centers)
  # Convertir el cluster en un factor
  clusters$cluster <- as.factor(clusters$cluster)
  # Asignar etiquetas a los niveles del factor
  levels(clusters$cluster) <- labels
  # Añadir el cluster al conjunto de datos
  data$Cluster <- factor("Sano", levels=c("Sano", labels))
  data[data$Glaucoma=="Y", "Cluster"] <- clusters$cluster
  # Añadir el cluster al conjunto de datos de los componentes principales
  data.pca$Cluster <- data$Cluster
  # Calcular las medias de las variables en los clusters
  means <- data %>% group_by(Cluster) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  means <- t(means[, sapply(means,is.numeric)])
  colnames(means) <- c("Sanos", labels)
  # Dibujar clusters sobre componentes principales solo enfermos
  plot.glaucoma <- fviz_pca_ind(result.pca, 
                                geom = c("point"),
                                axes = c(1,2), 
                                select.ind = list(name=rownames(data[data$Glaucoma=="Y",])),
                                col.ind=data$Cluster,
                                palette = palette[2:(n+1)],
                                addEllipses = T, 
                                ellipse.type="t",
                                title = "Grupos de ojos según estadio del Glaucoma sobre los dos primeros componentes principales")
  # Dibujar los cluster sobre los componentes principales enfermos y sanos
  plot.all <- fviz_pca_ind(result.pca, 
                           geom = c("point"),
                           axes = c(1,2), 
                           col.ind=data.pca$Cluster,
                           palette = palette,
                           addEllipses = T, 
                           ellipse.type="t",
                           title = "Grupos de ojos según estadio del Glaucoma sobre los dos primeros componentes principales")
  # Distribución de los clusters
  plot.dist <- ggplot(data.pca, aes(x = Dim.1)) + 
    geom_density(aes(fill=Cluster), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones del primer componente principal según estadios de Galucoma") +
    scale_fill_manual(values=palette)
  # Predicción de los clusters mediante análisis discriminante sobre los componentes principales
  # Enfermos
  result.lda.glaucoma <- lda(Cluster~., data=data.pca[data.pca$Cluster!="Sano", ], CV=T)
  # Bondad del modelo
  performance.glaucoma <- confusionMatrix(result.lda.glaucoma$class, data.pca[data.pca$Cluster!="Sano","Cluster"])
  # Todos
  result.lda <- lda(Cluster~., data=data.pca, CV=T)
  # Bondad del modelo
  performance <-  confusionMatrix(result.lda$class,data$Cluster)
  return(list(clusters = clusters,
              means = means,
              pca = result.pca, 
              plot.glaucoma = plot.glaucoma, 
              plot.all = plot.all, 
              plot.dist = plot.dist,
              lda.glaucoma = result.lda.glaucoma,
              lda = result.lda,
              performance.glaucoma = performance.glaucoma,
              performance = performance))
}


# Función para la creación de clusters basados en los componentes principales
crear.clusters.pca <- function(data, labels, numpc=2){
  # Cálculo de los componentes principales para todos los ojos
  result.pca <- PCA(data[, sapply(data,is.numeric)], scale.unit = F, graph=F)
  # Crear conjunto de datos con los componentes principales
  data.pca <- as.data.frame(result.pca$ind$coord)
  require(caret)
  seed = 123
  # Número de clusters
  n = length(labels)
  # Paleta de colores para los clusters (gradiente de colores)
  colfunc <- colorRampPalette(c("#F8766D", "#00BFC4"))
  palette <- colfunc(n+1)
  # k-medias basada en los dos primeros componentes principales
  clusters <- kmeans(data.pca[data$Glaucoma=="Y", paste0("Dim.", 1:numpc)], centers = n, nstart = 25)
  centers <- clusters$centers[order(clusters$centers[,1], decreasing = T),]
  clusters <- kmeans(data.pca[data$Glaucoma=="Y", paste0("Dim.", 1:numpc)], centers = centers)
  # Convertir el cluster en un factor
  clusters$cluster <- as.factor(clusters$cluster)
  # Asignar etiquetas a los niveles del factor
  levels(clusters$cluster) <- labels
  # Añadir el cluster al conjunto de datos
  data$Cluster <- factor("Sano", levels=c("Sano", labels))
  data[data$Glaucoma=="Y", "Cluster"] <- clusters$cluster
  data.pca$Cluster <- data$Cluster
  # Calcular las medias de los componentes principales en los clusters
  # means <- data.pca %>% group_by(Cluster) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  # means <- t(means[, sapply(means,is.numeric)])
  # colnames(means) <- c("Sanos", labels)
  # Dibujar clusters sobre componentes principales solo enfermos
  plot.glaucoma <- fviz_pca_ind(result.pca, 
                                geom = c("point"),
                                axes = c(1,2), 
                                select.ind = list(name=rownames(data[data$Glaucoma=="Y",])),
                                col.ind=data.pca$Cluster,
                                palette = palette[2:(n+1)],
                                addEllipses = T, 
                                ellipse.type="t",
                                title = "Grupos de ojos según estadio del Glaucoma sobre los dos primeros componentes principales")
  # Dibujar los cluster sobre los componentes principales enfermos y sanos
  plot.all <- fviz_pca_ind(result.pca, 
                           geom = c("point"),
                           axes = c(1,2), 
                           col.ind=data.pca$Cluster,
                           palette = palette,
                           addEllipses = T, 
                           ellipse.type="t",
                           title = "Grupos de ojos según estadio del Glaucoma sobre los dos primeros componentes principales")
  # Distribución de los clusters
  plot.dist <- ggplot(data.pca, aes(x = Dim.1)) + 
    geom_density(aes(fill=Cluster), colour=I("white"), position="identity", alpha=.5) + ggtitle("Distribuciones del primer componente principal según estadios de Galucoma") +
    scale_fill_manual(values=palette)
  # Predicción de los clusters mediante análisis discriminante (cuidado, alta colinearidad)
  # Enfermos
  result.lda.glaucoma <- lda(Cluster~., data=data.pca[data.pca$Cluster!="Sano",], CV=T)
  # Bondad del modelo
  performance.glaucoma <- confusionMatrix(result.lda.glaucoma$class, data.pca[data.pca$Cluster!="Sano","Cluster"])
  # Todos
  result.lda <- lda(Cluster~., data=data.pca, CV=T)
  # Bondad del modelo
  performance <-  confusionMatrix(result.lda$class, data.pca$Cluster)
  return(list(clusters=clusters,
              # means = means,
              pca = result.pca, 
              plot.glaucoma = plot.glaucoma, 
              plot.all = plot.all, 
              plot.dist=plot.dist,
              lda.glaucoma = result.lda.glaucoma,
              lda = result.lda,
              performance.glaucoma = performance.glaucoma,
              performance = performance))
}

accuracy.table <- data.frame(vars=character(), accuracy=double())
# Clusters basados en el primer componente principal de todos los anillos
vars <- "Primer componente principal"
clusters <- crear.clusters.pca(datos[, c("Glaucoma", varRims)], labels=c("Dudoso", "Leve", "Moderado", "Grave"), numpc=1)
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en los dos primeros componentes principales de todos los anillos
vars <- "Dos primeros componentes principaless"
clusters <- crear.clusters.pca(datos[, c("Glaucoma", varRims)], labels=c("Dudoso", "Leve", "Moderado", "Grave"), numpc=2)
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en los tres primeros componentes principales de todos los anillos
vars <- "Tres primeros componentes principales"
clusters <- crear.clusters.pca(datos[, c("Glaucoma", varRims)], labels=c("Dudoso", "Leve", "Moderado", "Grave"), numpc=3)
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])

# Clusters basados en todos los sectores de todos los anillos
vars <- "Todos los sectores de todos los anillos"
clusters <- crear.clusters(datos, vars=varRims, labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en todos los sectores globales y temporales inferiores
vars <- "Sectores global y temporal inferior de todos los anillos"
clusters <- crear.clusters(datos, vars=c(varG, varTI), labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en todos los sectores globales
vars <- "Sectores globales de todos los anillos"
clusters <- crear.clusters(datos, vars=varG, labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en todos los sectores globales
vars <- "Sectores temporales inferiores de todos los anillos"
clusters <- crear.clusters(datos, vars=varTI, labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en los sectores global y temporal inferior de losa anillos BMO y 3.5
vars <- "Sectores global y temporal inferior de los anillos BMO y 3.5"
clusters <- crear.clusters(datos, vars=c("AnilloBMO.G", "AnilloBMO.TI", "Anillo3.5.G", "Anillo3.5.TI"), labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en los sectores global y temporal inferior del anillo BMO
vars <- "Sectores global y temporal inferior del anillos BMO"
clusters <- crear.clusters(datos, vars=c("AnilloBMO.G", "AnilloBMO.TI"), labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en los sectores global y temporal inferior del anillo 3.5
vars <- "Sectores global y temporal inferior del anillo 3.5"
clusters <- crear.clusters(datos, vars=c("Anillo3.5.G", "Anillo3.5.TI"), labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en los sectores global de los anillos BMO y 3.5
vars <- "Sector global de los anillos BMO y 3.5"
clusters <- crear.clusters(datos, vars=c("AnilloBMO.G", "Anillo3.5.G"), labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados en los sectores temporales inferiores de los anillos BMO y 3.5
vars <- "Sector temporal inferior de los anillos BMO y 3.5"
clusters <- crear.clusters(datos, vars=c("AnilloBMO.TI", "Anillo3.5.TI"), labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados el sector global del anillo BMO
vars <- "Sector global del anillo BMO"
clusters <- crear.clusters(datos, vars="AnilloBMO.G", labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados el sector global del anillo 3.5
vars <- "Sector global del anillo 3.5"
clusters <- crear.clusters(datos, vars="Anillo3.5.G", labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados el sector temporal inferior del anillo BMO
vars <- "Sector temporal inferior del anillo BMO"
clusters <- crear.clusters(datos, vars="AnilloBMO.TI", labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])
# Clusters basados el sector temporal inferior del anillo 3.5
vars <- "Sector temporal inferior del anillo 3.5"
clusters <- crear.clusters(datos, vars="Anillo3.5.TI", labels=c("Dudoso", "Leve", "Moderado", "Grave"))
accuracy.table %<>% add_row(vars=vars, accuracy=clusters$performance$overall[1])

# Caracterización de los clusters
# Combinaciones lineales de los componentes principales
clusters$pca$var$coord
# Medias por clusters









# Usando el paquete flipMultivariates
lda <- LDA(Cluster ~ BMO.G + BMO.NS + BMO.N + BMO.NI + BMO.TI + BMO.T + BMO.TS, data = datos.Clusters, 
           prior = "Observed", 
           output = "Prediction-Accuracy Table", 
           show.labels = F,
           seed = 123
           )

lda




# k-medioids
# Determinación del número de clusters
fviz_nbclust(datos.Glaucoma[datos.Glaucoma$Eye=="L", varRims], pam, method = "silhouette")
# k-medioids 2 clusters
result <- pam(datos.Glaucoma[datos.Glaucoma$Eye=="L", varRims], 4)
print(result)
fviz_cluster(result,
             palette = brewer.pal(4, "Set1"),
             ellipse.type = "t", # Concentration ellipse
             repel = F, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
# Añadir el cluster al que pertenece cada ojo
datos.Glaucoma[datos.Glaucoma$Eye=="L",]$Cluster  <- result$cluster
datos.Sanos$Cluster <- 0
datos.Clusters <- rbind(datos.Sanos, datos.Glaucoma)
datos.Clusters$Cluster <- as.factor(datos.Clusters$Cluster)
# Dibujar los cluster sobre los componentes princiaples
result.pca <- PCA(datos.Clusters[datos.Clusters$Eye=="L", varRims], scale.unit = F, graph=F)
result.pca$var$contrib
fviz_pca_ind(result.pca, axes = c(1,2), col.ind=datos.Clusters[datos.Clusters$Eye=="L",]$Cluster, addEllipses = T, ellipse.type="t")





# Análisis discriminante
# Requisitos
# Multicolinearidad
corDF = cor(datos[, varRims]);
dissimilarity <- 1 - abs(corDF);
# ATENCIÓN: Correlación muy alta entre las variables predictoras
distance <- as.dist(dissimilarity);
hc <- hclust(distance);  
clusterV = cutree(hc,h=0.05);

lda <- lda(Glaucoma~., data=datos[, -1])
predict(lda)
plda <- predict(object = lda, newdata = datos)
plda$class
plda$posterior
plda$x
datos.lda <- cbind(datos, plda$x, plda$posterior, plda$class)
ggplot(data = datos.lda, aes(x=BMO.G, fill=Glaucoma, colour=I("white"))) + geom_density(alpha=0.5)
result <- kmeans(datos.lda[datos.lda$Glaucoma=="Y", "LD1"], 4, nstart = 25)
result
fviz_cluster(result, data = datos.lda[datos.lda$Glaucoma=="Y", "LD1"],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = F, # Add segments from centroids to items
             repel = F, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)
# Añadir el cluster al que pertenece cada ojo
datos.lda$Cluster <- 0
datos.lda[datos.lda$Glaucoma=="Y",]$Cluster  <- result$cluster
datos.lda$Cluster <- as.factor(datos.lda$Cluster)
# Dibujar los cluster sobre los componentes princiaples
result.pca <- PCA(datos.lda[, varRims], scale.unit = F, graph=F)
result.pca$var$contrib
fviz_pca_ind(result.pca, axes = c(1,2), col.ind=datos.lda$Cluster, addEllipses = T, ellipse.type="t")








lda <- LDA(Glaucoma ~ ., data=datos[, -1], output = "Detail")
lda
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


# Anova multivariante
summary(manova(z~zz$A),test="Wilks")