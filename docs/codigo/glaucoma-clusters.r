# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Clusters de pacientes con  glaucoma
source("codigo/carga-datos.R")

# Load packages
.packages <- c("reshape2", "FactoMineR", "factoextra", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Filtrado de ojos izquierdos
data %<>% filter(Ojo=="L")
# Conjunto de datos de ojos con glaucoma
data.glaucoma <- data[data$Glaucoma == "Y", ]

# Number of clusters
fviz_nbclust(data.glaucoma[, varRims], kmeans, method = "wss") + 
  xlab("Número de grupos") + 
  ylab("Suma de cuadrados intra-grupos") + 
  ggtitle("Reducción de la variabilidad intra-grupos según el número de grupos")
#ggsave("img/reduccion-variabilidad-numero-grupos.pdf")

# K-means clusters
seed = 123
# Variables considered
vars = varRims
# Número de clusters
n = 4
labels <- c("I", "II", "III", "IV")
# Color palette
colfunc <- colorRampPalette(c("#FD8D3C", "#800026"))
palette <- colfunc(n)
palette.healthy <- c("#00BFC4", palette)
# Shapes
shapes <- c(1, 3, 15, 17, 16)
# k-means
clusters <- kmeans(data.glaucoma[, vars], centers = n, nstart = 25)
centers <- clusters$centers[order(clusters$centers[,1], decreasing = T),]
clusters <- kmeans(data.glaucoma[, vars], centers = centers)
# Convert the cluster into a factor
clusters$cluster <- as.factor(clusters$cluster)
# Assign labels to factors
levels(clusters$cluster) <- labels
# Add cluster to data frame
data$Estadio <- factor("Sano", levels=c("Sano", labels))
data[data$Glaucoma=="Y", "Estadio"] <- clusters$cluster
# Save data frame with clusters
write.csv(data, file = "datos/datos-anillos-preprocesados-estadios.csv", row.names=FALSE, sep=",")

# Principal components
result.pca <- PCA(data[, sapply(data,is.numeric)], scale.unit = F, graph=F) 
# Add principal components coordinates to a data frame
data.pca <- as.data.frame(result.pca$ind$coord)
# Add cluster to principal components data frame
data.pca$Estadio <- data$Estadio

# Map of clusters (without healthy eyes)
fviz_pca_ind(
  result.pca,
  geom = c("point"),
  axes = c(1, 2),
  select.ind = list(name = rownames(data[data$Glaucoma == "Y", ])),
  col.ind = data$Estadio,
  palette = palette,
  addEllipses = T,
  ellipse.type = "t",
  title = "Mapa de estadios de glaucoma sobre los dos primeros componentes principales",
  legend.title = "Estadio"
) +
  xlab("Primer componente principal") +
  ylab("Segundo componente principal") +
  scale_shape_manual(values = shapes[-1]) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave("img/mapa-glaucoma-estadios-sin-sanos.pdf")

# Map of clusters (with healthy eyes)
fviz_pca_ind(
  result.pca,
  geom = c("point"),
  axes = c(1, 2),
  col.ind = data$Estadio,
  palette = palette.healthy,
  addEllipses = T,
  ellipse.type = "t",
  title = "Mapa de estadios de glaucoma sobre los dos primeros componentes principales",
  legend.title = "Estadio"
) +
  xlab("Primer componente principal") +
  ylab("Segundo componente principal") +
  scale_shape_manual(values = shapes) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave("img/mapa-glaucoma-estadios-con-sanos.pdf")

# Clusters distribution
ggplot(data.pca, aes(x = Dim.1)) +
  geom_density(aes(fill = Estadio), colour = I("white"), position = "identity", alpha = .5) + 
  ggtitle("Distribución de los primeros componentes principales según el estadio del glaucoma") +
  xlab("Primer componente principal") +
  scale_fill_manual(values = palette.healthy) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("img/distribucion-glaucoma-estadios.pdf")

# Means of variables by clusters
melted.data <- melt(data[, c("Estadio", vars)])
colnames(melted.data)[2] <- "Sector" 
means <- melted.data %>% group_by(Estadio, Sector) %>% 
  summarise(n = n(), mean = mean(value, na.rm = T), sd = sd(value, na.rm = T))  %>%
  mutate(se = sd/sqrt(n), lower.ci = mean-qt(1-(0.05/2), n-1)*se, upper.ci = mean+qt(1-(0.05/2), n-1)*se)

# Plot of means by clusters
ggplot(means, aes(x = Sector, y = mean, colour = Estadio, shape = Estadio)) +
  geom_pointrange(aes(ymin = lower.ci, ymax = upper.ci), position = position_dodge(width = 0.5)) +
  ggtitle("Intervalos de confianza para las medias del BMO y los anillos RNF por estadios") +
  scale_colour_manual(values = palette.healthy) +
  scale_shape_manual(values = shapes) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5), plot.title = element_text(hjust = 0.5))
  