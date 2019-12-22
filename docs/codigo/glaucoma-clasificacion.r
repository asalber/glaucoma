# Proyecto Glaucoma
# Alfredo Sánchez Alberca (asalber@ceu.es)

# ANÁLISIS DISCRININATE
# Clasificación mediante Análisis Discriminante

## BMO
#Utilizando como variables predictoras sólo las del anillo BMO.

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
