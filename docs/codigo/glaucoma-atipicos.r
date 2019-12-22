# Proyecto glaucoma 
# Alfredo Sánchez Alberca (asalber@ceu.es)

# DETECCIÓN DE DATOS ATÍPICOS
# Pacientes sanos
outliers.table <- function(i) {
  outliers <- boxplot(datos[datos$Glaucoma=="N", i], plot=FALSE)$out
  datos.outliers <- datos[datos[,i] %in% outliers, c(1,i)]
  datatable(format(datos.outliers,4), rownames = F, escape=F, class = 'display', options = list(pageLength = 10, dom="ltip", language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
}
ot <- lapply(11:ncol(datos), outliers.table)
do.call(div, ot)

## Pacientes con glaucoma

outliers.table <- function(i) {
  outliers <- boxplot(datos[datos$Glaucoma=="Y", i], plot=FALSE)$out
  datos.outliers <- datos[datos[,i] %in% outliers, c(1,i)]
  datatable(format(datos.outliers,4), rownames = F, escape=F, class = 'display', options = list(pageLength = 10, dom="ltip", language = list(url = 'http://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
}
ot <- lapply(11:ncol(datos), outliers.table)
do.call(div, ot)