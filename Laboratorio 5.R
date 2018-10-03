#------------------------------------------------------------#
# Laboratorio 5
# Grupo 1
#------------------------------------------------------------#

#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("stringr","tm", "R.utils","openNLP","qdap","RWeka","ggplot2", "dplyr","tidyr","wordcloud")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#---------------------- Lectura de Datos --------------------------------------------#
Datos <- read.csv("./GrammarandProductReviews.csv", stringsAsFactors = F)

#Se realiza un vector de los datos y se convierten en volátiles para cambiar su contenido
VectorDatos <- VectorSource(Datos)
DatosLimpios <- VCorpus(VectorDatos)

#Preparar transformación
DatosLimpios <- tm_map(DatosLimpios, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

#Se transforman los caracteres a minúsculas
DatosLimpios<-tm_map(DatosLimpios, content_transformer(tolower))

#Se eliminan los espacios en blanco adicionales
DatosLimpios<-tm_map(DatosLimpios, content_transformer(stripWhitespace))

#Se eliminan los URLs, se detiene al encontrar un espacio
train_raw<- train_raw[,-train_raw$reviews.sourceURLs]
