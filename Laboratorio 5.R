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

#---------------------------- Lectura de Datos ---------------------------------------#

Datos <- read.csv("./datos/GrammarandProductReviews.csv", stringsAsFactors = F)

#-------------------------- Limpieza y preprocesamiento ------------------------------#

#Se eliminan las columnas que contienen datos que no son relevantes
Datos <- subset(Datos, select=-c(id,dateAdded,dateUpdated,ean,keys,manufacturerNumber,reviews.date,reviews.dateAdded,reviews.dateSeen,reviews.id,reviews.numHelpful,reviews.sourceURLs,reviews.userCity,reviews.userProvince,upc))

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
RemoverURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
DatosLimpios <- tm_map(DatosLimpios, RemoverURL)
Espacio <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
DatosLimpios <- tm_map(DatosLimpios, Espacio, "/")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "@")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "\\|")

#Se eliminan las puntuaciones y símbolos
DatosLimpios<-tm_map(DatosLimpios, content_transformer(removePunctuation))

#Se eliminan los números para que no interfieran con la predicción
DatosLimpios<-tm_map(DatosLimpios, content_transformer(removeNumbers))

#Se eliminan artículos, preposiciones y conjunciones (stopwords)
DatosLimpios<-tm_map(DatosLimpios, removeWords, stopwords('english'))

#Eliminación de "malas palabras" por medio de una lista de estas
MP <- file("./datos/profanities.txt", "r")
MPVector <- VectorSource(readLines(MP))
DatosLimpios <- tm_map(DatosLimpios, removeWords, MPVector)