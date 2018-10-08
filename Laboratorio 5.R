#------------------------------------------------------------#
# Laboratorio 5
# Grupo 1
#------------------------------------------------------------#

#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("stringr","tm", "R.utils","openNLP","qdap","RWeka","ggplot2", "wordcloud", "RColorBrewer","tidyr","dplyr","tidytext","knitr","kableExtra","formattable","ggplot2","memery","magick")) {
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

#Se eliminan todos los NAs para que no interfieran con el análisis
Datos <- na.omit(Datos)

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

#------------------------ Análisis Exploratorio -----------------------------------#

#Tokenización de Unigramas
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))

#Tokenización de Bigramas
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))

#Tokenización de Trigramas
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))

#Función para obtener las frecuencias de cada palabra
Frecuencia <- function(tdm){
  Frec <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  Frecuencia <- data.frame(word=names(Frec), Frec=Frec)
  return(Frecuencia)
}

#Remoción de unigramas que reaparecen menos de una vez
Unigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = UnigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los unigramas
FrecUnigrama <- Frecuencia(Unigrama)

#Remoción de bigramas que reaparecen menos de una vez
Bigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = BigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los bigramas
FrecBigrama <- Frecuencia(Bigrama)

#Remoción de trigramas que reaparecen menos de una vez
Trigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = TrigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los trigramas
FrecTrigrama <- Frecuencia(Trigrama)

#Función para graficación de los datos
PlotFrec <- function(data, title) {
  ggplot(data[1:25,], aes(reorder(word, -Frec), Frec)) +
    labs(x = "Palabras/Frases", y = "Frecuencia") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
    geom_bar(stat = "identity")
}

#--------------------------- Gráficas de frecuencia --------------------------------------#

#Unigramas
PlotFrec(FrecUnigrama, "Unigramas más comúnes (Top 25)")

#Bigramas
PlotFrec(FrecBigrama, "Bigramas más comúnes (Top 25)")

#Trigramas
PlotFrec(FrecTrigrama, "Trigramas más comúnes (Top 25)")

#--------------------------------- Wordclouds ---------------------------------------------#

#Unigrama
#Primero se coloca el unigrama en un data frame y se visualiza una tabla de frecuencias
MatU <- as.matrix(Unigrama)
OrdenU <- sort(rowSums(MatU),decreasing=TRUE)
DFU <- data.frame(word = names(OrdenU),freq=OrdenU)
head(DFU, 10)

wordcloud(words = DFU$word, freq = DFU$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#--------------------------------- Sentiment analysis - Clasificacion de las palabras ---------------------------------------------#
#Conversion de datos a formato tidyr para utilizar las librerias de analisis de sentimientos. Libreria = tidytext.
#Toma la informacion del review que hizo la persona y la parte por palabras individuales.
Data_tidy <- Datos %>%
  unnest_tokens(word, reviews.text) %>% 
  filter(!word %in% MP) %>% 
  filter(!nchar(word) < 3) %>% 
  anti_join(stop_words) 

#le da formato a las tablas
styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

#Toma todas las palabras y las asigna segun sean los tres lexicons disponibles
#grafica en una tabla los resultados y da un indice de que tan acertado son los sentimientos
#encontrados respecto a las palabras.
Data_tidy %>%
  mutate(words_in_data = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_data, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), 
         match_ratio = lex_match_words / words_in_data) %>%
  select(lexicon, lex_match_words,  words_in_data, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  styling(caption = "Palabras encontradas en los Lexicons")

#se crean datasets por cada uno de los lexicons.
Data_nrc_sub <- Data_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

Data_nrc <- Data_tidy %>%
  inner_join(get_sentiments("nrc"))

Data_bing <- Data_tidy %>%
  inner_join(get_sentiments("bing"))



#--------------------------------- Sentiment analysis - Clasificacion del producto ---------------------------------------------#

