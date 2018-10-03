for (libreria in c("stringr","tm", "R.utils","openNLP","rJava","qdap","RWeka","ggplot2","stylo")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

Datos <- read.csv("./datos/GrammarandProductReviews.csv", stringsAsFactors = F)
