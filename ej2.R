library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
load("tp2.RData")
library(Metrics)

# Dataset con las columnas que nos interesan

news = fake_news %>% select(type,title_has_excl,negative,title_words)


# Realizar visualizaciones apropiadas para convencerse de que las variables predictoras podrían servir para clasificar las noticias en “reales” vs. “fake-news”.

# title_has_excl: variable binaria que indica si el título de la noticia tiene o no signos de exclamación.
# negative: porcentaje estimado de palabras en el título que tienen connotaciones negativas.
# title_words: número de palabras en el título.

ggplot(news, aes(x = type, fill = negative)) + geom_bar()

ggplot(news, aes(x = type, y = title_has_excl)) + geom_col()

# hay mas palabras en los titulos de las reales
ggplot(news, aes(x = type, y = title_words)) + geom_col()

