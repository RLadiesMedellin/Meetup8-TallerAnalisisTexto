# ---------------------------------------------------------------------
# Taller procesamiento y análisis de texto en R
# ---------------------------------------------------------------------


# Extracción de tweets con R

# instalar paquete
install.packages("rtweet")

# Cargar paquete
library(rtweet)

# Credenciales de Twitter 
# https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/object-model/tweet
consumerKey <- 'XXXXXX'
consumerSecret <- 'XXXXXX'
accessToken <- 'XXXXXX'
accessTokenSecret <- 'XXXXXX'

# Nombre que le diste a la API 
appname <- "appname"

# Crear token para extraer datos
twitter_token <- create_token(app = appname,
                              consumer_key = consumerKey,
                              consumer_secret = consumerSecret,
                              access_token = accessToken,
                              access_secret = accessTokenSecret)
# Buscar tweets
datos_vacunacion <- search_tweets('vacunas OR vacunación AND "covid 19" AND colombia', n = 5000, include_rts = FALSE, lang = "es")

# Instalar paquetes
install.packages(c("readr", "dplyr", "tidytext", "tm", "Hmisc", "wordcloud", "stringr", "tidyr", "udpipe"))

# Cargar paquetes
library(dplyr)
library(stringr)
library(tidytext)
library(tm)
library(wordcloud)
library(Hmisc)
library(tidyr)
library(udpipe)
library(ggplot2)

# Leer la base de datos
datos <- read.csv("/content/datos_vacunacion_taller.csv")
head(datos, 5)

# Dimensiones de la base de datos
print(dim(datos))

# Nombre de las columnas
print(colnames(datos))

# Explorar variables de la base de datos
glimpse(datos)

# Separar fecha y hora
datos <- separate(datos, created_at, c("date", "time"), sep = " ", remove = FALSE)
head(datos, 5)

# Columna fecha de tipo caracter a tipo date
datos$date <- as.Date(datos$date, format = "%d/%m/%Y")

glimpse(datos)

print(min(datos$date))
print(max(datos$date))

# Cantidad de tweets por día
datos %>% group_by(date) %>% 
  summarise(Frecuencia = n()) %>%
  ggplot(aes(x = date, y = Frecuencia)) +
  geom_line(color = "purple") +
  geom_point(color = "purple") +
  labs(x = "Frecuencia", y = "Fecha")

# Top source
unique(datos$source)
datos %>% count(source, sort = TRUE) %>%
  top_n(5)

# Top retweets
datos %>% arrange(-retweet_count) %>%
  head(5) %>%
  select(id_tweet, created_at, text, retweet_count)

# Top favoritos
datos %>% arrange(-favorite_count) %>%
  head(5) %>%
  select(id_tweet, created_at, text, favorite_count)

devtools::install_github("hadley/emo")
library(emo)

datos %>% mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>% 
  count(emoji, sort = TRUE) %>%
  top_n(10)

# Top hashtags
datos %>% unnest_tokens(hashtag, hashtags) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = n, y = reorder(hashtag, n))) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(x = "Frecuencia", y = "Hashtag")