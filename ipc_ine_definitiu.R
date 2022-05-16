if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(httr)) install.packages("httr"); library(httr)
if (!require(jsonlite)) install.packages("jsonlite"); library(jsonlite)
if (!require(rjstat)) install.packages("rjstat"); library(rjstat)
if (!require(tictoc)) install.packages("tictoc"); library(tictoc)
if (!require(highcharter)) install.packages("highcharter"); library(highcharter)
library(stringr)
library(tidyverse)
library(lubridate)
library(rlist)
library(magrittr)

rm(list = ls())
#################################################

# Per tal d'extreure dades mitjançant el creuament de metadades s'ha d'utilitzar un altre tipus d'url.
# Amb el DATOS_SERIE podíem extreure les dades individualment, però amb DATOS_METADATAOPERACIÓN podem 
# afegir els paràmetres de búsqueda.

# Per exemple, podem fer una petició tal com "/DATOS_METADAOPERACIÓN/IPC?g1=115:29&g2=3:74&g3=762:&nult=1"
# Després de l'IPC no especificam cap codi, això es farà en els paràmetres:

# g1=115:29, 115 és el codi per la selecció de províncies i 29 és el codi de Madrid. A código_provincias es pot buscar el codi de qualsevol
# g2=3:74, 3 és el codi per al grup de TIPO DE DATOS i 74 es refereix a la variació anual. A códigos_tipo_dato es pot veure els diferents codis per cada mesura
# g3=762, fa referència als grups ECOICOP (els del nostre interès) i els seleccionam tots.
# nult=1, en aquest tipus de url no es pot posar un rang de dates sinó les últimes dates disponibles, llavors nult=1 retornarà l'última data disponible


categorias_url <- "https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC"
res_categorias_url <- GET(categorias_url)
codigos_categorias <- fromJSON(rawToChar(res_categorias_url$content))

tipo_dato_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/3/IPC"
res_tipo_dato_url <- GET(tipo_dato_url)
codigos_tipo_dato <- fromJSON(rawToChar(res_tipo_dato_url$content))

provincias_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/115/IPC"
provincias_url <- GET(provincias_url)
codigo_provincias <- fromJSON(rawToChar(provincias_url$content))

comunidades_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/70/IPC"
res_comunidades_url <- GET(comunidades_url)
codigos_comunidades <- fromJSON(rawToChar(res_comunidades_url$content))

nacional_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/349/IPC"
res_nacional_url <- GET(nacional_url)
codigos_nacional <- fromJSON(rawToChar(res_nacional_url$content))

grupos_especiales_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/269/IPC"
res_grupos_especialesl_url <- GET(grupos_especiales_url)
codigos_grupos_especiales <- fromJSON(rawToChar(res_grupos_especialesl_url$content))

grupos_ECOICOP_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/762/IPC"
res_grupos_ECOICOP_url <- GET(grupos_ECOICOP_url)
codigos_grupos_ECOICOP <- fromJSON(rawToChar(res_grupos_ECOICOP_url$content))

#################################################
# INE 
# Madrid

tic()
period <- days(today() - ymd(20000101))$day
url <- str_c("https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?g1=70:9009&g2=3:74&g3=762:&p=1&nult=",period)
res =  GET(url)
categories <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  select(Nombre) %>%
  unlist() %>%
  cbind() %>%
  as_tibble() %>%
  rename(Nombre = ".") %>%
  tidyr::separate(Nombre, into = c("Regio", "Categoria"), sep = "\\. ")

raw_data <- fromJSON(rawToChar(res$content))$Data 

# Extreim les llistes
my_function <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Categoria, Data, Valor, Regio)
  data_frame
}

# Cream un data frame buit i anam copiant les llistes a la data frame
IPC_INE_MAD <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(my_function(raw_data, i)) 
  IPC_INE_MAD <- rbind(data, IPC_INE_MAD)
}

IPC_INE_MAD <- IPC_INE_MAD %>% arrange(Data) %>% filter(Data > ymd(20020101)) %>% 
  spread(key = "Categoria", value = "Valor")

######

# IPC subjacent
period <- days(today() - ymd(20000101))$day
url <- str_c("https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?g1=70:9009&g2=3:74&g3=269:12850&p=1&nult=",period)
res =  GET(url)
categories <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  select(Nombre) %>%
  slice(2) %>%
  unlist() %>%
  cbind() %>%
  as_tibble() %>%
  rename(Nombre = ".") %>%
  tidyr::separate(Nombre, into = c("Regio", "Categoria"), sep = "\\. ")

raw_data <- fromJSON(rawToChar(res$content))$Data[2] 

# Extreim les llistes
my_function <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Data, Valor) 
  data_frame
}

# Cream un data frame buit i anam copiant les llistes a la data frame
IPC_INE_MAD_sub <- vctrs::data_frame(.size = 0)

data <- as_tibble(my_function(raw_data, 1)) 
IPC_INE_MAD_sub <- rbind(data, IPC_INE_MAD_sub)  %>% arrange(Data) %>% filter(Data > ymd(20020101))

IPC_INE_MAD <- IPC_INE_MAD %>% 
  cbind(IPC_INE_MAD_sub$Valor) %>%
  rename(Subjacent = "IPC_INE_MAD_sub$Valor")

toc()



#################################################
# INE 
# Barcelona

tic()
period <- days(today() - ymd(20000101))$day
url <- str_c("https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?g1=115:9&g2=3:74&g3=762:&p=1&nult=",period)
res = GET(url)
categories <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  select(Nombre) %>%
  unlist() %>%
  cbind() %>%
  as_tibble() %>%
  rename(Nombre = ".") %>%
  tidyr::separate(Nombre, into = c("Regio", "Categoria"), sep = "\\. ")

raw_data <- fromJSON(rawToChar(res$content))$Data 

# Extreim les llistes
my_function <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Categoria, Data, Valor, Regio)
  data_frame
}

# Cream un data frame buit i anam copiant les llistes a la data frame
IPC_INE_BAR <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(my_function(raw_data, i)) 
  IPC_INE_BAR <- rbind(data, IPC_INE_BAR)
}

IPC_INE_BAR <- IPC_INE_BAR %>% arrange(Data) %>% filter(Data > ymd(20020101)) %>% 
  spread(key = "Categoria", value = "Valor") %>% mutate(Subjacent = NA)
toc()


#################################################
# INE 
# Catalunya

tic()
period <- days(today() - ymd(20000101))$day
url <- str_c("https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?g1=70:9005&g2=3:74&g3=762:&p=1&nult=8104",period)
res = GET(url)
categories <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  select(Nombre) %>%
  unlist() %>%
  cbind() %>%
  as_tibble() %>%
  rename(Nombre = ".") %>%
  tidyr::separate(Nombre, into = c("Regio", "Categoria"), sep = "\\. ")

raw_data <- fromJSON(rawToChar(res$content))$Data 

# Extreim les llistes
my_function <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Categoria, Data, Valor, Regio)
  data_frame
}

# Cream un data frame buit i anam copiant les llistes a la data frame
IPC_INE_CAT <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(my_function(raw_data, i)) 
  IPC_INE_CAT <- rbind(data, IPC_INE_CAT)
}

IPC_INE_CAT <- IPC_INE_CAT %>% arrange(Data) %>% filter(Data > ymd(20020101)) %>% 
  spread(key = "Categoria", value = "Valor")


##########

# IPC subjacent
period <- days(today() - ymd(20000101))$day
url <- str_c("https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?g1=70:9005&g2=3:74&g3=269:12850&p=1&nult=",period)
res =  GET(url)
categories <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  select(Nombre) %>%
  slice(1) %>%
  unlist() %>%
  cbind() %>%
  as_tibble() %>%
  rename(Nombre = ".") %>%
  tidyr::separate(Nombre, into = c("Regio", "Categoria"), sep = "\\. ")

raw_data <- fromJSON(rawToChar(res$content))$Data[1] 

# Extreim les llistes
my_function <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Data, Valor) 
  data_frame
}

# Cream un data frame buit i anam copiant les llistes a la data frame
IPC_INE_CAT_sub <- vctrs::data_frame(.size = 0)

data <- as_tibble(my_function(raw_data, 1)) 
IPC_INE_CAT_sub <- rbind(data, IPC_INE_CAT_sub)  %>% arrange(Data) %>% filter(Data > ymd(20020101))

IPC_INE_CAT <- IPC_INE_CAT %>% 
  cbind(IPC_INE_CAT_sub$Valor) %>%
  rename(Subjacent = "IPC_INE_CAT_sub$Valor")
toc()
#################################################
# INE 
# Espanya

tic()
period <- days(today() - ymd(20000101))$day
url <- str_c("https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?g1=349:16473&g2=3:74&g3=762:&p=1&nult=",period)
res = GET(url)
categories <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  select(Nombre) %>%
  unlist() %>%
  cbind() %>%
  as_tibble() %>%
  rename(Nombre = ".") %>%
  tidyr::separate(Nombre, into = c("Regio", "Categoria", "Indicador", "Impostos"), sep = "\\. ") %>%
  filter(Impostos != "Impuestos constantes") %>%
  select(-c(Indicador,Impostos))

# Identificam les files que contenen dades segons impostos constants
posicions <- fromJSON(content(res, "text", encoding = "UTF-8")) %>% .$Nombre %>% str_detect("Impuestos")

raw_data <- fromJSON(content(res, "text", encoding = "UTF-8"))$Data[!posicions]

# Extreim les llistes
my_function <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Categoria, Data, Valor, Regio)
  data_frame
}

# Cream un data frame buit i anam copiant les llistes a la data frame
IPC_INE_ESP <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(my_function(raw_data, i)) 
  IPC_INE_ESP <- rbind(data, IPC_INE_ESP)
}

IPC_INE_ESP <- IPC_INE_ESP  %>% filter(Data > ymd(20020101)) %>% 
  spread(key = "Categoria", value = "Valor")

##########

# IPC subjacent
period <- days(today() - ymd(20000101))$day
url <- str_c("https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?g1=349:16473&g2=3:74&g3=269:12850&p=1&nult=",period)
res =  GET(url)
categories <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  select(Nombre) %>%
  slice(2) %>%
  unlist() %>%
  cbind() %>%
  as_tibble() %>%
  rename(Nombre = ".") %>%
  tidyr::separate(Nombre, into = c("Regio", "Categoria"), sep = "\\. ")

raw_data <- fromJSON(rawToChar(res$content))$Data[2] 

# Extreim les llistes
my_function <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Data, Valor) 
  data_frame
}

# Cream un data frame buit i anam copiant les llistes a la data frame
IPC_INE_ESP_sub <- vctrs::data_frame(.size = 0)

data <- as_tibble(my_function(raw_data, 1)) 
IPC_INE_ESP_sub <- rbind(data, IPC_INE_ESP_sub)  %>% arrange(Data) %>% filter(Data > ymd(20020101))

IPC_INE_ESP <- IPC_INE_ESP %>% 
  cbind(IPC_INE_ESP_sub$Valor) %>%
  rename(Subjacent = "IPC_INE_ESP_sub$Valor")
toc()


#################################################
# Taula final

IPC_INE <- IPC_INE_MAD %>% 
  rbind(IPC_INE_CAT) %>% 
  rbind(IPC_INE_BAR) %>% 
  rbind(IPC_INE_ESP) %>%
  arrange(Data) %>%
  filter(!is.na(Data)) %>%
  select(Data, Regio, `Índice general`, `Alimentos y bebidas no alcohólicas`:Subjacent)


