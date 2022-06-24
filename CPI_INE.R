#########################################################
#                                                       #
#        SCRIPT TO DOWNLOAD DISSAGREGATED CPI           #
#     FROM MADRID, BARCELONA, CATALONIA AND SPAIN       #
#                                                       #
#########################################################

###-----------------------------------------------------------------------------
# Libraries
library(httr)
library(jsonlite)
library(tictoc)
library(tidyverse)
library(lubridate)
library(rlist)
library(vctrs)

###-----------------------------------------------------------------------------
# Some useful tables
categorias_url <- "https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC"
res_categorias_url <- GET(categorias_url)
codigos_categorias <- fromJSON(rawToChar(res_categorias_url$content))

tipo_dato_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/3/IPC"
res_tipo_dato_url <- GET(tipo_dato_url)
codigos_tipo_dato <- fromJSON(rawToChar(res_tipo_dato_url$content))

provincias_url <- "https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/115/IPC"
res_provincias_url <- GET(provincias_url)
codigo_provincias <- fromJSON(rawToChar(res_provincias_url$content))

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

###-----------------------------------------------------------------------------
# Functions
extract_CPI <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Categoria, Data, Valor, Regio)
  data_frame
}
extract_core_CPI <- function(data, x) {
  data_frame  <- data %>%
    list.extract(x) %>%
    mutate(Regio = categories[[1]][x],
           Categoria = categories[[2]][x],
           Data = as.POSIXct(Fecha/1000, origin="1970-01-01")) %>%
    select(Data, Valor)
  data_frame
}
###-----------------------------------------------------------------------------
### Madrid ###
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

# Create an empty data frame i copy the lists into the dataframe
CPI_INE_MAD <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(extract_CPI(raw_data, i))
  CPI_INE_MAD <- rbind(data, CPI_INE_MAD)
}
CPI_INE_MAD <- CPI_INE_MAD %>% arrange(Data) %>% filter(Data > ymd(20020101)) %>%
  spread(key = "Categoria", value = "Valor")

### Core CPI ###
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

# Create an empty data frame i copy the lists into the dataframe
CPI_INE_MAD_sub <- vctrs::data_frame(.size = 0)
data <- as_tibble(extract_core_CPI(raw_data, 1))
CPI_INE_MAD_sub <- rbind(data, CPI_INE_MAD_sub)  %>% arrange(Data) %>% filter(Data > ymd(20020101))
CPI_INE_MAD <- CPI_INE_MAD %>%
  cbind(CPI_INE_MAD_sub$Valor) %>%
  rename(Core = "CPI_INE_MAD_sub$Valor")

###-----------------------------------------------------------------------------
### Barcelona ###
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

# Create an empty data frame i copy the lists into the dataframe
CPI_INE_BAR <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(extract_CPI(raw_data, i))
  CPI_INE_BAR <- rbind(data, CPI_INE_BAR)
}
CPI_INE_BAR <- CPI_INE_BAR %>% arrange(Data) %>% filter(Data > ymd(20020101)) %>%
  spread(key = "Categoria", value = "Valor") %>% mutate(Core = NA)

# There is no information available aboout the core inflation of provincies

###-----------------------------------------------------------------------------
### Catalunya ###
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

# Create an empty data frame i copy the lists into the dataframe
CPI_INE_CAT <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(extract_CPI(raw_data, i))
  CPI_INE_CAT <- rbind(data, CPI_INE_CAT)
}
CPI_INE_CAT <- CPI_INE_CAT %>% arrange(Data) %>% filter(Data > ymd(20020101)) %>%
  spread(key = "Categoria", value = "Valor")

### Core CPI ###
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

# Create an empty data frame i copy the lists into the dataframe
CPI_INE_CAT_sub <- vctrs::data_frame(.size = 0)
data <- as_tibble(extract_core_CPI(raw_data, 1))
CPI_INE_CAT_sub <- rbind(data, CPI_INE_CAT_sub) %>%
  arrange(Data) %>% filter(Data > ymd(20020101))
CPI_INE_CAT <- CPI_INE_CAT %>%
  cbind(CPI_INE_CAT_sub$Valor) %>%
  rename(Core = "CPI_INE_CAT_sub$Valor")

###-----------------------------------------------------------------------------
### Spain ###
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

# Identify the rows that contains observations with constant taxes
positions <- fromJSON(content(res, "text", encoding = "UTF-8")) %>%
  .$Nombre %>% str_detect("Impuestos")
raw_data <- fromJSON(content(res, "text", encoding = "UTF-8"))$Data[!positions]

# Create an empty data frame i copy the lists into the dataframe
CPI_INE_ESP <- vctrs::data_frame(.size = 0)
for (i in 1:13) {
  data <- as_tibble(extract_CPI(raw_data, i))
  CPI_INE_ESP <- rbind(data, CPI_INE_ESP)
}
CPI_INE_ESP <- CPI_INE_ESP  %>% filter(Data > ymd(20020101)) %>%
  spread(key = "Categoria", value = "Valor")

### Core CPI ###
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
# Create an empty data frame i copy the lists into the dataframe
CPI_INE_ESP_sub <- vctrs::data_frame(.size = 0)
data <- as_tibble(extract_core_CPI(raw_data, 1))
CPI_INE_ESP_sub <- rbind(data, CPI_INE_ESP_sub)  %>% arrange(Data) %>% filter(Data > ymd(20020101))
CPI_INE_ESP <- CPI_INE_ESP %>%
  cbind(CPI_INE_ESP_sub$Valor) %>%
  rename(Core = "CPI_INE_ESP_sub$Valor")
toc()
###-----------------------------------------------------------------------------
### Final table ###

CPI_INE <- CPI_INE_MAD %>%
  rbind(CPI_INE_CAT) %>%
  rbind(CPI_INE_BAR) %>%
  rbind(CPI_INE_ESP) %>%
  arrange(Data) %>%
  filter(!is.na(Data)) %>%
  select(Data, Regio, `Índice general`, `Alimentos y bebidas no alcohólicas`:Core)
View(CPI_INE)
