#### Diretorio ####
setwd("C:\\Users\\pedro\\Desktop\\Monografia\\RStudio\\Spatial Econometrics\\R_CTA_aula8\\aula8")

#### Pacotes ####
library(sf)
library(sp)
library(rgdal)
library(tmap)
library(raster)
library(tidyverse)

library(cleangeo) # Correcao topologica
library(spdep) # Dependencia espacial
library(pgirmess) # Correlograma de distancia
library(spatialreg) # Regressao espacial global
library(spgwr) # Regressao ponderada geografica

#### Importar Dados ####
st_layers("aula8.gpkg")

setores_sf <- st_read("aula8.gpkg", layer = "setores_abc")

# 1- Municípios da Região Metropolitana de São Paulo
# 2- Setores Censitários do ABC Paulista

view(setores_sf)

# Alguns setores tem informacoes zeradas e outros são NA
plot(setores_sf["rede_esg"], border = NA)
plot(setores_sf["Renda"], border = NA)

# Acha que tem relacao entre tratamento de esgoto e renda?