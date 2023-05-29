#### Pacotes ####
library(sf)
library(sp)
library(plyr)
library(rgdal)
library(tmap)
library(raster)
library(tidyverse)
library(openxlsx)
library(readxl)
library(geobr)
library(knitr)
library(cleangeo) # Correcao topologica
library(spdep) # Dependencia espacial
library(pgirmess) # Correlograma de distancia
library(spatialreg) # Regressao espacial global
library(spgwr) # Regressao ponderada geografica

setwd("C:\\Users\\CEA ALUNO\\Desktop\\mono-spatial-econometrics-r\\mono-spatial-econometrics-r\\Mono - Bancos de Dados")


#### 1. Iniciando Analise espacial ####