#### Diretorio ####
setwd("C:\\Users\\pedro\\Desktop\\independentes")

#### Pacotes ####
library(sf)
library(sp)
library(rgdal)
library(tmap)
library(raster)
library(tidyverse)
library(openxlsx)
library(readxl)
library(geobr)

library(cleangeo) # Correcao topologica
library(spdep) # Dependencia espacial
library(pgirmess) # Correlograma de distancia
library(spatialreg) # Regressao espacial global
library(spgwr) # Regressao ponderada geografica

#### Variaveis calculadas ####
shp_sp <- read_census_tract(code_tract = 35, year = 2010) %>%
  rename(Cod_setor = code_tract) %>%
  mutate(Cod_setor = as.factor(Cod_setor))

#### Porcentagem de homens entre 15 e 25 anos (Segmento populacao predisposto a cometer infracoes) 
# - Pessoa12UF - V001 Homens residentes em domicílios particulares e domicílios coletivos
# - V049 Homens com 15 anos de idade
# - V059 Homens com 25 anos de idade
# - ((sum(V049:V059) / V001) * 100)

# Censo SP CAPITAL
setwd("C:\\Users\\pedro\\Desktop\\Monografia\\RStudio\\Bancos de dados Mono\\SP_Capital_20190823\\Base informaçoes setores2010 universo SP_Capital\\CSV")
censo2010sp_cap <- read.csv("Pessoa12_SP1.csv", sep = ";") %>%
  select(Cod_setor, V001, V049:V059)

# Censo NAO SP CAPITAL
setwd("C:\\Users\\pedro\\Desktop\\Monografia\\RStudio\\Bancos de dados Mono\\SP_Exceto_a_Capital_20190207\\SP Exceto a Capital\\Base informaçoes setores2010 universo SP_Exceto_Capital\\CSV")
censo2010naosp_cap <- read.csv("Pessoa12_SP.csv", sep = ";") %>%
  select(Cod_setor, V001, V049:V059)

# Unindo
censo2010_homens15_25 <- rbind(censo2010sp_cap, censo2010naosp_cap)
rm(censo2010naosp_cap, censo2010sp_cap)

censo2010_homens15_25 <- censo2010_homens15_25 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(Cod_setor = as.factor(Cod_setor))

# Adicionando dados por setor censitario no shapefile de SP
shp_sp <- shp_sp %>%
  left_join(censo2010_homens15_25)

# Realizando agrupamento por setor censitario e calculando variavel
shp_sp <- shp_sp %>%
  group_by(Cod_setor) %>%
  summarise(V049_V059 = sum(c_across(V049:V059), na.rm = TRUE),
            V001 = sum(V001, na.rm = TRUE),
            code_muni = code_muni,
            geom = geom) %>%
  ungroup()

shp_sp2 <- shp_sp %>%
  st_drop_geometry()

# Agrupando por municipio e calculando vaariavel
shp_sp2 <- shp_sp2 %>% 
  group_by(code_muni) %>%
  summarise(V049_V059 = sum(V049_V059, na.rm = TRUE),
            V001 = sum(V001, na.rm = TRUE)) %>%
  ungroup()

shp_sp2 <- shp_sp2 %>% 
  group_by(code_muni) %>%
  summarise(man_predisposition = (V049_V059/V001) * 100) %>%
  ungroup()

# man_predispositiondf
man_predispositiondf <- shp_sp2 

#### Porcentagem de familias lideradas por mulher (Instabilidade familiar)
# Pessoa13UF - V003 Mulheres responsáveis pelo domicílio particular
# V002 Mulheres em domicílios particulares permanentes
# (sum(V003) / V002) * 100
# Shapefile
shp_sp <- read_census_tract(code_tract = 35, year = 2010) %>%
  rename(Cod_setor = code_tract) %>%
  mutate(Cod_setor = as.factor(Cod_setor))

# Censo SP CAPITAL
setwd("C:\\Users\\pedro\\Desktop\\Monografia\\RStudio\\Bancos de dados Mono\\SP_Capital_20190823\\Base informaçoes setores2010 universo SP_Capital\\CSV")
censo2010sp_cap <- read.csv("Pessoa13_SP1.csv", sep = ";") %>%
  select(Cod_setor, V002, V003)


# Censo NAO SP CAPITAL
setwd("C:\\Users\\pedro\\Desktop\\Monografia\\RStudio\\Bancos de dados Mono\\SP_Exceto_a_Capital_20190207\\SP Exceto a Capital\\Base informaçoes setores2010 universo SP_Exceto_Capital\\CSV")
censo2010naosp_cap <- read.csv("Pessoa13_SP.csv", sep = ";") %>%
  select(Cod_setor, V002, V003)

# Unindo
censo2010_lider_mulher <- rbind(censo2010sp_cap, censo2010naosp_cap)
rm(censo2010naosp_cap, censo2010sp_cap)

censo2010_lider_mulher <- censo2010_lider_mulher %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(Cod_setor = as.factor(Cod_setor))

# Adicionando dados por setor censitario no shapefile de SP
shp_sp <- shp_sp %>%
  left_join(censo2010_lider_mulher)

shp_sp <- shp_sp %>%
  st_drop_geometry()

# Realizando agrupamento por setor censitario e calculando variavel
shp_sp <- shp_sp %>%
  group_by(Cod_setor) %>%
  summarise(V002 = sum(V002, na.rm = TRUE),
            V003 = sum(V003, na.rm = TRUE),
            code_muni = code_muni) %>%
  ungroup()

shp_sp2 <- shp_sp

# Agrupando por municipio e calculando vaariavel
shp_sp2 <- shp_sp2 %>% 
  group_by(code_muni) %>%
  summarise(family_instability = (sum(V003)/sum(V002)) * 100) %>%
  ungroup()

# family_instability
family_instabilitydf <- shp_sp2
write.xlsx(family_instabilitydf, "family_instability.xlsx")

# Domicilio01- V002 Domicílios particulares permanentes
# V081 Domicílios particulares permanentes com mulher responsável e mais 1 morador até
# V099
# (sum(V081:V099) / V002) *100 
# Censo SP CAPITAL
setwd("C:\\Users\\pedro\\Desktop\\Monografia\\RStudio\\Bancos de dados Mono\\SP_Capital_20190823\\Base informaçoes setores2010 universo SP_Capital\\CSV")
censo2010sp_cap <- read.csv("Domicilio01_SP1.csv", sep = ";") %>%
  select(Cod_setor, V002, V081:V099)

# Censo NAO SP CAPITAL
setwd("C:\\Users\\pedro\\Desktop\\Monografia\\RStudio\\Bancos de dados Mono\\SP_Exceto_a_Capital_20190207\\SP Exceto a Capital\\Base informaçoes setores2010 universo SP_Exceto_Capital\\CSV")
censo2010naosp_cap <- read.csv("Domicilio01_SP2.csv", sep = ";") %>%
  select(Cod_setor, V002, V081:V099)

# Unindo
censo2010_2lider_mulher <- rbind(censo2010sp_cap, censo2010naosp_cap)
rm(censo2010naosp_cap, censo2010sp_cap)

censo2010_2lider_mulher <- censo2010_2lider_mulher %>%
  mutate_if(is.character, as.numeric)

censo2010_2lider_mulher <- censo2010_2lider_mulher %>%
  group_by(Cod_setor) %>%
  summarise(home_instability2 = (sum(c_across(V081:V099))/V002) * 100) %>%
  ungroup()


#### Variaveis coletadas ####

#### Gini index
# Fonte: IBGE/Censos Demográficos 1991, 2000 e 2010			
# Índice de Gini da renda domiciliar per capita segundo Município			
gini <- read_xlsx("gini_br_SIM.xlsx")

#### Densidade demografica
density <- read_xlsx("dens_demogr_censo_2010completa.xlsx")

#### Renda Per Capta
# Renda média domiciliar per capita - Brasil
# O salário mínimo do último ano para o qual a série está sendo calculada torna-se a referência para toda a série. Esse valor é corrigido
# para todos com base no INPC de julho de 2010, alterando o valor da linha de pobreza e consequentemente a proporção de pobres.
# Nesta tabela, o valor de referência, salário mínimo de 2010, é de R$ 510,00.
# Fonte: IBGE - Censos Demográficos
# Datasus
income_percapta <- read_xlsx("renda_percapta_2010tabnet.xlsx")

#### Religiao
#  População residente, por religião
# Fonte: IBGE - Censo Demográfico
no_religion <- read_xlsx("religiao_mun_2010.xlsx")

#### education
# Sidra
# Porcentagem adolescentes frequentando escola (Eficiencia escolar da regiao)
# Taxa de escolarização das pessoas de 10 a 17 (educ basica) anos de idade, por grupos de idade e situação de ocupação na semana de referência (Vide Notas)
# 2010
education <- read_xlsx("taxa_escolarizacao2010_sidra.xlsx") 

#### Qualidade policial
library(haven)
path <- file.path()
security_quality <- read_sav("Base_SPSS_PM_2010.sav")

#### Aprisionamento
imprisonment <- read.xlsx("aprisionamento2014_infopen.xlsx") %>%
  filter(`Invite:.UF` == 'SP')
