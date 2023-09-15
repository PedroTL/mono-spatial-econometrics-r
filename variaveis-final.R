library(scraEP)

# Abrindo Banco de Dados com Variaveis Independentes Atlas Brasil
independentes <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\independentes_r.xlsx")

# Adicionando Variavel Taxa de Desemprego no Banco de Dados Final
unemployment_rate2000_2010 <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\taxa_desemprego_1991_2000_2010tabnet.xlsx") |>
  mutate(code_muni = as.factor(gsub("[^0-9.-]", "", Município)),
         Município = str_squish(str_remove(Município, "[0-9]+")),
         `2010` = as.numeric(as.character(`2010`))) |>
  select(code_muni, municipio = Município, `Taxa de desemprego 2000` = `2000`, `Taxa de desemprego 2010` = `2010`, -`1991`)

# Funcao para limpar String
limpo_address <- function(address) {
  limp_address <- str_replace_all(address, "[[:punct:]]", "")
  limp_address <- toupper(limp_address)
  limp_address <- unaccent(limp_address)
  limp_address <- str_squish(limp_address)
  return(limp_address)
}

# Preparando Para União
independentes <- data.frame(independentes)

independentes <- independentes |>
  mutate(municipio = str_remove_all(municipio, "(SP)")) |>
  mutate(municipio = limpo_address(municipio))

# Banco de Dados com variaveis independentes
final_df_independentes <- independentes |>
  left_join(unemployment_rate2000_2010, by = "municipio")

# Carregando Banco de dados com variaveis independentes censo 2000-2010
final_df_independentes <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\final_df_independentes.xlsx")

#ELABORACAO ATLAS DO DESENVOLVIMENTO HUMANO NO BRASIL PNUD BRASIL IPEA E FJP 2022
#FONTES DADOS DO IBGE E DE REGISTROS ADMINISTRATIVOS CONFORME ESPECIFICADOS NOS METADADOS DISPONIVEIS DISPONIVEIS EM HTTPATLASBRASILORGBRACERVOBIBLIOTECA

# Abrindo Banco de Dados Homicidio
homicide <- read.csv2("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\homicidios2010.csv") %>%
  rename(code_muni = cod, homicidio = valor) |>
  filter(período %in% c(2000, 2009, 2010, 2011)) |>
  mutate(code_muni = as.factor(code_muni))

# Realizando Pivot Wider e selecionando anos 2000, 2009, 2010, 2011
homicide <- homicide |>
  group_by(período) |>
  mutate(row = row_number()) |>
  tidyr::pivot_wider(names_from = período, values_from = homicidio) |>
select(-row, municipio = nome, homicidio2000 = `2000`, homicidio2009 = `2009`, homicidio2010 = `2010`, homicidio2011 = `2011`)

# Organizando Banco de Dados
re_organize <- function(df, municipio_col, code_muni_col, homicide_col) {
  require(dplyr)
  
  municipio_col <- sym(municipio_col)
  code_muni_col <- sym(code_muni_col)
  homicide_col <- sym(homicide_col)
  
  df <- df %>%
    select(!!municipio_col, !!code_muni_col, !!homicide_col) %>%
    distinct(!!municipio_col, !!code_muni_col, !!homicide_col) %>%
    drop_na()
  
  df <- df |>
    mutate(code_muni_original := !!code_muni_col) |>
    mutate(!!code_muni_col := str_sub(!!code_muni_col, start = 1, end = -2))
  
  return(df)
}

# Selecionando anos de 2000, 2009, 2010, 2011
homicidio2000 <- re_organize(homicide, "municipio", "code_muni", "homicidio2000")
homicidio2009 <- re_organize(homicide, "municipio", "code_muni", "homicidio2009")
homicidio2010 <- re_organize(homicide, "municipio", "code_muni", "homicidio2010")
homicidio2011 <- re_organize(homicide, "municipio", "code_muni", "homicidio2011")

# Unindo em Banco de Dados Final
homicidio_2000_2011 <- plyr::join_all(list(homicidio2000, homicidio2009, homicidio2010, homicidio2011), by = c("code_muni", "municipio", "code_muni_original"), type = "left")

# Padronizando Texto municipio
homicidio_2000_2011 <- homicidio_2000_2011 |>
  mutate(municipio = limpo_address(municipio))

# Unindo dados de Homicidio ao Banco de Dados final
final_df_completo <- final_df_independentes |>
  left_join(homicidio_2000_2011, by = "code_muni") |>
  select(municipio = municipio.x, code_muni, code_muni_original, homicidio2000, homicidio2009, homicidio2010, homicidio2011, everything(), -municipio.y)

# Adicionando Binaria Região Metropolitana
metropolitana <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\regioesmetropolitanas.xlsx") |>
  dplyr::select(COD_MUN, NOME_CATMETROPOL)

final_df_completo <- final_df_completo |>
  left_join(metropolitana, by = c("code_muni_original" = "COD_MUN")) 

final_df_completo <- final_df_completo |>
  dplyr::rename(metropolitana_geral = NOME_CATMETROPOL) |>
  mutate(metropolitana = ifelse(!is.na(metropolitana_geral), 1, 0),
         metropolitana_menor = ifelse(metropolitana_geral %in% c("Região Metropolitana de São Paulo", "Região Metropolitana de Campinas", "Região Metropolitana da Baixada Santista"), 1, 0)) 

write.xlsx(final_df_completo, "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\final_df_completo.xlsx")


## Again
limpo_address <- function(address) {
  limp_address <- str_replace_all(address, "[[:punct:]]", "")
  limp_address <- toupper(limp_address)
  limp_address <- unaccent(limp_address)
  limp_address <- str_squish(limp_address)
  return(limp_address)
}

independentes <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\var_independentes2.xlsx", sep.names = " ") |>
  rename(municipio = Territorialidades) |>
  mutate(municipio = str_remove_all(municipio, "(SP)")) |>
  mutate(municipio = limpo_address(municipio)) # 703 

ibge <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\code_muni_ibge.xlsx", sep.names = " ") |>
  select(2, 3, 4, 5) |>
  mutate(municipio = limpo_address(municipio),
         code_muni = as.character(code_muni),
         code_muni7 = as.character(code_muni7)) |>
  filter(uf == "SP")

df_final2 <- ibge |>
  left_join(independentes, by = "municipio")

df_final2 <- df_final2 |>
  left_join(homicidio_2000_2011, by = "code_muni")

metropolitana <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\regioesmetropolitanas.xlsx") |>
  dplyr::select(code_muni7 = COD_MUN, metropolitana_geral = NOME_CATMETROPOL) |>
  mutate(metropolitana = ifelse(!is.na(metropolitana_geral), 1, 0),
         metropolitana_menor = ifelse(metropolitana_geral %in% c("Região Metropolitana de São Paulo", "Região Metropolitana de Campinas", "Região Metropolitana da Baixada Santista"), 1, 0))
  
df_final2 <- df_final2 |>
  left_join(metropolitana, by = "code_muni7")

colnames(df_final2) <- unaccent(colnames(df_final2))  
colnames(df_final2) = gsub("%", "porcentagem", colnames(df_final2))
colnames(df_final2) = gsub(",", "", colnames(df_final2))
colnames(df_final2) = gsub("-", "", colnames(df_final2))


df_final2 <- df_final2 |>
  mutate(mediahomicidio2009_2011 = rowMeans(df_final2[, c("homicidio2009", "homicidio2010", "homicidio2011")], na.rm = TRUE),
         `densidade demografica 2000` = (`Populacao urbana 2000`/`Populacao total 2000`)*100,
         `densidade demografica 2010` = (`Populacao urbana 2010`/`Populacao total 2000`)*100,
         `porcentagem populacao masculina de 15 a 19 anos de idade 2000` = (`Populacao masculina de 15 a 19 anos de idade 2000`/`Populacao total masculina 2000`)*100,
         `porcentagem populacao masculina de 15 a 19 anos de idade 2010` = (`Populacao masculina de 15 a 19 anos de idade 2010`/`Populacao total masculina 2010`)*100,
         `porcentagem populacao masculina 2000` = (`Populacao total masculina 2000`/`Populacao total 2000`)*100,
         `porcentagem populacao masculina 2010` = (`Populacao total masculina 2010`/`Populacao total 2010`)*100)


df_final2 <- df_final2 |>
  left_join(unemployment_rate2000_2010, by = "code_muni")

df_final2 <- df_final2 |> 
  select(code_muni, code_muni7, municipio_f = municipio.x, homicidio2000, homicidio2009, homicidio2010, homicidio2011, mediahomicidio2009_2011, `porcentagem de maes chefes de familia sem fundamental completo e com pelo menos um filho menor de 15 anos de idade 2000`, `porcentagem de maes chefes de familia sem fundamental completo e com pelo menos um filho menor de 15 anos de idade 2010`, `porcentagem de 15 a 24 anos de idade que nao estudam nao trabalham e sao vulneraveis na populacao vulneravel dessa faixa etaria 2010`, `porcentagem de mulheres de 10 a 17 anos de idade que tiveram filhos 2000`, `porcentagem de mulheres de 10 a 17 anos de idade que tiveram filhos 2010`, `porcentagem de 15 a 24 anos de idade que nao estudam nao trabalham e sao vulneraveis na populacao vulneravel dessa faixa etaria 2000`, `porcentagem de 15 a 24 anos de idade que nao estudam nao trabalham e sao vulneraveis na populacao vulneravel dessa faixa etaria 2010`, `porcentagem populacao masculina de 15 a 19 anos de idade 2000`, `porcentagem populacao masculina de 15 a 19 anos de idade 2010`, `porcentagem populacao masculina 2000`, `porcentagem populacao masculina 2010`, `porcentagem de 6 a 17 anos de idade na escola 2000`, `porcentagem de 6 a 17 anos de idade na escola 2010`, `densidade demografica 2000`, `densidade demografica 2010`, `Renda per capita 2000`, `Renda per capita 2010`, `Indice de TheilL 2000`, `Indice de TheilL 2010`, `Indice de Gini 2000`, `Indice de Gini 2010`, `Indice de Atkinson  Renda 2012`, `Indice de Atkinson  Renda 2013`, `Taxa de desocupacao  15 a 17 anos de idade 2000`, `Taxa de desocupacao  15 a 17 anos de idade 2010`, `Taxa de desemprego 2000`, `Taxa de desemprego 2010`, metropolitana, metropolitana_menor, everything(), -municipio, -municipio.y, duplicated, duplicated2, duplicated3)

df_final2 <- df_final2 |>
  select(-duplicated, -duplicated2, -duplicated3)

df_final2 <- df_final2 |>
  drop_na(code_muni) |>
  distinct(code_muni, .keep_all = T)

write.xlsx(df_final2, "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\final_df_completo2.xlsx")
