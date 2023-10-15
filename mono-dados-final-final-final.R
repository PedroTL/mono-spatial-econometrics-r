# percent_pop_extremamente_pobre_2010 = Proporção dos indivíduos com renda domiciliar per capita igual ou inferior a R$ 70,00 mensais, em reais de agosto de 2010. O universo de indivíduos é limitado àqueles que vivem em domicílios particulares permanentes.
# gini_2010 = Mede o grau de desigualdade existente na distribuição de indivíduos segundo a renda domiciliar per capita. Seu valor varia de 0, quando não há desigualdade (a renda domiciliar per capita de todos os indivíduos tem o mesmo valor), a 1, quando a desigualdade é máxima (apenas um indivíduo detém toda a renda).O universo de indivíduos é limitado àqueles que vivem em domicílios particulares permanentes.
# theil_2010 = Mede a desigualdade na distribuição de indivíduos segundo a renda domiciliar per capita, excluídos aqueles com renda domiciliar per capita nula. É o logaritmo da razão entre as médias aritmética e geométrica da renda domiciliar per capita dos indivíduos, sendo nulo quando não existir desigualdade de renda entre eles e tendente ao infinito quando a desigualdade tender ao máximo.
# idhm_2010 = Índice de Desenvolvimento Humano Municipal. Média geométrica dos índices das dimensões Renda, Educação e Longevidade, com pesos iguais.
# grau_urbanizacao_2010 - Porcentagem da populacao residindo em areas urbana por municipio em 2010 (pop_total_urbana_2010/pop_total_2010)
# idhm_renda_2010 - Índice da dimensão Renda que é um dos 3 componentes do IDHM. É obtido a partir do indicador Renda per capita, através da fórmula: [ln (valor observado do indicador) - ln (valor mínimo)] / [ln (valor máximo) - ln (valor mínimo)], onde os valores mínimo e máximo são R$ 8,00 e R$ 4.033,00 (a preços de agosto de 2010). 
# percent_pop_pobre - Proporção dos indivíduos com renda domiciliar per capita igual ou inferior a R$ 140,00 mensais, em reais de agosto de 2010. O universo de indivíduos é limitado àqueles que vivem em domicílios particulares permanentes. 

# Var adicionais
# percent_criancas_6a14_fora_escola = Razão entre as crianças de 6 a 14 anos que não frequenta a escola e o total de crianças nesta faixa etária multiplicado por 100.
# percent_mulher_15a17_com_um_filho_2010 = Razão entre as mulheres de 15 a 17 anos de idade que tiveram filhos e o total de mulheres nesta faixa etária multiplicado por 100.
# percent_pop_homem_15a29_2010 = Percentual de populacão masculina entre 19 e 29 anos
# percent_desocupacao_18_mais_2010 = Percentual da população economicamente ativa (PEA) nessa faixa etária que estava desocupada, ou seja, que não estava ocupada na semana anterior à data do Censo mas havia procurado trabalho ao longo do mês anterior à data dessa pesquisa.

# Var para calculos
# pop_total_2010 = População residente total
# pop_total_urbana_2010 = População residente na área urbana
# homem_15a19_2010 = População masculina entre 15 e 19 anos 
# homem_20a24_2010 = População masculina entre 20 e 24 anos 
# homem_25a29_2010 = População masculina entre 25 e 29 anos 

# Variáveis Independentes
independentes_simples <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\Atlas 2013_municipal, estadual e Brasil.xlsx", sheet = 2) |>
  filter(UF %in% c(35, 31,  33, 32) & ANO == 2010) |>
  dplyr::select(UF, Codmun6, Codmun7, municipio = Município, 
         percent_pop_extremamente_pobre_2010 = PIND,
         idhm_2010 = IDHM,
         gini_2010 = GINI, 
         theil_2010 = THEIL, 
         percent_mulher_15a17_com_um_filho_2010 = T_M15A17CF, 
         percent_criancas_6a14_fora_escola_2010 = T_FORA6A14, 
         percent_desocupacao_18_mais_2010 = T_DES18M, 
         pop_total_2010 = pesotot, 
         pop_total_urbana_2010 = pesourb, 
         homem_15a19_2010 = HOMEM15A19, 
         homem_20a24_2010 = HOMEM20A24, 
         homem_25a29_2010 = HOMEM25A29, 
         idhm_renda_2010 = IDHM_R, 
         percent_pop_pobre_2010 = PMPOB
         ) 

# Variavel Dependente homicidio 100 mil hab
homicide <- read.csv2("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\homicidios2010.csv") |>
  rename(Codmun7 = cod, homicidio = valor, ano = período, municipio = nome) |>
  filter(ano %in% c(2009, 2010, 2011, 2016, 2017, 2018, 2019))

homicide2 <- homicide |>
  group_by(ano) |>
  mutate(row = row_number()) |>
  tidyr::pivot_wider(names_from = ano, values_from = homicidio) |>
  dplyr::select(-row, municipio, homicidio_2009 = `2009`, homicidio_2010 = `2010`, homicidio_2011 = `2011`, homicidio_2016 = `2016`, homicidio_2017 = `2017`, homicidio_2018 = `2018`, homicidio_2019 = `2019`)

re_organize <- function(df, municipio_col, code_muni_col, homicide_col) {
  require(dplyr)
  
  municipio_col <- sym(municipio_col)
  code_muni_col <- sym(code_muni_col)
  homicide_col <- sym(homicide_col)
  
  df <- df %>%
    dplyr::select(!!municipio_col, !!code_muni_col, !!homicide_col) %>%
    distinct(!!municipio_col, !!code_muni_col, !!homicide_col) %>%
    drop_na()
  
  return(df)
}

homicidio2009 <- re_organize(homicide2, "municipio", "Codmun7", "homicidio_2009")
homicidio2010 <- re_organize(homicide2, "municipio", "Codmun7", "homicidio_2010")
homicidio2011 <- re_organize(homicide2, "municipio", "Codmun7", "homicidio_2011")
homicidio2016 <- re_organize(homicide2, "municipio", "Codmun7", "homicidio_2016")
homicidio2017 <- re_organize(homicide2, "municipio", "Codmun7", "homicidio_2017")
homicidio2018 <- re_organize(homicide2, "municipio", "Codmun7", "homicidio_2018")
homicidio2019 <- re_organize(homicide2, "municipio", "Codmun7", "homicidio_2019")

homicide_final <- plyr::join_all(list(homicidio2009, homicidio2010, homicidio2011, homicidio2016, homicidio2017, homicidio2018, homicidio2019), by = c("Codmun7", "municipio"), type = "left")
homicide_final <- homicide_final |>
  mutate(Codmun7 = as.numeric(Codmun7))

rm(homicidio2009, homicidio2010, homicidio2011, homicidio2016, homicidio2017, homicidio2018, homicidio2019)
rm(homicide, homicide2)

independentes_final <- independentes_simples |>
  left_join(homicide_final, by = "Codmun7")

independentes_final <- independentes_final |>
  rename(municipio = municipio.x, municipio_ipea = municipio.y)

independentes_final <- independentes_final |>
  mutate(percent_pop_extremamente_pobre_2010 = percent_pop_extremamente_pobre_2010 / 100,
         percent_mulher_15a17_com_um_filho_2010 = percent_mulher_15a17_com_um_filho_2010 / 100,
         percent_criancas_6a14_fora_escola_2010 = percent_criancas_6a14_fora_escola_2010 / 100,
         percent_desocupacao_18_mais_2010 = percent_desocupacao_18_mais_2010 / 100,
         percent_pop_pobre_2010 = percent_pop_pobre_2010 / 100,
         grau_urbanizacao_2010 = pop_total_urbana_2010 / pop_total_2010,
         percent_pop_homem_15a29_2010 = rowSums(across(starts_with("homem")), na.rm = TRUE) / pop_total_2010,
         pop_total_dividido_mil_2010 = pop_total_2010 / 1000,
         homicidio_100mil_2010 = (homicidio_2010 / (pop_total_2010/100000)),
         gini_scaled_2010 = scale(gini_2010),
         theil_scaled_2010 = scale(theil_2010),
         idhm_scaled_2010 = scale(idhm_2010),
         idhm_renda_scaled_2010 = scale(idhm_renda_2010),
         log_percent_pop_extremamente_pobre_2010 = log(percent_pop_extremamente_pobre_2010),
         log_percent_pop_pobre_2010 = log(percent_pop_pobre_2010),
         log_percent_pop_extremamente_pobre_2010 = ifelse(is.infinite(log_percent_pop_extremamente_pobre_2010), NA, log_percent_pop_extremamente_pobre_2010),
         log_percent_pop_pobre_2010 = ifelse(is.infinite(log_percent_pop_pobre_2010), NA, log_percent_pop_pobre_2010))

# Salvando homicidio (contagem) e var independentes
write.xlsx(independentes_final, "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep_count_2.xlsx")

##########################

# Empirical bayesian smoother para contagem homicidios
# Define observed number of cases 
bd_final_ind_dep_count<- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep.xlsx")

# To compute the expected number of cases through indirect standardisation we need the overall incidence ratio
overall_incidence_ratio = sum(bd_final_ind_dep_count$homicidio_2010, na.rm = TRUE)/sum(bd_final_ind_dep_count$pop_total_2010, na.rm = TRUE)

# The expected number of cases can then be obtained by multiplying the overall incidence rate by the population
bd_final_ind_dep_count <- bd_final_ind_dep_count |>
  mutate(observed_2010 = homicidio_2010,
         expected_2010 = pop_total_2010 * overall_incidence_ratio,
         raw_risk = observed_2010 / expected_2010,
         homicidio_2010_100mil = (homicidio_2010 / (pop_total_2010/100000)))

bd_final_ind_dep_count2 <- bd_final_ind_dep_count |>
  mutate(observed_2010 = replace_na(observed_2010, 0),
         expected_2010 = replace_na(expected_2010), 0)

# Estimate the smooth relative risk
res <- empbaysmooth(bd_final_ind_dep_count2$observed_2010, bd_final_ind_dep_count2$expected_2010)
bd_final_ind_dep_count2$homicidio_rate_sm_2010 <- res$smthrr

shp <- read_municipality(code_muni = "all", year = 2020) %>%
  mutate(Codmun7 = as.numeric(code_muni),
         municipio = as.character(name_muni)) %>%
  dplyr::select(code_state, Codmun7, municipio, geometry = geom) |>
  filter(code_state %in% c(35, 31,  33, 32)) 

shp <- shp |>
  left_join(bd_final_ind_dep_count2, by = "Codmun7")

shp <- shp |>
  mutate(homicidio_2010 = replace_na(homicidio_2010, 0),
         pop_total_2010 = replace_na(pop_total_2010, 0))

shp_sp <- as(shp, "Spatial")

w_nb <- poly2nb(shp_sp, row.names = shp_sp$Codmun7)
eb2 <- EBlocal(shp$homicidio_2010, shp$pop_total_2010, w_nb, zero.policy = FALSE)

bd_final_ind_dep_count2 <- bd_final_ind_dep_count2 |>
  mutate(homicidio_rate_EBSL_2010 = eb2$est * 100000)

# Visualizando
tm_shape(shp,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) +
  tm_fill("homicidio_2010_100mil", style = "quantile", title = "raw_count", palette = "Reds") +
  tm_layout(legend.position = c("left", "BOTTOM"),
            legend.title.size = 0.8,
            legend.text.size = 0.5)

tm_shape(shp,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) +
  tm_fill("homicidio_rate_EBSL_2010", style = "quantile", title = "raw_count", palette = "Reds") +
  tm_layout(legend.position = c("left", "BOTTOM"),
            legend.title.size = 0.8,
            legend.text.size = 0.5)

# Testes Modelo
fit_1 <- lm(homicidio_rate_EBSL_2010 ~ percent_pop_extremamente_pobre_2010 + gini_2010 + theil_2010 + idhm_2010 + grau_urbanizacao_2010, shp)
summary(fit_1)

fit_2 <- lm(homicidio_2010_100mil ~ percent_pop_extremamente_pobre_2010 + gini_2010 + theil_2010 + idhm_2010 + grau_urbanizacao_2010, shp)
summary(fit_2)

#######################
# A687 - Existe no munícipio: Delegacia de Homícidio (Sim ou Não)
# A708 - Efetivo total de PM

pm <- read_ods("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\munic_2014_pm.ods", sheet = 8) |>
  dplyr::select(Codmun7 = A1, deleg_pm_mun_binaria_2014 = A687, efetivo_total_pm_2014 = A708) |>
  mutate(deleg_pm_mun_binaria_2014 = as.factor(ifelse(deleg_pm_mun_binaria_2014 == "Sim", 1, 0)),
         efetivo_total_pm_2014 = ifelse(efetivo_total_pm_2014 == "-", NA, efetivo_total_pm_2014))

metropolitana <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\regioesmetropolitanas.xlsx") |>
  filter(COD_UF %in% c(35, 31,  33, 32)) |>
  dplyr::select(Codmun7 = COD_MUN, regiao_metrop = LABEL_CATMETROPOL) |>
  mutate(metrop_binaria = 1,
         Codmun7 = as.numeric(Codmun7)) |>
  distinct(Codmun7, .keep_all = T)

pop_2014 <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\pop_total_2014.xlsx") |>
  dplyr::select(Codmun7, pop_total_2014) |>
  mutate(Codmun7 = as.numeric(Codmun7))

bd_final_ind_dep_count2 <-bd_final_ind_dep_count2 |>
  left_join(pop_2014)

bd_final_ind_dep_count2 <-bd_final_ind_dep_count2 |>
  left_join(metropolitana)

bd_final_ind_dep_count2 <-bd_final_ind_dep_count2 |>
  mutate(metrop_binaria = ifelse(is.na(metrop_binaria), 0, metrop_binaria),
         pop_total_dividido_mil_2014 = pop_total_2014 / 1000,
         pop_pm_por_100_mil_habit = (efetivo_total_pm_2014 / (pop_total_2014/100000)),
         pop_pm_por_1000_habit = (efetivo_total_pm_2014 / pop_total_2014) * 1000)
