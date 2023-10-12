pacman::p_load(tidyverse, openxlsx, geobr, tmap, spdep, janitor, spatialreg, lme4, plm, tmap, 
               DCluster, install = TRUE)

# Var Basicas
# percent_pop_extremamente_pobre_2010 = Proporção dos indivíduos com renda domiciliar per capita igual ou inferior a R$ 70,00 mensais, em reais de agosto de 2010. O universo de indivíduos é limitado àqueles que vivem em domicílios particulares permanentes.
# gini_2010 = Mede o grau de desigualdade existente na distribuição de indivíduos segundo a renda domiciliar per capita. Seu valor varia de 0, quando não há desigualdade (a renda domiciliar per capita de todos os indivíduos tem o mesmo valor), a 1, quando a desigualdade é máxima (apenas um indivíduo detém toda a renda).O universo de indivíduos é limitado àqueles que vivem em domicílios particulares permanentes.
# theil_2010 = Mede a desigualdade na distribuição de indivíduos segundo a renda domiciliar per capita, excluídos aqueles com renda domiciliar per capita nula. É o logaritmo da razão entre as médias aritmética e geométrica da renda domiciliar per capita dos indivíduos, sendo nulo quando não existir desigualdade de renda entre eles e tendente ao infinito quando a desigualdade tender ao máximo.
# idhm_2010 = Índice de Desenvolvimento Humano Municipal. Média geométrica dos índices das dimensões Renda, Educação e Longevidade, com pesos iguais.
# grau_urbanizacao_2010 - Porcentagem da populacao residindo em areas urbana por municipio em 2010 (pop_total_urbana_2010/pop_total_2010)

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

# Abrindo Banco de Dados
model_1 <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep.xlsx") |>
  filter(Codmun7 != "3520400") # Removendo Ilha

# Modelo Inicial OLS (Linear Simples)
fit_1 <- lm(homicidio_2010 ~ percent_pop_extremamente_pobre_2010 + gini_2010 + theil_2010 + idhm_2010 + grau_urbanizacao_2010, model_1)
summary(fit_1)

# Selecionar Variaveis
model_2 <- model_1 %>%
  select(Codmun7, Codmun6, municipio, homicidio_2009, homicidio_2010, homicidio_2011, homicidio_2016, homicidio_2017, homicidio_2018, homicidio_2019, percent_pop_extremamente_pobre_2010, gini_2010, theil_2010, idhm_2010, grau_urbanizacao_2010)

# Escala (Normalizar) 
model_2_scaled <- model_2 |>
  mutate(across(where(is.numeric) & !contains("homicidio") & !contains("Codmun"), ~scale(.)))

# Multicolineariedade
corr <- stats::cor(model_2_scaled[, 11:15], use = 'pairwise.complete.obs')
corr

# Wider
model_2_scaled_wide <- model_2_scaled %>%
  pivot_longer(cols = starts_with("homicidio_"), names_to = "ano", values_to = "homicidio") %>%
  mutate(ano = str_squish(sub("homicidio_", "", ano)))

# Banco de dadosPainel 
model_2_scaled_wide_pannel <- pdata.frame(model_2_scaled_wide, index = c("Codmun7", "ano"))

# Modelo Painel
# Pooled OLS regression model is simply a linear regression model fitted using the OLS
# AIC is used to compare different possible models and determine which one is the best fit for the data.
model_2_scaled_wide_pannel <- model_2_scaled_wide_pannel |>
  filter(ano %in% c(2016, 2017, 2018, 2019))

fit_1_pannel <- plm(homicidio ~ log(percent_pop_extremamente_pobre_2010) + gini_2010 + theil_2010 + grau_urbanizacao_2010 + idhm_2010, model_2_scaled_wide_pannel, model = "pooling", effect = "twoways", na.action = na.exclude)
summary(fit_1_pannel)

fit_2_pannel <- plm(homicidio ~ percent_pop_extremamente_pobre_2010 + gini_2010 + theil_2010 + grau_urbanizacao_2010 + idhm_2010, data = model_2_scaled_wide_pannel, model = "pooling", effect = "twoways", na.action = na.exclude)
summary(fit_2_pannel)

# 
shp <- read_municipality(code_muni = "all", year = 2020) %>%
  mutate(Codmun7 = as.numeric(code_muni),
         municipio = as.character(name_muni)) %>%
  dplyr::select(code_state, Codmun7, municipio, geometry = geom) |>
  filter(code_state %in% c(35, 31,  33, 32)) 

shp <- shp |>
  left_join(model_1, by = "Codmun7")

st_bbox(shp)

tm_shape(shp,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) +
  tm_fill(col = "idhm_2010", style = "quantile", palette = "Reds") +
  tm_grid(lwd = 0.3,
          alpha = 0.5)
