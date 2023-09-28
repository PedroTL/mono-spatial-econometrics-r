pacman::p_load(tidyverse, openxlsx, geobr, tmap, spdep, janitor, spatialreg, lme4, install = TRUE)

# Modelo simplificado
# Variaveis independentes
# Proporcao de Pobreza - Porcentagem da populacao com renda per capita familiar menor ou igual a metade do salario minimimo por municipio em 2010 - Proxy é porcentagem de extrema pobreza em 2010 (Renda domiciliar per capita até $70 reais mensais 2010)
# Gini Index - Mensuracao para acessar o grau de desigualdade na distribuicao de renda municipio em 2010
# Theil-L Index - Indicador sintetico que mensura desigualdade na distribuicao de renda municipio em 2010
# HDI - Mensuracao composta por tres dimensões de desenvolvimento humano: Longevidade, Educacao e Renda municipio em 2010
# Grau de Urbanizacao - Porcentagem da populacao residindo em areas urbana por municipio em 2010
model_1 <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\final_df_dep_ind.xlsx")

# Criando Varíaveis | (Média Homicidio 2009, 2010, 2011) | Proporcao Pobres? | Grau de Urbanizacao
model_1 <- model_1 |>
  mutate(media_homicidio_2009_2011 = rowMeans(model_1[, c("homicidio_2009", "homicidio_2010", "homicidio_2011")], na.rm = TRUE),
         grau_urbanizacao_2010 = (populacao_urbana_2010/populacao_total_2010),
         percent_de_extremamente_pobres_2010_2 = percent_de_extremamente_pobres_2010/100) |>
  filter(codigo_municipio_completo != "3520400")

# Modelo Inicial OLS (Linear Simples)
fit_1 <- lm(media_homicidio_2009_2011 ~ percent_de_extremamente_pobres_2010_2 + indice_de_gini_2010 + indice_de_theil_l_2010 + idhm_2010 + grau_urbanizacao_2010, model_1)
summary(fit_1)

# Selecionar Variaveis
model_2 <- model_1 %>%
  select(codigo_municipio_completo, municipio_f, homicidio_2009, homicidio_2010, homicidio_2011, percent_de_extremamente_pobres_2010_2, grau_urbanizacao_2010, idhm_2010, indice_de_theil_l_2010, indice_de_gini_2010)

# Escala (Normalizar) 
model_2_scaled <- model_2 |>
  mutate(across(where(is.numeric) & !contains("homicidio"), ~scale(.)))

# Multicolineariedade
corr <- stats::cor(model_2_scaled[, 7:11], use='pairwise.complete.obs')

# Wider
model_2_scaled_wide <- model_2_scaled %>%
  pivot_longer(cols = starts_with("homicidio_"), names_to = "ano", values_to = "homicidio") %>%
  mutate(ano = str_squish(sub("homicidio_", "", ano)))

# Banco de dadosPainel 
model_2_scaled_wide_pannel <-  pdata.frame(model_2_scaled_wide, index = c("codigo_municipio_completo", "ano"))

# Modelo Painel
# Pooled OLS regression model is simply a linear regression model fitted using the OLS
# AIC is used to compare different possible models and determine which one is the best fit for the data.
fit_2 <- plm(homicidio ~ percent_de_extremamente_pobres_2010_2 + grau_urbanizacao_2010 + idhm_2010 + indice_de_theil_l_2010 + indice_de_gini_2010, data = model_2_scaled_wide_pannel, model = "pooling", effect = "twoways")
fit_3 <- plm(homicidio ~ percent_de_extremamente_pobres_2010_2 + grau_urbanizacao_2010 + idhm_2010 + indice_de_theil_l_2010 + indice_de_gini_2010, data = model_2_scaled_wide_pannel, model = "fd")
summary(fit_2)

pFtest(fit_3, fit_2) # Autocorrelacao
plmtest(fit_2, type = "bp", effect = "individual")
