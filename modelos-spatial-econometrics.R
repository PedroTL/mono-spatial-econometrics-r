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
library(openxlsx)


pacman::p_load(tidyverse, openxlsx, geobr, tmap, spdep, janitor, install = TRUE)
setwd("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado")

# 1. Banco de Dados -----------------------------------------------
## 1.1 final_df_completo (Homicidios (2000, 2009, 2010, 2011) ~ Variaveis Independentes) ------------
var_modelo <- read.xlsx("final_df_completo2.xlsx", sep = " ") |>
  distinct(code_muni, .keep_all = T)

## 1.2 ShapeFile estado de SP -------------------------------------
shp_sp <- read_municipality(35, year = 2020) %>%
  mutate(code_muni = as.factor(code_muni),
         name_muni = as.character(name_muni)) %>%
  dplyr::select(code_muni, name_muni, geometry = geom)

## 1.3 Join de var_modelo com shp_sp ------------------------------
shp_sp <- shp_sp |>
  left_join(var_modelo, by = c("code_muni" = "code_muni7")) |> 
  dplyr::filter(!is.na(mediahomicidio2009_2011) | mediahomicidio2009_2011 == 0 | code_muni == "3520400") # 645 mun para 631 (Tirar NA na média) + Ilha Bela

# 2. Analise Topologica -------------------------------------------
## 2.1 Transformando SF em SP -------------------------------------
shp_sp_valid <- as(shp_sp, "Spatial")

### 2.1.1 Verificando se é Valido ---------------------------------
clgeo_IsValid(shp_sp_valid) # Utilizar shp_sp pois é valido

# Removendo Historico
rm(shp_sp_valid)

# 3. Visualizando Estrutura de Dados ----------------------------
style <- tmap_style("col_blind")

tm_shape(shp_sp) + 
  tm_fill("mediahomicidio2009_2011", title = "Taxa de Homicidio Por 100 Mil Habitantes (Quantiles)", style="quantile", palette = "Reds") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Taxa de Homicidio no estado de SP, 20--", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)

# 3. Olhando para os Resíduos e Testando correlacao Espacial -------------------------------------
# Residuals, as we have explained last week, give you an idea of the distance between our observed Y values and the predicted Y values.
# So in essence they are deviations of observed reality from your model. Your regression line or hyperplane is optimised to be the one that best represents your data if those assumptions are met.
# Therefore, residuals are very helpful in diagnosing whether your model is a good representation of reality or not. Most diagnostics of the assumptions for OLS regression rely on exploring the residuals.

# Criando diff variaveis independentes
shp_sp_subset3 <- clean_names(shp_sp)

shp_sp_subset3 <- shp_sp_subset3 |>
  select(everything(), -c("code_muni_y", "name_muni"))

shp_sp_subset_independente <- shp_sp_subset3 |>
  select(code_muni, municipio_f, ends_with(c("2000", "2010")), -c("homicidio_2010", "homicidio_2000"))|>
  sf::st_drop_geometry() |>
  as.data.frame()

shp_sp_subset_independente_long <- shp_sp_subset_independente |> 
  pivot_longer(cols = ends_with(c("2000", "2010")),
               names_to = c("variavel", "ano"),
               names_pattern = "(.*)(\\d{4}$)",
               values_to = "valor")

shp_sp_subset_independente_long <- shp_sp_subset_independente_long |>
  arrange(variavel, ano) |>
  group_by(variavel, code_muni) |>
  mutate(diff_valor = diff(valor),
         lag_valor = lag(valor))

shp_sp_subset_independente_wider <- shp_sp_subset_independente_long |>
  pivot_wider(
    names_from = c(variavel, ano), 
    values_from = c(valor, lag_valor, diff_valor))

shp_sp_subset_independente_diff <- shp_sp_subset_independente_wider |>
  select(code_muni, municipio_f, starts_with("diff") & ends_with("2010"))
names(shp_sp_subset_independente_diff) <- gsub("_2010", "_2010_2000", names(shp_sp_subset_independente_diff))

shp_sp_subset_independente_diff <- shp_sp_subset_independente_diff |>
  left_join(shp_sp_subset3, by = "code_muni")

shp_sp_subset_independente_diff <- shp_sp_subset_independente_diff |>
  mutate(metropolitana = ifelse(is.na(metropolitana), 0, metropolitana),
         metropolitana_menor = ifelse(is.na(metropolitana_menor), 0, metropolitana_menor))

shp_sp <- shp_sp |>
  left_join(shp_sp_subset_independente_diff, by = "code_muni")

shp_sp_subset_f <- shp_sp

shp_sp_subset_f <- shp_sp_subset_f |>
  select(code_muni, name_muni, mediahomicidio2009_2011, densidade_demografica_2010, indice_de_gini_2010, indice_de_theil_l_2010, porcentagem_de_15_a_24_anos_de_idade_que_nao_estudam_nao_trabalham_e_sao_vulneraveis_na_populacao_vulneravel_dessa_faixa_etaria_2010, porcentagem_de_6_a_17_anos_de_idade_na_escola_2010, porcentagem_de_maes_chefes_de_familia_sem_fundamental_completo_e_com_pelo_menos_um_filho_menor_de_15_anos_de_idade_2010, porcentagem_de_mulheres_de_10_a_17_anos_de_idade_que_tiveram_filhos_2010, porcentagem_populacao_masculina_2010, renda_per_capita_2010, taxa_de_desemprego_2010, taxa_de_desocupacao_15_a_17_anos_de_idade_2010, geometry = geometry.x, metropolitana, metropolitana_menor) |>
  drop_na() |>
  filter(code_muni != 3520400)


## 3.1 Análise de regressão NÃO Espacial em dados espaciais (Observar os Resíduos) ---------------
fit_1 <- lm(mediahomicidio2009_2011 ~ diff_valor_densidade_demografica__2010_2000 + diff_valor_indice_de_gini__2010_2000 + diff_valor_indice_de_theil_l__2010_2000 + diff_valor_porcentagem_de_15_a_24_anos_de_idade_que_nao_estudam_nao_trabalham_e_sao_vulneraveis_na_populacao_vulneravel_dessa_faixa_etaria__2010_2000 + diff_valor_porcentagem_de_6_a_17_anos_de_idade_na_escola__2010_2000 + diff_valor_porcentagem_de_maes_chefes_de_familia_sem_fundamental_completo_e_com_pelo_menos_um_filho_menor_de_15_anos_de_idade__2010_2000 + diff_valor_porcentagem_de_mulheres_de_10_a_17_anos_de_idade_que_tiveram_filhos__2010_2000 + diff_valor_porcentagem_populacao_masculina__2010_2000 + diff_valor_renda_per_capita__2010_2000 + diff_valor_taxa_de_desemprego__2010_2000 + diff_valor_taxa_de_desocupacao_15_a_17_anos_de_idade__2010_2000, shp_sp_subset_independente_diff)
summary(fit_1)

fit_2 <- lm(mediahomicidio2009_2011 ~ densidade_demografica_2010 + indice_de_gini_2010 + indice_de_theil_l_2010 + porcentagem_de_15_a_24_anos_de_idade_que_nao_estudam_nao_trabalham_e_sao_vulneraveis_na_populacao_vulneravel_dessa_faixa_etaria_2010 + porcentagem_de_6_a_17_anos_de_idade_na_escola_2010 + porcentagem_de_maes_chefes_de_familia_sem_fundamental_completo_e_com_pelo_menos_um_filho_menor_de_15_anos_de_idade_2010 + porcentagem_de_mulheres_de_10_a_17_anos_de_idade_que_tiveram_filhos_2010 + porcentagem_populacao_masculina_2010 + renda_per_capita_2010 + taxa_de_desemprego_2010 + taxa_de_desocupacao_15_a_17_anos_de_idade_2010, data = shp_sp_subset_f) 
summary(fit_2)

fit_3 <- lm(mediahomicidio2009_2011 ~ densidade_demografica_2000 + indice_de_gini_2000 + indice_de_theil_l_2000 + porcentagem_de_15_a_24_anos_de_idade_que_nao_estudam_nao_trabalham_e_sao_vulneraveis_na_populacao_vulneravel_dessa_faixa_etaria_2000 + porcentagem_de_6_a_17_anos_de_idade_na_escola_2000 + porcentagem_de_maes_chefes_de_familia_sem_fundamental_completo_e_com_pelo_menos_um_filho_menor_de_15_anos_de_idade_2000 + porcentagem_de_mulheres_de_10_a_17_anos_de_idade_que_tiveram_filhos_2000 + porcentagem_populacao_masculina_2000 + renda_per_capita_2000 + taxa_de_desemprego_2000 + taxa_de_desocupacao_15_a_17_anos_de_idade_2000, data = shp_sp_subset_independente_diff) 
summary(fit_3)


### 3.1.1 Extraindo Resíduo do Modelo ------------------------------------------------------
# In those cases where the residual is negative this is telling us that the observed value is lower than the predicted (that is, our model is overpredicting the level of homicide for that observation) 
# when the residual is positive the observed value is higher than the predicted (that is, our model is underpredicting the level of homicide for that observation).
shp_sp_subset_f$res_fit2 <- residuals(fit_2)

### 3.1.2 Extraindo o Valor Previsto ------------------------------------------------------
shp_sp_subset_f$fitted_fit2 <- fitted(fit_2)

# With spatial data one useful thing to do is to look at any spatial patterning in the distribution of the residuals. Notice that the residuals are the difference between the observed values for homicide and the predicted values for homicide
# so you want your residual to NOT display any spatial patterning. If, on the other hand, your model displays a patterning in the areas of the study region where it predicts badly, then you may have a problem. 
# This is telling you that your model is not a good representation of the social phenomena you are studying across the full study area: there is systematically more distortion in some areas than in others.

# We are going to produce a choropleth map for the residuals, but we will use a common classification method we haven’t covered yet: standard deviations. 
# Standard deviation is a statistical technique that is based on how much the data differs from the mean. First, you measure the mean and standard deviation for your data. Then, each standard deviation becomes a class in your choropleth maps.
# In order to do that we will compute the mean and the standard deviation for the variable we want to plot and break the variable according to these values. The following code creates a new variable in which we will express the residuals in terms of standard deviations away from the mean. 
# So, for each observation, we subtract the mean and divide by the standard deviation. Remember, this is exactly what the scale function does, which we have introduced in week 7:
shp_sp_subset_f$sd_breaks <- scale(shp_sp_subset_f$res_fit2)[,1] # because scale is made for matrices, we just need to get the first column using [,1]

# this is equal to (ncovr_sf$res_fit1 - mean(ncovr_sf$res_fit1)) / sd(ncovr_sf$res_fit1)
summary(shp_sp_subset_f$sd_breaks)

# Next we use a new style, fixed, within the tm_fill function. When we break the variable into classes using the fixed argument we need to specify the boundaries of the classes. We do this using the breaks argument. 
# In this case we are going to ask R to create 7 classes based on standard deviations away from the mean. Remember that a value of 1 would be 1 standard deviation (s.d.) higher than the mean, and -1 would be one s.d. lower. If we assume normal distribution, then 68% of all counties should lie within the middle band from -1 to +1 s.d. 
my_breaks <- c(-14, -3, -2, -1, 1, 2, 3, 14)
tm_shape(shp_sp_subset_f) + 
  tm_fill("sd_breaks", title = "Residuals", style = "quantile", palette = "-RdBu", midpoint = 0) +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("left", "bottom"), legend.hist.size = 0.3, legend.title.size = 0.8)

# Notice the spatial patterning of areas of over-prediction (negative residuals, or blue tones) and under-prediction (positive residuals, or brown tones). This visual inspection of the residuals is telling you that spatial autocorrelation may be present here

## 3.2 Autocorrelacao Espacial ------------------------------------------------------------
# This should give you an idea of the distribution of connectedness across the data, with counties having on average nearly 6 neighbours. Now we can generate the row-standardised spatial weight matrix and the Moran Scatterplot.
### 3.2.1 Criando uma matriz de vizinhanca (Criterio da Rainha) -------------
shp_sp_subset_f_sp <- as(shp_sp_subset_f, "Spatial")

w <- poly2nb(shp_sp_subset_f, row.names=shp_sp_subset_f$code_muni)
print(w) # Vizinhanca (Rainha)

### 3.2.1 Moran Test I -----------------------------------------------------------------
wm <- nb2mat(w, style='B')
rwm <- mat2listw(wm, style='W')

# We obtain the Moran’s test for regression residuals using the function lm.morantest() as below. It is important to realize that the Moran’s I 
# test statistic for residual spatial autocorrelation takes into account 
# the fact that the variable under consideration is a residual, computed from a regression. 
# The usual Moran’s I test statistic does not. It is therefore incorrect to simply apply a Moran’s I test to the residuals from the 
# regression without correcting for the fact that these are residuals.
lm.morantest(fit_2, rwm, alternative="two.sided")

# You will notice we obtain a statistically significant value for Moran’s I. The value of the Moran’s I test is not too high, 
# but we still need to keep it in mind. If we diagnose that spatial autocorrelation is an issue, that is, 
# that the errors (the residuals) are related systematically among themselves, then we have a problem and need to
# use a more appropriate approach: a spatial regression model.

# 4. What to do Now? -----------------------------------------------------------------
# If the test is significant (as in this case), then we possibly need to think of a more suitable model to represent our data: 
# a spatial regression model. Remember spatial dependence means that (more typically) there will be areas of spatial clustering for 
# the residuals in our regression model. 
# So our predicted line (or hyperplane) will systematically under-predict or over-predict in areas that are close to each other. 
# That’s not good. We want a better model that does not display any spatial clustering in the residuals.

# There are two general ways of incorporating spatial dependence in a regression model, through what we called a spatial error model 
# or by means of a spatially lagged model. 
# There are spdep functions that provides us with some tools to help us make a decision as to which of these two is most appropriate: 
# the Lagrange Multiplier tests.

# The difference between these two models is both technical and conceptual. 
# The spatial error model treats the spatial autocorrelation as a nuisance that needs to be dealt with. 
# A spatial error model basically implies that the:
  # “spatial dependence observed in our data does not reflect a truly spatial process, but merely the geographical clustering of the sources of the behaviour of interest. 
  # For example, citizens in adjoining neighbourhoods may favour the same (political) candidate not because they talk to their neighbors, 
  # but because citizens with similar incomes tend to cluster geographically, and income also predicts vote choice. 
  # Such spatial dependence can be termed attributional dependence” (Darmofal, 2015: 4)

# The spatially lagged model, on the other hand, incorporates spatial dependence explicitly by adding a “spatially lagged” variable y on 
# the right hand side of our regression equation. Its distinctive characteristic is that it includes a spatially lagged “dependent” variable 
# among the explanatory factors.
# It’s basically explicitly saying that the values of y in the neighbouring areas of observation n~i is an important predictor 
# of y on each individual area n~i. This is one way of saying that the spatial dependence may be produced by a spatial process 
# such as the diffusion of behaviour between neighboring units:
  # “If so the behaviour is likely to be highly social in nature, and understanding the interactions between interdependent 
  # units is critical to understanding the behaviour in question. For example, citizens may discuss politics across adjoining neighbours such that an increase in support for a candidate 
  # in one neighbourhood directly leads to an increase in support for the candidate in adjoining neighbourhoods” (Darmofal, 2015: 4)

# 5. Spatial Regimes -----------------------------------------------------------------
# Before we proceed to a more detailed description of these two models it is important that we examine another aspect of our model that also links to geography. Remember that when we brought up our data into R, we decided to test for the presence of an interaction. We looked at whether the role of unemployment was different in Southern and Northern states. 
# We found that this interaction was indeed significant. Unemployment had a more significant effect in Southern than in Northern states. This was particularly obvious during the 1970s, when unemployment did not affect homicide rates in the Northern states, but it did lead to a decrease in homicide in the Southern states.

# We could have attempted to test other interaction effects between some of our other predictors and their geographical location in the South or the North. But we did not.

# If you have read the Ballen et al. (2001) paper that we are replicating in the lab last week and this week, you will have noticed that they decided that they needed to run separate models for the South and the North. This kind of situation, where sub-regions seem to display different patterns is often alluded with the name of spatial regimes. 
# In the context of regression analysis, spatial regimes relate to the possibility that we may need to split our data into two (or more) sub-regions in order to run our models, because we presume that the relationship of the predictors to the outcome may play out differently in these sub-regions (spatial regimes).

# So how can we assess whether this is an issue in our data? As with many other diagnostics of regression, you may want to start by looking at your residuals.

## 5.1 Acessando Regimes Espaciais Utilizando os Residuos ----------------------------
# Vendo a distribuicao e diferenca em regiões metropolitanas
ggplot(shp_sp_subset_f, aes(x = res_fit2, colour = as.factor(metropolitana_menor))) + 
  geom_density()

# What is this telling you about the predicted values that result from our model? (Remember what a residual is: the difference between the observed values and the predicted values).
# Sufficient to say that, as Bollen et al. (2001), we are going to split our analysis and run them separately for the metropolitan and the non metropolitan municipalities. We have covered the filter() 
# function from dplyr to split datasets based on values of a variable.
metrop_shp_sp_subset_f <- subset(shp_sp_subset_f, metropolitana == 1)
n_metrop_shp_sp_subset_f <- subset(shp_sp_subset_f, metropolitana_menor == 0)

## 5.2 Lagrange multipliers
# The Moran’s I test statistic has high power against a range of spatial alternatives. However, it does not provide much help in terms of which alternative model would be most appropriate. 
# The Lagrange Multiplier test statistics do allow a distinction between spatial error models and spatial lag models.

# In order to practice their computation and interpretation, let’s run two separate OLS regression models (one for the metropolitan and non metropolitan)
# We have split the data in two, so that means that before we do this we need to create new files for the spatial weight matrix: in particular we will create one using first order queen criteria.

# vizinhanca rainha para regioes metropolitanas
w_metrop <- poly2nb(n_metrop_shp_sp_subset_f, row.names=n_metrop_shp_sp_subset_f$code_muni)
print(w_metrop) # Vizinhanca (Rainha)

wm_n_metrop <- nb2mat(w_metrop, style='B')
rwm_n_metrop <- mat2listw(wm_metrop, style='W')

fit_2_metrop <- lm(mediahomicidio2009_2011 ~ densidade_demografica_2010 + indice_de_gini_2010 + indice_de_theil_l_2010 + porcentagem_de_15_a_24_anos_de_idade_que_nao_estudam_nao_trabalham_e_sao_vulneraveis_na_populacao_vulneravel_dessa_faixa_etaria_2010 + porcentagem_de_6_a_17_anos_de_idade_na_escola_2010 + porcentagem_de_maes_chefes_de_familia_sem_fundamental_completo_e_com_pelo_menos_um_filho_menor_de_15_anos_de_idade_2010 + porcentagem_de_mulheres_de_10_a_17_anos_de_idade_que_tiveram_filhos_2010 + porcentagem_populacao_masculina_2010 + renda_per_capita_2010 + taxa_de_desemprego_2010 + taxa_de_desocupacao_15_a_17_anos_de_idade_2010, data = n_metrop_shp_sp_subset_f) 
summary(fit_2_metrop)

lm.morantest(fit_2_metrop, rwm_n_metrop, alternative = "two.sided")

#  OLS regression won’t do. In order to decide whether to fit a spatial error or a spatially lagged model we need to run the Lagrange Multipliers.
### 5.2.1 Lagrange multiplier test
# Both Lagrange multiplier tests (for the error and the lagged models, LMerr and LMlag respectively), as well as their robust forms (RLMerr and RLMLag, 
# also respectively) are included in the lm.LMtests function. Again, a regression object and a spatial listw object must be passed as arguments.
# In addition, the tests must be specified as a character vector (the default is only LMerror), using the c( ) operator (concatenate), as illustrated below.
lm.LMtests(fit_2_metrop, rwm_metrop, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

# How do we interpret the Lagrange Multipliers? First we look at the standard ones (LMerr and LMlag). 
# If both are below the .05 level this means we need to have a look at the robust version of these tests (Robust LM)

# If the non-robust version is not significant, the mathematical properties of the robust tests may not hold, so we don’t look at them in those scenarios. 
# It is fairly common to find that both the lag (LMlag) and the error (LMerr) non-robust LM are significant. If only one of them are: problem solved. 
# We would choose a spatial lag or a spatial error model according to this (i.e., if the lag LM was significant and the error LM was not we would run a spatial lag model or viceversa).

# “When both LM test statistics reject the null hypothesis, proceed to the bottom part of the graph and consider the Robust forms of the test statistics.
# Typically, only one of them will be significant, or one will be orders of magnitude more significant than the other (e.g., p < 0.00000 compared to p < 0.03). 
# In that case the decision is simple: estimate the spatial regression model matching the (most) significant” robust “statistic. 
# In the rare instance that both would be highly significant, go with the model with the largest value for the test statistic. However, in this situation, some caution is needed, since there may be other sources of misspecification.
# One obvious action to take is to consider the results for different spatial weight and/or change the basic (i.e., not the spatial part) specification of the model. there are also rare instances where neither of the Robust LM test statistics are significant.
# In those cases, more serious specification problems are likely present and those should be addressed first.”
