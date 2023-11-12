pacman::p_load(dplyr, openxlsx, geobr, tmap, spdep, janitor, spatialreg, lme4, plm, tmap, 
               DCluster, sp, raster, tidyr, install = TRUE)

#### 0. Abrindo Banco de Dados, Homicidio (Contagem 2010) e variáveis independentes ####
bd_final_ind_dep_count <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep_count_2_final.xlsx")
bd_final_ind_dep_count <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep_count_2.xlsx")

#### 1. Bayesian Smooth #### 
overall_incidence_ratio = sum(bd_final_ind_dep_count$homicidio_2010, na.rm = TRUE)/sum(bd_final_ind_dep_count$pop_total_2010, na.rm = TRUE)

bd_final_ind_dep_count <- bd_final_ind_dep_count |>
  mutate(observed_2010 = homicidio_2010,
         expected_2010 = pop_total_2010 * overall_incidence_ratio,
         raw_risk = observed_2010 / expected_2010)

bd_final_ind_dep_count2 <- bd_final_ind_dep_count |>
  mutate(observed_2010 = replace_na(observed_2010, 0),
         expected_2010 = replace_na(expected_2010), 0)

##### 1.1 Estimate the smooth relative risk #####
res <- empbaysmooth(bd_final_ind_dep_count2$observed_2010, bd_final_ind_dep_count2$expected_2010)
bd_final_ind_dep_count2$homicidio_rate_sm_2010 <- res$smthrr

###### 1.1.1 Abrindo Shapefile Municipio dos estados do sudeste ######
shp <- read_municipality(code_muni = "all", year = 2020) %>%
  mutate(Codmun7 = as.numeric(code_muni),
         municipio = as.character(name_muni)) %>%
  dplyr::select(code_state, Codmun7, municipio, geometry = geom) |>
  filter(code_state %in% c(35, 31,  33, 32)) 

###### 1.1.2 Trazendo variáveis para o shapefile pelo Codmun7 e transformando NAs ###### 
shp <- shp |>
  left_join(bd_final_ind_dep_count2, by = "Codmun7") |>
  mutate(homicidio_2010 = replace_na(homicidio_2010, 0),
         pop_total_2010 = replace_na(pop_total_2010, 0))

######  1.1.3 Transformando em formato SP ###### 
shp_sp <- as(shp, "Spatial")

###### 1.1.4 Criando matrix de vizinhança ###### 
w_nb <- poly2nb(shp_sp, row.names = shp_sp$Codmun7)
eb2 <- EBlocal(shp$homicidio_2010, shp$pop_total_2010, w_nb, zero.policy = FALSE)

###### 1.1.5 Criando taxa de homicidio suavizada ###### 
bd_final_ind_dep_count2 <- bd_final_ind_dep_count2 |>
  mutate(homicidio_rate_EBSL_2010 = eb2$est * 100000)

###### 1.1.6 Realizando transformações de variáveis para stepwise ###### 
bakcwards <- bd_final_ind_dep_count2 |>
  mutate(log_homicidio_rate_EBSL_2010 = log(homicidio_rate_EBSL_2010),
         log_homicidio_rate_EBSL_2010 = ifelse(is.infinite(log_homicidio_rate_EBSL_2010), NA, log_homicidio_rate_EBSL_2010),
         gini_scaled_2010 = scale(gini_2010),
         theil_scaled_2010 = scale(theil_2010),
         idhm_renda_scaled_2010 = scale(idhm_renda_2010),
         idhm_scaled_2010 = scale(idhm_2010),
         homicidio_100mil_2010 = (homicidio_2010 / (pop_total_2010/100000)))

#### 2. Verificando variáveis com melhor ajuste - stepwise ####

##### 2.1 Modelo apenas com intercepto (Utilizando variável taxa homicidio suavizada) #####
intercept_only <- lm(log_homicidio_rate_EBSL_2010 ~ 1, data = bakcwards)

##### 2.2 Adicionando as possíveis variáveis independentes #####
independent <- c("gini_scaled_2010",
                "gini_2010",
                "theil_scaled_2010",
                "theil_2010",
                "percent_pop_homem_15a29_2010",
                "percent_desocupacao_18_mais_2010",
                "percent_criancas_6a14_fora_escola_2010",
                "percent_mulher_15a17_com_um_filho_2010",
                "percent_pop_extremamente_pobre_2010",
                "log_percent_pop_extremamente_pobre_2010", 
                "percent_pop_pobre_2010",
                "log_percent_pop_pobre_2010",
                "grau_urbanizacao_2010",
                "pop_total_dividido_mil_2010",
                "idhm_2010",
                "idhm_scaled_2010",
                "idhm_renda_2010",
                "idhm_renda_scaled_2010",
                #"percent_desemprego_2010",   # Adicionado depois (modelo_2)
                "metrop_binaria"            # Adicionado depois (modelo_2)
                #"deleg_pm_mun_binaria_2014"  # Adicionado depois (modelo_2)
                ) 

##### 2.3 Criando novo banco de dados apenas com variáveis selecionadas e removendo NAs #####
bakcward <- bakcwards |>
  dplyr::select(log_homicidio_rate_EBSL_2010, independent) |>
  drop_na()

##### 2.4 Definindo modelo com todas as variáveis independentes #####
model <- lm(log_homicidio_rate_EBSL_2010 ~ ., data = bakcward)

##### 2.5 Performando modelo de regressão stepwise #####
backward_m <- stats::step(model, direction = 'backward', scope = formula(model), trace = 0)

##### 2.6 Vendo resultado da regressão stepwise #####
backward_m$anova
backward_m$coefficients

##### 2.7 Rodando Modelo 1 #####
backwards_model <- lm(homicidio_rate_EBSL_2010 ~ 
                        pop_total_dividido_mil_2010 +
                        percent_mulher_15a17_com_um_filho_2010 +
                        log_percent_pop_extremamente_pobre_2010 +
                        theil_scaled_2010 +
                        percent_desocupacao_18_mais_2010 +
                        #idhm_2010 +
                        log_percent_pop_pobre_2010 +
                        grau_urbanizacao_2010 +
                        percent_criancas_6a14_fora_escola_2010 +
                        #idhm_renda_2010 +
                        metrop_binaria,
                      bakcwards)

summary(backwards_model)

##### 2.8 Rodando Modelo 8 homicidio_rate_EBSL_2010 em LOG #####
backwards_model_2 <- lm(log_homicidio_rate_EBSL_2010 ~ 
                        percent_pop_homem_15a29_2010 +
                        percent_mulher_15a17_com_um_filho_2010 +
                        log_percent_pop_extremamente_pobre_2010 +
                        #theil_scaled_2010 +
                        gini_scaled_2010 +
                        percent_desocupacao_18_mais_2010 +
                        #idhm_2010 +
                        #log_percent_pop_pobre_2010 +
                        grau_urbanizacao_2010 +
                        percent_criancas_6a14_fora_escola_2010 +
                        #idhm_renda_2010 +
                        as.factor(metrop_binaria),
                      bakcwards)

summary(backwards_model_2)


#### 3. Correlação das variáveis independentes modelo 1 ####
var <- c("pop_total_dividido_mil_2010",
         "percent_mulher_15a17_com_um_filho_2010",
         "log_percent_pop_extremamente_pobre_2010",
         "theil_scaled_2010",
         "percent_desocupacao_18_mais_2010",
         #"idhm_2010",
         "log_percent_pop_pobre_2010",
         "grau_urbanizacao_2010",
         "metrop_binaria",
         "percent_criancas_6a14_fora_escola_2010",
         #"idhm_renda_2010",
         "metrop_binaria") 

corr <- metan::corr_coef(bakcwards[, var], use = 'pairwise.complete.obs')
plot(corr)

car::vif(backwards_model_2)

##### 3.1 Salvando Banco de dados backwards_model #####
bakcwards_df <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\backwards_model.xlsx") |>
  mutate_at(vars(contains("scaled")), as.numeric) |>
  mutate(metrop_binaria = as.factor(metrop_binaria),
         log_homicidio_rate_EBSL_2010 = log(homicidio_rate_EBSL_2010),
         log_homicidio_rate_EBSL_2010 = ifelse(is.infinite(log_homicidio_rate_EBSL_2010), NA, log_homicidio_rate_EBSL_2010)) |>
  drop_na()

bakcwards <- bakcwards |>
  dplyr::select(Codmun7, homicidio_100mil_2010)

bakcwards_df <- bakcwards_df |>
  left_join(bakcwards)
  
#### 4. Visualizando variável suavizada ####
##### 4.1 Abrindo ShapeFile Sudeste #####
shp <- read_municipality(code_muni = "all", year = 2020) %>%
  mutate(Codmun7 = as.numeric(code_muni),
         municipio = as.character(name_muni)) %>%
  dplyr::select(code_state, Codmun7, municipio, geometry = geom) |>
  filter(code_state %in% c(35, 31,  33, 32)) 

##### 4.2 Unindo dados do modelo stepwise #####
shp_bakcwards_df <- bakcwards_df |>
  left_join(shp, by = "Codmun7") |>
  st_as_sf()

###### 4.2.1 Matriz de Vizinhança ######
shp_bakcwards_df_sp <- as(shp_bakcwards_df, "Spatial")
nb <- poly2nb(shp_bakcwards_df_sp) 

###### 4.2.2 Criando nova coluna com quantidade de vizinhos ######
shp_bakcwards_df_sp$numero_nb <- card(nb)

###### 4.2.3 Removendo Municipios sem Vizinhos ######
shp_bakcwards_df_sp <- subset(shp_bakcwards_df_sp, shp_bakcwards_df_sp$numero_nb != 0) # Todos poligonos tem ao menos 1 vizinho

###### 4.2.4 Criando nova matriz de vizinhanca para aqueles que tem vizinho ######
nb <- poly2nb(shp_bakcwards_df_sp)
coords_xy <- coordinates(shp_bakcwards_df_sp) # Cria matriz de X e Y com latitude e longitude

###### 4.2.5 Exportar grafico de vizinhanca para sp, precisa ter matriz de vizinhança, coordenadas (lat lon centroid), projeção (crs) ######
nb_sp <- nb2lines(nb, coords = coords_xy, proj4string = crs(shp_bakcwards_df))

###### 4.2.6 Vizinhança normalizada (pesos) ###### 
nb_pesos <- nb2listw(nb)
View(nb_pesos) # Temos lista de vizinhos e peso (Quanto tenho 1 vizinh o peso é 1, quanto tenho 2 vizinhos o peso é 0.5)

nb_sp_pesos <- listw2lines(nb_pesos, coords = coords_xy, proj4string = crs(shp_bakcwards_df))
plot(nb_sp_pesos)

##### 4.3 Visualizando Taxa de Homicídio suavizada EBSL ####
map1 <-
tm_shape(shp_bakcwards_df,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) +
  tm_fill("homicidio_rate_EBSL_2010", style = "quantile", title = "Taxa de Homícidio\nSuavizada 2010", palette = "Reds") +
  tm_layout(legend.position = c("LEFT", "TOP"),
            legend.title.size = 0.8,
            legend.text.size = 0.5) +
  tm_shape(state) +
  tm_borders(col = "black",
             lwd = 1.5) +
  tm_layout(frame = F)

map2 <-
tm_shape(shp_bakcwards_df,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) +
  tm_fill("homicidio_100mil_2010", style = "quantile", title = "Taxa de Homícidio\n100 mil habitantes\n2010", palette = "Reds") +
  tm_layout(legend.position = c("LEFT", "TOP"),
            legend.title.size = 0.8,
            legend.text.size = 0.5) +
  tm_shape(state) +
  tm_borders(col = "black",
             lwd = 1.5) +
  tm_layout(frame = F)

arrange <- tmap_arrange(map1, map2)

tmap_save(map1, filename = "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Referencia\\homicidio_suavizada.png", height = 5, width = 6, dpi=300)
tmap_save(map2, filename = "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Referencia\\homicidio_n_suavizada.png", height = 5, width = 6, dpi=300)

#### 4. Resíduos e previstos do Modelo ####
##### 4.1 Modelo #####
backwards_model <- lm(log_homicidio_rate_EBSL_2010 ~ 
                         percent_pop_homem_15a29_2010 +
                         percent_mulher_15a17_com_um_filho_2010 +
                         log_percent_pop_extremamente_pobre_2010 +
                         #theil_scaled_2010 +
                         gini_scaled_2010 +
                         percent_desocupacao_18_mais_2010 +
                         #idhm_2010 +
                         #log_percent_pop_pobre_2010 +
                         grau_urbanizacao_2010 +
                         percent_criancas_6a14_fora_escola_2010 +
                         #idhm_renda_2010 +
                         metrop_binaria,
                      shp_bakcwards_df_sp@data)

summary(backwards_model)

#### Modelo LM Mono Papel ####
mod_lm_paper <- backwards_model |>
  gtsummary::tbl_regression(intercept = TRUE) |>
  gtsummary::add_vif() |>
  gtsummary::add_significance_stars(hide_p = FALSE, thresholds = c(0.01, 0.05, 0.10)) |>
  gtsummary::modify_column_hide(columns = std.error) |>
  gtsummary::add_glance_table(include = c(r.squared, adj.r.squared, AIC, p.value))

mod_lm_paper

b <- stargazer::stargazer(backwards_model,
                          dep.var.labels = c("", "", "", ""), model.names = F, type = "latex", 
                          out = "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Referencia\\tabl21.txt")

vif_table <- data.frame(car::vif(backwards_model))
logLik(backwards_model)

##### 4.1 Resíduos #####
shp_bakcwards_df <- st_as_sf(shp_bakcwards_df_sp)
shp_bakcwards_df$res_backwards_model <- residuals(backwards_model)

##### 4.2 Previstos #####
shp_bakcwards_df$fitted_backwards_model <- fitted(backwards_model)

##### 4.3 Produzindo mapa cloropleth dos resíduos #####
shp_bakcwards_df$sd_breaks <- scale(shp_bakcwards_df$res_backwards_model)[,1] # Because scale is made for matrices, we just need to get the first column using [,1]
summary(shp_bakcwards_df$sd_breaks)

my_breaks <- c(-5, -3, -2, -1, 1, 2, 3, 5)

tm_shape(shp_bakcwards_df,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) + 
  tm_fill("sd_breaks", title = "Residuals", 
          style = "fixed",
          breaks = my_breaks,
          palette = "-RdBu", 
          midpoint = 0) +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("left", "bottom"), legend.hist.size = 0.3, legend.title.size = 0.8)

#### 5. Teste de Moran Autocorrelação espacial ####
##### 5.1 Teste Moran #####
moran <- lm.morantest(backwards_model, listw = nb_pesos, alternative="two.sided") # P < 0.05 e ObservedMoran I 0.6 (Vizinhos apresentam correlação positiva moderada)
moran

##### 5.2 Correlograma de Moran #####
correlograma_contiguidade <- sp.correlogram(neighbours = nb, var = shp_bakcwards_df$homicidio_rate_EBSL_2010, order = 5, method = "I")
plot(correlograma_contiguidade, main = NULL)

#### 6. Indicadors Locais de Associacao Espacil Lisa MAP ####
localmoran <- localmoran(x = shp_bakcwards_df$homicidio_rate_EBSL_2010, listw = nb_pesos)
View(localmoran)

##### 6.1 Transformando Local Moran em DataFrame
localmoran_df <- as.data.frame(localmoran)

###### 6.1.1 Passando li e p value para banco de dados shp_bakcwards_df ######
shp_bakcwards_df <- shp_bakcwards_df |>
  mutate(moran = localmoran_df$Ii,
         moran_p = localmoran_df$`Pr(z != E(Ii))`)

tm_shape(shp_bakcwards_df) +
  tm_fill("moran_p", style = "quantile",
          pallette = c("red", "lightblue", "blue", "blue4"))

###### 6.1.2 Diagrama de dispersão de Moran I ######
moran.plot(x = shp_bakcwards_df$homicidio_rate_EBSL_2010, listw = nb_pesos, cex = 0.6, labels = FALSE, xlab = "Tx. Homicídio Suavizada", ylab = "Spatially Lagged Tx. Homicídio Suavizada")

###### 6.1.3 Cada fator H acima da media L abaixo da media no poligono ###### 
L1 <- factor(shp_bakcwards_df$homicidio_rate_EBSL_2010 < mean(shp_bakcwards_df$homicidio_rate_EBSL_2010), labels = c("H", "L"))

###### 6.1.4 Vizinhos fator H acima da media L abaixo da media ###### 
shp_bakcwards_df$lag_homicidio_rate_EBSL_2010 <- lag.listw(nb_pesos, var = shp_bakcwards_df$homicidio_rate_EBSL_2010)
L2 <- factor(shp_bakcwards_df$lag_homicidio_rate_EBSL_2010 < mean(shp_bakcwards_df$lag_homicidio_rate_EBSL_2010), labels = c("H", "L"))

shp_bakcwards_df$lisa <- paste(L1, L2)

###### 6.1.5 Mapear apenas aqueles com moran_p < 0.05 ###### 
state_31 <- read_state(code_state = 31)
state_32 <- read_state(code_state = 32)
state_33 <- read_state(code_state = 33)
state_35 <- read_state(code_state = 35)

lisa_map <- shp_bakcwards_df |>
  mutate(LISA = ifelse(moran_p > 0.1, "Não Significativo", lisa))

lisa <- 
  tm_shape(lisa_map,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) + 
  tm_fill("LISA", palette = c("red", "blue4", "lightblue", "blue", "white")) +
  tm_borders("grey") +
  tm_shape(state) +
  tm_borders(col = "black",
             lwd = 1.5) +
  tm_layout(frame = F)

state <- rbind(state_31, state_32, state_33, state_35)

tmap_save(lisa, filename = "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Referencia\\lisa.png", height = 5, width = 6, dpi=300)

#### 7. Teste Multiplicador de Lagrange ####
lm.LMtests(backwards_model, nb_pesos, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

##### 7.1 Fitting RLMlag #####
lag_backwards_model <- lagsarlm(log_homicidio_rate_EBSL_2010 ~ 
                                  percent_pop_homem_15a29_2010 +
                                  percent_mulher_15a17_com_um_filho_2010 +
                                  log_percent_pop_extremamente_pobre_2010 +
                                  #theil_scaled_2010 +
                                  gini_scaled_2010 +
                                  percent_desocupacao_18_mais_2010 +
                                  #idhm_2010 +
                                  #log_percent_pop_pobre_2010 +
                                  grau_urbanizacao_2010 +
                                  percent_criancas_6a14_fora_escola_2010 +
                                  #idhm_renda_2010 +
                                  metrop_binaria,
                     data = shp_bakcwards_df_sp@data,
                     listw = nb_pesos)

summary(lag_backwards_model, Nagelkerke = T)

mod_lag_paper <- lag_backwards_model 

###

a <- stargazer::stargazer(backwards_model, mod_lag_paper,
          dep.var.labels = c("", "", "", ""), model.names = F, type = "latex", 
          out = "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Referencia\\table1.txt")

# The impacts() function gives us something like OLS regression coefficients for a spatial lag model. 
# The logic of the impacts() function is similar to the code above, it tells you the direct (local), 
# indirect (spill-over), and total effect of a unit change in each of the predictor variables. 
# The changes reported by impacts are the global average impact:
impacto <- summary(impacts(lag_backwards_model, listw = nb_pesos, R = 100), zstat = TRUE)
impacto

######################## Acabou Mono ############################################

##### 7.1 Fitting RLMerror #####
error_backwards_model <- errorsarlm(homicidio_rate_EBSL_2010 ~ 
                                  pop_total_dividido_mil_2010 +
                                  percent_mulher_15a17_com_um_filho_2010 +
                                  log_percent_pop_extremamente_pobre_2010 +
                                  theil_scaled_2010 +
                                  percent_desocupacao_18_mais_2010 +
                                  #idhm_2010 +
                                  log_percent_pop_pobre_2010 +
                                  grau_urbanizacao_2010 +
                                  percent_criancas_6a14_fora_escola_2010,
                                  #idhm_renda_2010,
                                data = shp_bakcwards_df_sp@data,
                                listw = nb_pesos)

summary(lag_backwards_model)

#### 5. Regressão com Pesos ####
##### 5.1 Calculando bandwidth adaptative kernel #####
GWR_shp_bakcwards_df <- spgwr::gwr.sel(homicidio_rate_EBSL_2010 ~
                                  pop_total_dividido_mil_2010 +
                                  percent_mulher_15a17_com_um_filho_2010 +
                                  log_percent_pop_extremamente_pobre_2010 +
                                  theil_scaled_2010 +
                                  percent_desocupacao_18_mais_2010 +
                                  idhm_2010 +
                                  log_percent_pop_pobre_2010 +
                                  (grau_urbanizacao_2010):as.factor(metrop_binaria) +
                                  percent_criancas_6a14_fora_escola_2010 +
                                  idhm_renda_2010,
                                data = shp_bakcwards_df, 
                                adapt = TRUE,
                                coords = coords_xy)

##### 5.2 Modelo GWR #####
gwr_backwards_model <- spgwr::gwr(homicidio_rate_EBSL_2010 ~ 
                                    pop_total_dividido_mil_2010 +
                                    percent_mulher_15a17_com_um_filho_2010 +
                                    log_percent_pop_extremamente_pobre_2010 +
                                    theil_scaled_2010 +
                                    percent_desocupacao_18_mais_2010 +
                                    idhm_2010 +
                                    log_percent_pop_pobre_2010 +
                                    (grau_urbanizacao_2010):as.factor(metrop_binaria) +
                                    percent_criancas_6a14_fora_escola_2010 +
                                    idhm_renda_2010,
                                  data = shp_bakcwards_df_sp@data,
                                  adapt = GWR_shp_bakcwards_df,
                                  hatmatrix = TRUE,
                                  se.fit = TRUE,
                                  coords = coords_xy) 

gwr_backwards_model


###### 5.2.1 Criando Mapa de Resultados GWR ######
results_gwr <- as.data.frame(gwr_backwards_model$SDF)
gwr_map <- cbind(shp_bakcwards_df, as.matrix(results_gwr))
  
###### 5.2.2 Distribuição espacial do Local R2 ######

map_maker <- function(db, variable, maintitle, legendtitle, style) {
  plot <- 
    tm_shape(db, bbox = c(-53.10986, -25.35794, -38.84784, -14.23333)) +
    tm_fill(col = variable, title = legendtitle, style = style, palette = "-RdBu", midpoint = 0) +
    tm_borders(alpha = 0.1) +
    tm_layout(main.title = maintitle, main.title.size = 0.7,
              legend.position = c("left", "bottom"), legend.hist.size = 0.3, legend.title.size = 0.8)
  
  return(plot)
}

map_maker(gwr_map, "localR2", "Local R2", "Local R2", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "pop_total_dividido_mil_2010", "Pop. Total Dividido por Mil", "Pop. Total Dividido por Mil", "quantile")
map_maker(gwr_map, "pop_total_dividido_mil_2010.1", "Coef. Pop. Total Dividido por Mil", "Coef. Pop. Total Dividido por Mil", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "percent_mulher_15a17_com_um_filho_2010", "% Mulheres 15 a 17 com filho", "% Mulheres 15 a 17 com filho", "quantile")
map_maker(gwr_map, "percent_mulher_15a17_com_um_filho_2010.1", "Coef. % Mulheres 15 a 17 com filho", "Coef. % Mulheres 15 a 17 com filho", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "log_percent_pop_extremamente_pobre_2010", "Log % pop. extremamente pobre", "Log % pop. extremamente pobre", "quantile")
map_maker(gwr_map, "log_percent_pop_extremamente_pobre_2010.1", "Coef. Log % pop. extremamente pobre", "Coef. Log % pop. extremamente pobre", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "theil_scaled_2010", "Theil Scaled", "Theil Scaled", "quantile")
map_maker(gwr_map, "theil_scaled_2010.1", "Coef. Theil Scaled", "Theil Scaled", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "percent_desocupacao_18_mais_2010", "% Desocupação 18a +", "% Desocupação 18a ", "quantile")
map_maker(gwr_map, "percent_desocupacao_18_mais_2010.1", "Coef. % Desocupação 18a ", "Coef. % Desocupação 18a ", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "idhm_2010", "IDHM", "IDHM", "quantile")
map_maker(gwr_map, "idhm_2010.1", "Coef. IDHM", "Coef. IDHM", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "log_percent_pop_pobre_2010", "Log % Pop. Pobre", "Log % Pop. Pobre", "quantile")
map_maker(gwr_map, "log_percent_pop_pobre_2010.1", "Coef. Log % Pop. Pobre", "Coef. Log % Pop. Pobre", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "grau_urbanizacao_2010", "Grau de Urbanização", "Grau de Urbanização", "quantile")
map_maker(gwr_map, "grau_urbanizacao_2010.1", "Coef. Grau de Urbanização", "Coef. Grau de Urbanização", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "percent_criancas_6a14_fora_escola_2010", "% crianças 6a14a fora da escola", "% crianças 6a14a fora da escola", "quantile")
map_maker(gwr_map, "percent_criancas_6a14_fora_escola_2010.1", "Coef. % crianças 6a14a fora da escola", "Coef. % crianças 6a14a fora da escola", "quantile")

###### 5.2.3 Distribuição pop_total_dividido_mil_2010 e o coeficiente de pop_total_dividido_mil_2010 ######
map_maker(gwr_map, "idhm_renda_2010", "IDHM Renda", "IDHM Renda", "quantile")
map_maker(gwr_map, "idhm_renda_2010.1", "Coef. IDHM Renda", "Coef. IDHM Renda", "quantile")

 