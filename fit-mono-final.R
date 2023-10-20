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
intercept_only <- lm(homicidio_rate_EBSL_2010 ~ 1, data = bd_final_ind_dep_count2)

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
bakcwards <- bd_final_ind_dep_count2 |>
  dplyr::select(homicidio_rate_EBSL_2010, independent) |>
  drop_na()

##### 2.4 Definindo modelo com todas as variáveis independentes #####
model <- lm(homicidio_rate_EBSL_2010 ~ ., data = bakcwards)

##### 2.5 Performando modelo de regressão stepwise #####
backward <- stats::step(model, direction = 'backward', scope = formula(model), trace = 0)

##### 2.6 Vendo resultado da regressão stepwise #####
backward$anova
backward$coefficients

##### 2.7 Rodando Modelo 1 #####
backwards_model <- lm(homicidio_rate_EBSL_2010 ~ 
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
                      bakcwards)

summary(backwards_model)

##### 2.8 Rodando Modelo 2 #####

backwards_model2 <- lm(homicidio_rate_EBSL_2010 ~ 
                        percent_mulher_15a17_com_um_filho_2010  +
                        idhm_renda_2010 +
                        theil_scaled_2010 +
                        log_percent_pop_extremamente_pobre_2010 +
                        percent_desocupacao_18_mais_2010 +
                        log_percent_pop_pobre_2010 +
                        percent_criancas_6a14_fora_escola_2010 +
                        metrop_binaria +
                        deleg_pm_mun_binaria_2014,
                      bakcwards)

summary(backwards_model2)

#### 3. Correlação das variáveis independentes modelo 1 ####
var <- c("pop_total_dividido_mil_2010",
         "percent_mulher_15a17_com_um_filho_2010",
         "log_percent_pop_extremamente_pobre_2010",
         "theil_scaled_2010",
         "percent_desocupacao_18_mais_2010",
         "idhm_2010",
         "log_percent_pop_pobre_2010",
         "grau_urbanizacao_2010",
         "metrop_binaria",
         "percent_criancas_6a14_fora_escola_2010",
         "idhm_renda_2010") 

corr <- stats::cor(bakcwards[, var], use = 'pairwise.complete.obs')
corr

##### 3.1 Salvando Banco de dados backwards_model #####
# backwards_df <- bd_final_ind_dep_count2 |>
  # select(Codmun7, homicidio_rate_EBSL_2010, independent)

# write.xlsx(backwards_df, "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\backwards_model.xlsx")

bakcwards_df <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\backwards_model.xlsx") |>
  mutate_at(vars(contains("scaled")), as.numeric) |>
  mutate(metrop_binaria = as.factor(metrop_binaria))

#### 4. Visualizando variável suavizada ####
##### 4.1 Abrindo ShapeFile Sudeste #####
shp <- read_municipality(code_muni = "all", year = 2020) %>%
  mutate(Codmun7 = as.numeric(code_muni),
         municipio = as.character(name_muni)) %>%
  dplyr::select(code_state, Codmun7, municipio, geometry = geom) |>
  filter(code_state %in% c(35, 31,  33, 32)) 

##### 4.2 Unindo dados do modelo stepwise #####
shp_bakcwards_df <- shp |>
  left_join(bakcwards_df, by = "Codmun7") |>
  drop_na()

###### 4.2.1 Matriz de Vizinhança ######
shp_bakcwards_df_sp <- as(shp_bakcwards_df, "Spatial")
nb <- poly2nb(shp_bakcwards_df_sp) 

###### 4.2.2 Criando nova coluna com quantidade de vizinhos ######
shp_bakcwards_df_sp$numero_nb <- card(nb)

###### 4.2.3 Removendo Municipios sem Vizinhos ######
shp_bakcwards_df_sp <- subset(shp_bakcwards_df_sp, shp_bakcwards_df_sp$numero_nb != 0)

# Todos poligonos tem ao menos 1 vizinho

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
tm_shape(shp_bakcwards_df,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) +
  tm_fill("homicidio_rate_EBSL_2010", style = "quantile", title = "Taxa de Homícidio\nEBSL 2010", palette = "Reds") +
  tm_layout(legend.position = c("left", "BOTTOM"),
            legend.title.size = 0.8,
            legend.text.size = 0.5)

#### 4. Resíduos e previstos do Modelo ####
##### 4.1 Modelo #####
backwards_model <- lm(homicidio_rate_EBSL_2010 ~ 
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
                      shp_bakcwards_df)

##### 4.1 Resíduos #####
shp_bakcwards_df$res_backwards_model <- residuals(backwards_model)

##### 4.2 Previstos #####
shp_bakcwards_df$fitted_backwards_model <- fitted(backwards_model)

##### 4.3 Produzindo mapa cloropleth dos resíduos #####
shp_bakcwards_df$sd_breaks <- scale(shp_bakcwards_df$res_backwards_model)[,1] # Because scale is made for matrices, we just need to get the first column using [,1]
summary(shp_bakcwards_df$sd_breaks)

my_breaks <- c(-Inf, -2.2898, -0.5965, -0.1978, 0.3694, 7.9445, Inf)

tm_shape(shp_bakcwards_df,
         bbox =  c(-53.10986, -25.35794, -38.84784, -14.23333)) + 
  tm_fill("sd_breaks", title = "Residuals", style = "quantile", palette = "-RdBu", midpoint = 0) +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("left", "bottom"), legend.hist.size = 0.3, legend.title.size = 0.8)

#### 5. Regressão com Pesos ####
##### 5.1 Calculando bandwidth adaptative kernel #####
GWR_shp_bakcwards_df <- gwr.sel(homicidio_rate_EBSL_2010 ~
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
                                adapt = TRUE)
