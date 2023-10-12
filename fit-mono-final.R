bd_final_ind_dep_count<- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep.xlsx")
bd_final_ind_dep_count<- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep_count_2.xlsx")

# Smooth 
overall_incidence_ratio = sum(bd_final_ind_dep_count$homicidio_2010, na.rm = TRUE)/sum(bd_final_ind_dep_count$pop_total_2010, na.rm = TRUE)

bd_final_ind_dep_count <- bd_final_ind_dep_count |>
  mutate(observed_2010 = homicidio_2010,
         expected_2010 = pop_total_2010 * overall_incidence_ratio,
         raw_risk = observed_2010 / expected_2010)

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

bd_final_ind_dep_count2 <- bd_final_ind_dep_count2 |>
  mutate(log_homicidio_rate_EBSL_2010 = log(homicidio_rate_EBSL_2010),
         log_homicidio_rate_EBSL_2010 = ifelse(is.infinite(log_homicidio_rate_EBSL_2010), NA, log_homicidio_rate_EBSL_2010),
         gini_scaled_2010 = scale(gini_2010),
         theil_scaled_2010 = scale(theil_2010),
         idhm_renda_scaled_2010 = scale(idhm_renda_2010),
         idhm_scaled_2010 = scale(idhm_2010))

# Models
fit_1 <- lm(homicidio_2010_100mil ~ gini_2010_scale + percent_pop_extremamente_pobre_2010 + percent_desocupacao_18_mais_2010 + percent_criancas_6a14_fora_escola_2010 + percent_pop_homem_15a29_2010 + grau_urbanizacao_2010, bd_final_ind_dep_count)
summary(fit_1)


fit_2 <- lm(homicidio_rate_EBSL_2010  ~ 
              #log_percent_pop_extremamente_pobre_2010 + 
              log_percent_pop_pobre_2010 +
              gini_scaled_2010 + 
              #percent_pop_homem_15a29_2010 + 
              percent_desocupacao_18_mais_2010 + 
              percent_criancas_6a14_fora_escola_2010 + 
              grau_urbanizacao_2010 +
              #theil_scaled_2010 +
              percent_mulher_15a17_com_um_filho_2010 +
              #idhm_renda_scaled_2010 +
              #idhm_renda_2010 +
              pop_total_dividido_mil_2010, 
            bd_final_ind_dep_count2)
summary(fit_2)

# Correlação
var <- c("log_percent_pop_extremamente_pobre_2010", "percent_mulher_15a17_com_um_filho_2010", "gini_2010_scale", "theil_2010_scaled", "percent_desocupacao_18_mais_2010", "percent_criancas_6a14_fora_escola_2010", "pop_total_dividido_mil_2010", "grau_urbanizacao_2010") 

corr <- stats::cor(bd_final_ind_dep_count2[, var], use = 'pairwise.complete.obs')
corr

# Stepwise

#define intercept-only model
intercept_only <- lm(homicidio_rate_EBSL_2010 ~ 1, data = bd_final_ind_dep_count2)

# All independent variables
independent <- c("gini_scaled_2010",
                "gini_2010",
                "theil_scaled_2010",
                "theil_2010",
                "percent_pop_homem_15a29_2010",
                "grau_urbanizacao_2010",
                "pop_total_dividido_mil_2010",
                "percent_desocupacao_18_mais_2010",
                "percent_criancas_6a14_fora_escola_2010",
                "percent_mulher_15a17_com_um_filho_2010",
                "idhm_2010",
                "idhm_scaled_2010",
                "idhm_renda_2010",
                "idhm_renda_scaled_2010",
                "percent_pop_extremamente_pobre_2010",
                "log_percent_pop_extremamente_pobre_2010",
                "percent_pop_pobre_2010",
                "log_percent_pop_pobre_2010")


bakcwards <- bd_final_ind_dep_count2 |>
  dplyr::select(homicidio_rate_EBSL_2010, independent)

# Define model with all predictors
model <- lm(homicidio_rate_EBSL_2010 ~ ., data = bakcwards)

# Perform backward stepwise regression
backward <- stats::step(model, direction = 'backward', scope = formula(model), trace = 0)

# View results of backward stepwise regression
backward$anova
backward$coefficients

backwards_model <- lm(homicidio_rate_EBSL_2010 ~ 
                        pop_total_dividido_mil_2010 +
                        percent_mulher_15a17_com_um_filho_2010 +
                        log_percent_pop_extremamente_pobre_2010 +
                        theil_scaled_2010 +
                        percent_desocupacao_18_mais_2010 +
                        idhm_2010 +
                        log_percent_pop_pobre_2010 +
                        grau_urbanizacao_2010 +
                        percent_criancas_6a14_fora_escola_2010 +
                        idhm_renda_2010,
                      bakcwards)

summary(backwards_model)
