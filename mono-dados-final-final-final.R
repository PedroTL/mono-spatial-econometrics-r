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

# Variáveis Independentes
independentes_simples <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\Atlas 2013_municipal, estadual e Brasil.xlsx", sheet = 2) |>
  filter(UF %in% c(35, 31,  33, 32) & ANO == 2010) |>
  select(UF, Codmun6, Codmun7, municipio = Município, percent_pop_extremamente_pobre_2010 = PIND, idhm_2010 = IDHM, gini_2010 = GINI, theil_2010 = THEIL, percent_mulher_15a17_com_um_filho_2010 = T_M15A17CF, percent_criancas_6a14_fora_escola_2010 = T_FORA6A14, percent_desocupacao_18_mais_2010 = T_DES18M, pop_total_2010 = pesotot, pop_total_urbana_2010 = pesourb, homem_15a19_2010 = HOMEM15A19, homem_20a24_2010 = HOMEM20A24, homem_25a29_2010 = HOMEM25A29) 

# Variavel Dependente
homicide <- read.csv2("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\homicidios2010.csv") |>
  rename(Codmun7 = cod, homicidio = valor, ano = período, municipio = nome) |>
  filter(ano %in% c(2009, 2010, 2011, 2016, 2017, 2018, 2019))

homicide2 <- homicide |>
  group_by(ano) |>
  mutate(row = row_number()) |>
  tidyr::pivot_wider(names_from = ano, values_from = homicidio) |>
  select(-row, municipio, homicidio_2009 = `2009`, homicidio_2010 = `2010`, homicidio_2011 = `2011`, homicidio_2016 = `2016`, homicidio_2017 = `2017`, homicidio_2018 = `2018`, homicidio_2019 = `2019`)

re_organize <- function(df, municipio_col, code_muni_col, homicide_col) {
  require(dplyr)
  
  municipio_col <- sym(municipio_col)
  code_muni_col <- sym(code_muni_col)
  homicide_col <- sym(homicide_col)
  
  df <- df %>%
    select(!!municipio_col, !!code_muni_col, !!homicide_col) %>%
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

# Banco de Dados Final Join
independentes_final <- independentes_simples |>
  left_join(homicide_final, by = c("Codmun7")) |>
  select(UF, Codmun6, Codmun7, municipio = municipio.x, municipio_ipea = municipio.y, everything())

# Calculando Varíaveis
independentes_final <- independentes_final |>
  mutate(percent_pop_homem_15a29_2010 = (raster::rowSums(across(homem_15a19_2010:homem_25a29_2010), na.rm = TRUE)) / pop_total_2010,
         percent_criancas_6a14_fora_escola_2010 = percent_criancas_6a14_fora_escola_2010 / 100,
         percent_mulher_15a17_com_um_filho_2010 = percent_mulher_15a17_com_um_filho_2010 / 100,
         percent_desocupacao_18_mais_2010 = percent_desocupacao_18_mais_2010 / 100,
         percent_pop_extremamente_pobre_2010 = percent_pop_extremamente_pobre_2010 / 100,
         grau_urbanizacao_2010 = pop_total_urbana_2010 / pop_total_2010) |>
  select(UF, Codmun6, Codmun7, municipio, municipio_ipea, percent_pop_extremamente_pobre_2010, percent_desocupacao_18_mais_2010, percent_mulher_15a17_com_um_filho_2010, percent_pop_homem_15a29_2010, grau_urbanizacao_2010, percent_criancas_6a14_fora_escola_2010, gini_2010, theil_2010, idhm_2010, homicidio_2009, homicidio_2010, homicidio_2011, homicidio_2016, homicidio_2017, homicidio_2018, homicidio_2019, everything())

# Salvando
write.xlsx(independentes_final, "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\bd_final_ind_dep.xlsx")
