# Abrindo Dados
independentes_simples <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\indicador_simples.xlsx", sep = " ") |>
  mutate(Territorialidades = unaccent(Territorialidades),
         Territorialidades = str_squish(str_remove_all(Territorialidades, "\\(SP\\)")),
         Territorialidades = str_squish(str_replace_all(Territorialidades, "-", " ")),
         Territorialidades = toupper(Territorialidades),
         Territorialidades = ifelse(Territorialidades == "EMBU", "EMBU DAS ARTES", Territorialidades),
         Territorialidades = ifelse(Territorialidades == "SAO LUIS DO PARAITINGA", "SAO LUIZ DO PARAITINGA", Territorialidades),
         Territorialidades = ifelse(Territorialidades == "FLORINIA", "FLORINEA", Territorialidades)) |>
  rename(municipio_f = Territorialidades) |>
  clean_names()


# Abrindo Codigo Municipio
codigo_municipio <- read.xlsx("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\code_muni.xlsx", sep = " ") |>
  filter(UF == 35) |>
  clean_names() |>
  mutate(nome_municipio = toupper(nome_municipio),
         nome_municipio = unaccent(nome_municipio),
         nome_municipio = str_squish(str_replace_all(nome_municipio, "-", " ")))|>
  select(codigo_municipio_completo, municipio_f = nome_municipio)

# Unindo Variaveis Independentes no Banco de Dados CodMunicipio IBGE
independentes_final <- codigo_municipio |>
  left_join(independentes_simples, by = "municipio_f")

# Taxa de Homicidio 100 mil habitantes IPEA DATA
homicide <- read.csv2("C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Mono - Bancos de Dados\\Banco de Dados Atualizado\\homicidios2010.csv") |>
  rename(code_muni = cod, homicidio = valor) |>
  filter(período %in% c(2000, 2009, 2010, 2011, 2016, 2017, 2018, 2019))

# Realizando Pivot Wider e selecionando anos 2000, 2009, 2010, 2011
homicide2 <- homicide |>
  group_by(período) |>
  mutate(row = row_number()) |>
  tidyr::pivot_wider(names_from = período, values_from = homicidio) |>
  select(-row, municipio = nome, homicidio_2000 = `2000`, homicidio_2009 = `2009`, homicidio_2010 = `2010`, homicidio_2011 = `2011`, homicidio_2016 = `2016`, homicidio_2017 = `2017`, homicidio_2018 = `2018`, homicidio_2019 = `2019`)

# Reorganizando Banco de Dados Homicidios
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
    mutate(codigo_municipio_completo := !!code_muni_col) |>
    mutate(codigo_municipio_completo := as.character(codigo_municipio_completo)) |>
    mutate(!!code_muni_col := str_sub(!!code_muni_col, start = 1, end = -2))
  
  return(df)
}

# Selecionando anos de 2000, 2009, 2010, 2011
homicidio2000 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2000")
homicidio2009 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2009")
homicidio2010 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2010")
homicidio2011 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2011")
homicidio2016 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2016")
homicidio2017 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2017")
homicidio2018 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2018")
homicidio2019 <- re_organize(homicide2, "municipio", "code_muni", "homicidio_2019")

# Unindo em Banco de Dados Final
homicide_final <- plyr::join_all(list(homicidio2000, homicidio2009, homicidio2010, homicidio2011, homicidio2016, homicidio2017, homicidio2018, homicidio2019), by = c("code_muni", "municipio"), type = "left")

homicide_final <- homicide_final |>
  select(-everything(contains(c("."))))

rm(homicidio2000, homicidio2009, homicidio2010, homicidio2011, homicidio2016, homicidio2017, homicidio2018, homicidio2019)

# Banco de Dados Final
final_df_dep_ind <- independentes_final |>
  left_join(homicide_final, by = "codigo_municipio_completo") |>
  drop_na(code_muni)

# Municipios sem informacao de homicidio IPEA

#3500600 AGUAS DE SAO PEDRO
#3504701 BALBINOS
#3507456 BOREBI
#3515657 FERNAO
#3516101 FLORINEA
#3528858 MARAPOAMA
#3529658 MESOPOLIS
#3532843 NOVA CANAA PAULISTA
#3534500 OSCAR BRESSANE
#3535903 PARANAPUA
#3544509 RUBINEIA
#3546108 SANTA CLARA D OESTE
#3547650 SANTA SALETE 
#3554755 TRABIJU

write.xlsx(final_df_dep_ind, "C:\\Users\\pedro\\Documents\\GitHub\\mono-spatial-econometrics-r\\Banco Dados Simples\\final_df_dep_ind.xlsx")
