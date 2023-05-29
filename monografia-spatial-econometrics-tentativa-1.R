#### Banco de dados e diretorio ####
library(sf)
library(sp)
library(rgdal)
library(tmap)
library(raster)
library(tidyverse)
library(geobr)
library(readxl)

library(cleangeo) # Correcao topologica
library(spdep) # Dependencia espacial
library(pgirmess) # Correlograma de distancia
library(spatialreg) # Regressao espacial global
library(spgwr) # Regressao ponderada geografica
setwd("C:\\Users\\CEA ALUNO\\Desktop\\mono-spatial-econometrics-r\\mono-spatial-econometrics-r\\Bancos de Dados")

#### 1. Abrindo dados Homicidios por municipio Atlas da Violencia IPEA 2000-2010 ####
csv_homicidios <- read.csv("homicidios.csv", sep = ";") %>%
  filter(período == 2010) %>%
  select(code_muni = cod, ano = período, valor)

#### 1.1 ShapeFile Municipios SP ####
shp_sp <- read_municipality(35, year = 2020)

#### 1.2 Trazendo dados do Censo 2010 ####
sp_capital <- read_excel("Basico_SP1.xls") %>%
  select(code_muni = Cod_municipio, cod_setor = Cod_setor, V001:V012)

sp_ncapital <- read_excel("Basico_SP2.xls") %>%
  select(code_muni = Cod_municipio, cod_setor = Cod_setor, V001:V012)

#### 1.3 Juntando dados de toda SP ####
sp_censo <- rbind(sp_capital, sp_ncapital)

rm(sp_capital, sp_ncapital)

#### 2. Agrupando dados por Municipio csv_homicidio ####
csv_homicidios <- csv_homicidios %>%
  group_by(code_muni) %>% # Temos 2000-2010 de Info. Pegar apenas dde 2010? 
  summarize(valor = sum(valor, na.rm = TRUE)) %>%
  drop_na() %>%
  ungroup()

#### 2.1 Agrupando dados por municipio censo variavel V007: Valor do Rend. Nominal Médio Mensal das Pessoas Responsaveis por Domicilios ####
sp_censo <- sp_censo %>%
  group_by(code_muni) %>% # Não agrupar por setor censitário?
  summarize(V007 = sum(V007, na.rm = TRUE),
            V002 = sum(V002, na.rm = TRUE))

#### 2.2 Join por Municipio (Left shp_sp) dados de homicidio + censo ####
# Padronizando colunas
csv_homicidios <- csv_homicidios %>%
  mutate(code_muni = as.factor(code_muni),
         valor = as.numeric(valor))

sp_censo <- sp_censo %>%
  mutate(code_muni = as.factor(code_muni),
         V007 = as.numeric(V007),
         V002 = as.numeric(V002))

shp_sp <- shp_sp %>%
  mutate(code_muni = as.factor(code_muni),
         name_muni = as.character(name_muni)) %>%
  select(code_muni, name_muni, geometry = geom)

# Iniciando Join por code_muni
shp_sp <- shp_sp %>%
  left_join(csv_homicidios)

shp_sp <- shp_sp %>%
  left_join(sp_censo)

shp_sp <- shp_sp %>%
  rename(homicidio = valor) %>%
  drop_na()

# Estrutura shp_sp - code_muni | name_muni | homicidio | randam | geometry
rm(sp_censo, csv_homicidios) # Mantendo apenas shp_sp no historico

#### 3. Iniciando Tratamento #### 
plot(shp_sp["homicidio"], border = NA)
plot(shp_sp["V007"], border = NA)
plot(shp_sp["V002"], border = NA)

#### 3.1 Tratamento dados Nulos ####
# Pegando RendaM que seja não NUL
sp_censo_V007_nulo <- subset(shp_sp, is.na(shp_sp$V007 & shp_sp$V002) == FALSE)

# Pegando não nulo e perguntando se é 0
muni_validos <- subset(sp_censo_V007_nulo, sp_censo_V007_nulo$V007 != 0)

#### 3.2 Analise Topologica ####
# Vendo se um poligono 'bate' um com outro e não tem geometria invalida (Quando no desenho do poligono um linha se intersecta com a outra)

# Transformando em sp
muni_sp <- as(muni_validos, "Spatial")

# Fazemos pergunta se esta tudo certo
clgeo_IsValid(muni_sp)

# Tudo OK muni_sp

#### 4. Critério de vizinhança ####
#### 4.1 Matriz vizinhança contiguidade (Todos que tocam são vizinhos) ####
# Criando matriz de vizinhança 
vizinhanca <- poly2nb(muni_sp) # Formato SP

# Cada elemento da lista é um poligono e temos o valor do lado, quais poligonos ele é vizinho (poligono 1 é vizinho do 183, 310, 326, 631)
View(vizinhanca)

# Contar quantos vizinhos tem cada elemento
# Elemento 1 tem 4 vizinhos ..
view(card(vizinhanca)) 

# Criando nova coluna com quantidade de vizinhos
muni_sp$vizinhos <- card(vizinhanca)
view(muni_sp@data) # Formato SP

# Pegando setores que tem algum vizinho, conectados entre si (Tirando setores sem vizinhos) 
muni_juntos <- subset(muni_sp, muni_sp$vizinhos != 0)
view(muni_juntos@data) # Todos poligonos tem ao menos 1 vizinho

# Criando nova matriz de vizinhanca para aqueles que tem vizinho
vizinhanca2 <- poly2nb(muni_juntos)
setores_xy <- coordinates(muni_juntos) # Cria matriz de X e Y com latitude e longitude

# Plotando muni
plot(muni_sp, border = "red")

# Plotando relações de vizinhança, indicando as coordenadas que ligam estas vizinhanças (Centroid de cada setor censitário sendo latitude e longitude)
plot(x = vizinhanca2, coord = setores_xy, cex = 0.6, add = TRUE) # Visualizando aqueles que são vizinhos entre si (Se ta ligado é pq tem vizinhança)

#### 4.2 Vizinhança normalizada (Com peso) ####
# Temos lista de vizinhos e peso (Quanto tenho 1 vizinh o peso é 1, quanto tenho 2 vizinhos o peso é 0.5)
vizinhanca_peso <- nb2listw(vizinhanca2)
View(vizinhanca_peso)

# Plotando muni
plot(muni_sp, border = "red")

# Plotando relações de vizinhança, indicando as coordenadas que ligam estas vizinhanças (Centroid de cada setor censitário sendo latitude e longitude)
plot(vizinhanca_peso, setores_xy, cex = 0.6, add = TRUE)

#### 4.3 Vizinhança por distancia ####
#### "n" vizinhos mais proximos (considera como vizinho 4 ou 5 pontos mais proximos) ####
# Entra com info de quem é vizinho e as coordenadas de cada elemento, dai ele ve a coord de cada elemento e a distancia para atribuir funcao
distancias <- nbdists(vizinhanca2, setores_xy)
View(distancias)

# Elemento 1 vai ter uma distancia de tanto de outros elementos
View(unlist(distancias)) # O numero 1 esta a tantos metros do elemento mais proximo
summary(unlist(distancias))

#### 4.4 Vizinhança por Raio de distancia ####
# Distancia maxima (considera como vizinho todos que estão a 1km de distancia do nosso ponto)
# Procure o vizinho mais proximo dentro da coordenada xy d1 dist min d2 dist max
vizinhanca_400m <- dnearneigh(setores_xy, d1=0, d2=0.4)
View(vizinhanca_400m)

# Elemento 1 esta a menos de 400 metros deste, deste
plot(vizinhanca_400m, setores_xy, cex = 0.3)
  
#### 4.4 K vizinhos mais proximos ####
# Funções de distancia (wij com valores continuos) (Diferenciar o peso de cada vizinho em relação a distancia, mais proximos tem mais peso) (precisa explicitar funcao matematica, decaimento linear, decaiment exponencial negativo da distancia, decaimento negtivo exponencial da distancia) (como que a distancia afeta o peso)
# Qual o numero de vizinhos que queremos (considere commo vizinhs os 4 pontos de centroides mais proximos)
vizinhos_4 <- knearneigh(setores_xy, k = 4)

# Elemento 1 - estes são os 4 vizinhos mais proximos
View(vizinhos_4$nn)


vizinhanca_4 <- knn2nb(vizinhos_4)
View(vizinhanca_4)

# Plotando muni
plot(muni_sp, border = "red")

# Adicionando vizinhanca
plot(vizinhanca_4, setores_xy, cex = 0.6, add = TRUE) # Eu posso ser vizinho do outro mas o outro pode não ser meu vizinh

# Neste metodo k vizinhos mais proximos todos tem algum vizinh, diferente dos outros metodos (Resolve prob de vizinho isolado, se tem mts ilhas é boa opcao)

#### 5. Correlação espacial (Dependencia Espacial) ####
# As coisas mais proximas se parecem mais entre si do que as mais distantes - Wald Tbler (1970)
# Auto correlacao espacial (grau dedependencia espacial)
# Tobler W R 1970 A computer movie simulating urban growth in the detrid regiono 46, 234-40

# Autocorrelacao positiva (Lei de tobler) - Feicoes similares em localizacao tambem sao similares em atributos
# Autocorrelacao negativa (Oposicao a lei de tobler) - Feicoes similares em localizacao tendem a ter atributos menos similares do que feicoes mais distantes (O vizinho tende a ser mais diferente do que o elemento mais distante)
# Ausencia de autocorrelação - Atributos sao independentes da localizacao

#### 5.1 Duas formas de cacular indices de autocorrelacao espacial ####
# 1. Indices globais de associacao espacial
  # Apresenta uma medida unica para toda a area analisada (Autcorrelacao positiva, negativa ou sem autocorrelacao)
  # Indice global de moran

# 2. Indices locais de associacao espacial (LISA)
  # Decomposicoes ds indices globais, podem ser visualizados na forma de mapas
  # Permite a identificacao de diferentes regimes de associacao espacial
  # Indice local de Moran (Pode ter locais que tem autocorrelacao positiva, outro local com negativa)

#### Indice global de moran ####
  # Indice global de autocorrelaca espacial, que varia entre -1 e 1
  # Correlacao de um atributo de um elemento no espaco em relacao ao mesmo atributo nos vizinhos (Renda- "Se for vizinho sera q tende a ser semelhante?, qual a correlacao entre o elemento do poligono e o valoor dos elementos vizinhos. Sera que os vizinhos são semelhantes?")
  # Extrema autocorrelacao psitiva (lei de tobler) I = 1 (Feeicoes similares em localizacao tambem sao similares em atributos)
  # Extrema autocorrelacao negativa (Oposicao a lei de Tobler) I = -1 Ficoes similares em localizacao endem a ter atributos menos similares do que feicoes mais distantes
  # Ausencia de autcorrelcao I = 0 Quando atributos sao independentes da localizacao

#### 5.2 Calculando indice de moran ####
# Qual atributo estamos medindo, ver se a renda dos vizinhos é semelhante
# Utilizar matriz normalizada (Todos os vizinhos tem influencia semelhante quando comparado com o poligono central)
moran.test(x = muni_juntos$V007, listw = vizinhanca_peso)

# P < 0.05 é significativo
# Moran I > 0 autocorrelacao espacial positiva (Vizinhos tendem a ser semelhantes)

#### 5.3 Correlograma de indice de moran ####
# Idea do correlograma é o seguinte, primeiro voce analise em relacao ao vizinho depois voce ve em relação aos vizinhos de segundo grau, vizinho do vizinho
# O valor do rosa, o quanto ele é semelhante aos verdinhos (img 1h17)
# O valor do rosa, o quanto ele é semelhante aos amarelos
# Conforme afasta o grau de vizinhanca como é a relação

correlograma_contiguidade <- sp.correlogram(neighbours = vizinhanca2, var = muni_juntos$V007, order = 5, method = "I")
plot(correlograma_contiguidade)
correlograma_contiguidade

# Quando passa para o segundo vizinho (lags) temos uma caida da semelhanca, quanto esta no 5 grau de vizinhaca a vizinhanca nao influencia tanto o atributo de renda
# Não é para todos que temos P < 0.5 (n aleatorio) no 3 e 5 lag P > 0.05

#### 5.3 Correlograma de distancia ####
# Ele usa a distancia (Todos os vizinhos q estao a 100 metros, qual é autocorrelacao espacial, depois 200 metros (no nosso caso a distanncia entre os pontos, centroides dos poligonos))
correlograma_distancia <- correlog(setores_xy, muni_juntos$V007)
plot(correlograma_distancia)
correlograma_distancia

# Começa alto e vai diminuindo com aumento da distancia (Normalmente)
# Nosso caso temos alto na maioria das distancias
# nem todos são P < 0.05

#### 6. Matriz vizinhanca para ordens superiores #### 
# Vamo falar que é vizinho nao so o primeiro mas o segundo vizinho
vizinhanca_1e2 <- nblag(vizinhanca2, maxlag = 2) # Expande nivel vizinhanca

# Pega a matriz de vizinhanca e acumula ate chegar a ordem 2
# 2 atributos (Primeiro para 1 vizinho eo segundo para o vizinho do vizinho, vizinhanca 2)

# Juntar vizinho de primeiro e segundo grau e falar q é tudo vizinho
vizinhanca_ordem_2 <- nblag_cumul(vizinhanca_1e2)
View(vizinhanca_ordem_2)

#### 7. Vizinhanca acumulada de ordens superiores (2 ordens agrupadas) ####
# Normalizar primeiro
vizinhanca_peso_ordem_2 <- nb2listw(vizinhanca_ordem_2) # Se tiver 10 vizinhos cada um vai ter 0.1 de peso e em seguida calcula teste de moran (considerando vizinho 1 e 2 junts)
moran.test(x = muni_juntos$V007, listw = vizinhanca_peso_ordem_2)

# Autocorrelacao positiva mas menor do que a de ordem 1
# P < 0.05 é significativo (Pode acontecer que a de ordem 2 ou 3 tenha mais autocorrelacao do que a de 1 ordem) (Qual tipo de vizinhanca traduz melhor a correlacao espacial)

#### 7. Indicadors Locais de Associacao Espacil (LISA) ####
# Para cada objeto vamos considerar a relacao com os vizinhos e falar se este objeto esta autocorrelacionado com seus vizinhos
# Valor especifico para cada objeto
# Identificacao de:
  # Clusters: Objetos com valores de atributos semelhantes
  # Outliers: Objetos anomalos em relacao aos vizinhos
  # Regimes espaciais distintos

#### 7.1 Indice local de moran ####
# Como o atributo de um objeto esta correlacionado ou nao com os seus vizinhos
localmoran <- localmoran(x = muni_juntos$V007, listw = vizinhanca_peso)
View(localmoran)

# Resultdo tabela, onde cada elemento ele fala o indice de moran li, se 0,2 é positivo, se 0,5 positvo e mais forte
# valor P < 0.05 é significativo 

# Transformação em dataframe info do localmoran e adicionando ao muni_junts para plots
localmoran_df <- as.data.frame(localmoran)
muni_juntos$moran <- localmoran_df$Ii
muni_juntos$moran_p <- localmoran_df$`Pr(z != E(Ii))`
View(muni_juntos@data)
summary(muni_juntos$moran)

# Pega o arquivos setores juntos e desenhe o moran com intervalos fixo
tm_shape(muni_juntos) +
  tm_fill("moran", style = "fixed", breaks = c(-3, 0, 0.2, 0.5, 30),
          pallette = c("red", "lightblue", "blue", "blue4"))

# Lugares vermelhos sao discrepantes (correlacao negativa, muito diferente dos vizinhos)
# Lugares mais verdes são positivos e semelhantes aos vizinhos

# Cruzando dados, vendo onde temos significancia P < 0.05
tm_shape(muni_juntos) +
  tm_fill("moran_p", style = "fixed", breaks = c(0, 0.01, 0.05, 1),
          pallette = c("darkblue", "blue", "blue4"))

#### 8. Diagrama de espalhamento de Moran ####
# Cada ponto é o valor de um atributo (renda)
# Eixo y e a media dos valores dos vizinhos (Padronizar, 0 é a media)
# Criar quadrantes onde pode separar valor alto de renda no local e os vizinhos tmb tem valor alto Q1 autocorrelacao positiva
# Poligonos quem tem valor baixo de renda e os vizinhos tem valor baixo Q2
# Poligono tem renda baixa e os vizinhos tem renda alta Q3

#### 8.1 Spatial Lag - Média dos valores dos vizinhos ####
# Qual q é a media dos vizinhs
# Pega vizinhanca ponderada e usa ela pra calcular a media das rendas dos vizinhos
# Calcular o valor dos vizinhos e colocar em um municipio sem informacao
muni_juntos$lag_renda <- lag.listw(vizinhanca_peso, var = muni_juntos$V007)
View(muni_juntos@data)

tm_shape(muni_juntos) + 
  tm_fill("lag_renda", style = "quantile")
  
# Moran plot pede qual atributo e qual matriz de vizinhança
moran.plot(x = muni_juntos$V007, listw = vizinhanca_peso, cex = 0.6, labels = FALSE)

#### 9. LISA map ####
# Cada fator H acima da media L abaixo da media no poligono
# H - High
# L - Low
L1 <- factor(muni_juntos$V007 < mean(muni_juntos$V007), labels = c("H", "L"))

# Vizinhos fator H (High) acima da media e L (Low) abaixo da media
L2 <- factor(muni_juntos$lag_renda < mean(muni_juntos$lag_renda), labels = c("H", "L"))

# Visualização com categorias usando H L
muni_juntos$lisa <- paste(L1, L2)

# Mapa lisa
tm_shape(muni_juntos) +
  tm_fill("lisa", palette = c("blue", "green", "yellow", "red"))

View(setores_juntos@data)

# Mapear só municipios com valor p abaixo de 0.05
lisa_map <- muni_juntos[muni_juntos$moran_p <= 0.010, ]
table(lisa_map$lisa)
tm_shape(muni_sp) + 
  tm_borders() +
  tm_shape(lisa_map) +
  tm_fill("lisa", palette = c("blue", "red"))

#### 10. Suavização espacial ####
# Suavizacao por estimadores bayesianos empiricos 
# Casos de risco em locais com baixa populacao
  # Geram altas taxas de risco
  # Podem ser gerados por acaso

# As vezes uma ocorrencia em um local coom baixa populacao no momento que se divide pela populacao pode gerar um risco grande 
# Pode-se redistribuir o risco dos locais com baixa populacao para as demais areas
# Qual o risco real de aconteccer algo naquele local
  # Altera de risco observado (Qnt_casos/Pop) para previsão do risco (qual a chance de acontecer no futuro)

# Metodo global
  # Redistribuir para todas as demais regioes (Aconteceu aqui, uma parte do risco é ao acaso e poderia ter acontecido em qualquer local)

# Metodo local
  # Redistribui risco para os vizinhs
    # Usa estrutura de autocorrelacao espacial

# Valores suavizados sao melhores para prever o futuro com base no que conhecemos do passado

#### 10.1 Suavizacao por estimadores bayesianos empiricos (Global) ####
# n = Casos de risco
# x = Populacao de risco
bayes_global <- EBest(n = muni_juntos$homicidio, x = muni_juntos$V002, family = "binomial")

# DF com 2 colunas (1 é risco observado n/x a segunda é o estimado, oq deve acontecer futuro)
View(bayes_global)

# Mapa dos valores brutos (Observado n/x)
muni_juntos$homic_V002 <- bayes_global$raw
tm_shape(muni_juntos) + 
  tm_fill("homic_V002", style = "fisher")

# Mapa dos valores estimados
muni_juntos$beyes_gl <- bayes_global$estmm
tm_shape(muni_juntos) + 
  tm_fill("beyes_gl", style = "fisher")

#### 10.2 Suavizacao por estimadores bayesiano empirico (Local) ####
bayes_local <- EBlocal(ri = muni_juntos$homicidio, ni = muni_juntos$V002, nb = vizinhanca2)
View(bayes_local)

# Mapa dos valores brutos (Observado n/x)
muni_juntos$homic_V002_local <- bayes_local$raw
tm_shape(muni_juntos) + 
  tm_fill("homic_V002_local", style = "fisher")

# Mapa dos valores estimados
muni_juntos$beyes_gl_local <- bayes_local$est
tm_shape(muni_juntos) + 
  tm_fill("beyes_gl_local", style = "fisher")

#### 11. Regressao espacial ####
plot(data = muni_juntos, homicidio ~ V007)
regressao_convencional <- lm(data = muni_juntos, homicidio ~ V007)
summary(regressao_convencional)
AIC(regressao_convencional)

plot(data=muni_juntos, homicidio ~ V007)
abline(regressao_convencional, col = "red", lwd = 2)

# Analise dos residuos
  # Medir a autocorrelacao espacial dos residuos da regressao (ndice de moran dos residuos)

# Se houver autocorrelacao espacial
  # Pode haver alguma variavel ou padro espacial que nao foi investigada pelo modelo
  # Possibilidade de aplicar modelos de regressao espacial

#### 11.1 Analise dos residuos ####
View(regressao_convencional$residuals)

muni_juntos$residuos <- regressao_convencional$residuals
tm_shape(muni_juntos) +
  tm_fill("residuos", style = "quantile", pallete = heat.colors(5))

lm.morantest(regressao_convencional, listw = vizinhanca_peso)
# Os vizinhos tem autocorrelacao positiva e o valor p é menor q 0.05

#### 11.2 Regressao espacial ####
# Global
  # Inclui no modelo de regressao um parametro que captura a estrutura de autocorrelacao espacial na area de estuco como um todo
  # A tendencia do vizinho ser semelhante é igual no mapa tod  

# Locais
  #  Parametros variam continuamente no espaco
  # Relacoes mudam em diferentes locais do mapa
  # Gerar um modelo de regressao para cada poligono com base nos vizinhos

# Modelos com efeitos espaciais globais
  # Premissa
  # é possivel capturar a estrutura de correlacao espacial num unico parametro (adicionado ao modelo de regressao)

# Alternativas
  # Spatial lag models: Atribuem a autocorrelacao espacial a vriavel resposta Y (Se eu sei o valor y dos vizinhos, alem dos valores x que tenho no mapa inteiro, posso pegar o valor y dos meus vizinhos para calcular e ajudar a inferir o valor y do meu dado)
  # Sparial Error Models: Atribuem a autocorrelação ao erro (Se eu sei o erro do meu modelo nos meus vizinhos eu consigo usar o erro dos meus vizinhos para prever o erro no meu poligono)

#### 12. Modelo Spatial Lag ####
# Premissa A variavel Yi é afetada pelos valores da variavel resposta nas areas vizinhas a i
# Y = pWY + Xb + u
# p = Coeficiente espacial autorregressivo - Medida de correlacao espacial (p = 0, se autocorrelacao é nula - Hipotese Nula)
# W = Matriz e proximidade espacial
# WY = Expressa a dependencia espacial em Y

# Pego o dados dos meus vizinhos, uso o indice de moran pra saber o quanto eles influenciam e divido pelo meus vizinhos
regressao_espacial_lag <- lagsarlm(data = muni_juntos, homicidio ~ V007, listw = vizinhanca_peso)

# Autorregressivo - Pega o resultado do modelo linear dos vizinhos e usa a para estimar para o nosso modelo no poligono
summary(regressao_espacial_lag, Nagelkerk = TRUE) #Pseudo R2 de Nalgelkerke

# Rho 0,80 É a influencia dos vizinhos se aumenta 1 Real na renda dos vizinhos aumenta 80 centavos na renda do nosso poligono  

#### 13. Modelo Spatial Error ####
# Premissa; As observacoes sao interdependentes gracas a variavel nao mensurada, e que sao espacialmente correlacionadas
# Ou seja: efeitos espaciais sao um ruido
# ASssume que se pudessemos adicionar as variaveis certas para remover o erro do modelo, o espaco nao importaria mais
# Tenta medir não pelo valor do Y dos vizinhos mas pelo erro
# Efeitos espaciais é porque existe um ruido, variaveis que nao conhecemos no nosso territorio mas influenciam os dados, vamos fingir que criamos uma variavel ficticia que explique a variacao dos erros no territorio e usa ela para tentar prever o valor da variavel Y
# Se tivessemos todas as explicacoes sobre o terreno conseguiriamos explicar perfeitamente Y, como nao temos nos utilizamos o erro para tentar simular a varivel que nao temos
# Y = Xb + u
# u = pWu + e

# Wu = erro com efeito espacial
# p = medidad de correlacao espacial
# e = componente do erro com variancia constante e nao correlacionada
regressao_espacial_CAR <- spautolm(data = muni_juntos, homicidio ~ V007, listw = vizinhanca_peso, family = "CAR")
summary(regressao_espacial_CAR, Nagelkerke = TRUE)

# lambda = coeficiente de vizinhanca dos residuos (o erro dos vizinhos é semelhante 0.52)
# AIC criterio de informacao de Akaike (quanto menor melhor o modelo fitted)

#### 14. Geographically Weighted Regression ####
# Pra eu prever o que acontece em um ponto especifico eu vou fazer uma regressao usando uma certa quantidade de vizinhos, mas nao todos, escolho um raio e falo assim, vou usar so estes vizinhos para o modelo de regressao para este ponto e alem disso os que estao mais longe vao pesar menos que o mais proximo
# Ajusta um modelo de regressao a cada ponto observado, ponderando todas as demais observacoes como funcao da distancia a este ponto
# y(i) = b0(i) + b1(i)x1 + e(i)

# b0(i), b1(i) - Para cada ponto i do espaço ha um b0 e b1 diferentes
# Para cada ponto observado tentamos prever ele usando uma certa funcao de distancia a este ponto, os os mais proximos influenciam mais e os mais longes influenciam menos

# Funcao de kernel sobre cada ponto do espaco para ponderar os pontos vizinhs em razao da distancia
# Pontos mais proximos do ponto central tem maior peso

# Assim como no kernel - a escolha da largura da banda é importante (Pode ser fixa ou adaptavel a densidade dos dados)

# Homicidio (resposta) X Renda (Preditora)
# GWR Homicidioi = b0(ui,vi) + b1(ui, vi). Rendai + ui

# mapa 2h48
# Esquerda, quanto que o aumento de 1% na renda aumenta tanto no homicidio
# Direita, mapear a qualidade do modelo

#### 14.1 GWR raio fixo ####
raio <- gwr.sel(data = muni_juntos, homicidio ~ V007)
raio

setores_gwr_fixo <- gwr(data = muni_juntos, homicidio ~ V007, bandwidth = raio)
View(setores_gwr_fixo$SDF@data)
# So aqueles municipio que o centro dele estiver em até 1041.708 metros do centro do nosso poligono que estamos utilizando vai ser utilizado pra calcular a regressao
summary(setores_gwr_fixo)

#### 14.2 GWR raio adaptativo ####
raio_adaptativo <- gwr.sel(data = muni_juntos, homicidio ~ V007, adapt = TRUE)
raio_adaptativo # # Eu vou usar 99% dos meus municipios que estao mais proximos do nosso setor pra fazer equacao de regressao

setores_gwr_adaptativo <- gwr(data = muni_juntos, homicidio ~ V007, adapt = raio_adaptativo)
# Em um local que tiver varios setores proximos o raio tende a ser menor
# Em um local com poucos setores proximos o raio tende a ser maior
View(setores_gwr_adaptativo$SDF@data)

# V007 = coeficiente de predicao - Considerando os vizinhos mais proximos, se eu aumento 1% na Renda oq acontece com o Homicidio
# gwr.e = erro da predicao
# pred = valor predito
# localR2 = R2 local

# Comparando raio fixo e o adaptativo
# Raiz do erro medio quadratico (Root mean Square erro RMSE)
# O que errar menos é melhor
sqrt(mean(setores_gwr_fixo$SDF$gwr.e^2))
sqrt(mean(setores_gwr_adaptativo$SDF$gwr.e^2))

tm_shape(setores_gwr_adaptativo$SDF) +
  tm_fill("V007", style = "fisher")

# Qunto mais intenso maior influencia da renda no Homicidio
# Quanto mais claro mais inf negativa, se tem mais Renda tem menor Homicidio

# Mapear R2
# Quanto mais intenso maior o R2
tm_shape(setores_gwr_adaptativo$SDF) +
  tm_fill("localR2", style = "fisher")
