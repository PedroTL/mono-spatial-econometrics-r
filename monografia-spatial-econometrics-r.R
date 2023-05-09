#### 1. Pacotes, diretorio, dados ####
#### Diretorio ####
setwd("C:\\Users\\CEA ALUNO\\Desktop\\mono-spatial-econometrics-r\\mono-spatial-econometrics-r\\Bancos de Dados")

#### Pacotes ####
library(sf)
library(sp)
library(rgdal)
library(tmap)
library(raster)
library(tidyverse)

library(cleangeo) # Correcao topologica
library(spdep) # Dependencia espacial
library(pgirmess) # Correlograma de distancia
library(spatialreg) # Regressao espacial global
library(spgwr) # Regressao ponderada geografica

#### Importar Dados ####
st_layers("aula8.gpkg")

setores_sf <- st_read("aula8.gpkg", layer = "setores_abc")

# 1- Municípios da Região Metropolitana de São Paulo
# 2- Setores Censitários do ABC Paulista

view(setores_sf)

# Alguns setores tem informacoes zeradas e outros são NA, problemas de acesso, area rural, erros.
plot(setores_sf["rede_esg"], border = NA)
plot(setores_sf["Renda"], border = NA)

# Acha que tem relacao entre tratamento de esgoto e renda? 
# Areas menos urbanizadas tem menos acesso a esgoto. 
# Existe uma certa relação entre estas varíaveis mas não 100%

# O que fazer com valores nulos ou zerados.

  # Porque eles estã nulos ou zerados?
    # Area deserta
    # Falhas de preenchimento (inerpolar?)

  # Faz sentido avaliar estes elementos ?
    # Focar o estudo em areas nao desertas
    # Trabalhar coom amostras e não com universo dos dados

  # Os modelos aceitam dados faltantes ou zerados?
    # Zerar ao agregar em unidades maiores
    # Descartar para análise de vizinhança
    # Dados zerados podem atrapalhar regressões

#### 2. Iniciando Análise ####
#### Tratando dados nulos ####
# Pegando Renda que não seja nulo
setores_nao_nulo <- subset(setores_sf, is.na(setores_sf$Renda) == FALSE)

# Pegando não nulo e perguntando se é 0
setores_valido <- subset(setores_nao_nulo, setores_nao_nulo$Renda != 0)

#### Análise topológica - Corrigindo geometria invalida ####
# Vendo se um poligono 'bate' um com outro e não tem geometria invalida (Quando no desenho do poligono um linha se intersecta com a outra)
# Transformando em sp
setores_sp <- as(setores_valido, "Spatial")

# Fazemos pergunta se esta tudo certo
clgeo_IsValid(setores_sp)

# Análise topólogica
analise_topologica <- clgeo_CollectionReport(setores_sp)

# O summary traz info se esta faltando algo (40), estão OK (3990), isto acontece pois algumas geometrias podem ter juntado e dado nó, gerando problema
clgeo_SummaryReport(analise_topologica)

# Quais são as linhas da tabela de atributo que estão com erro
clgeo_SuspiciousFeatures(analise_topologica)

# Correção topologica, onde tiver um cruzamento de aresta de poligono ele vai colocar um ponto no meio (vertice) no local onde se cruzam, resolvendo grande parte dos problemas
setores_clean <- clgeo_Clean(setores_sp)

# Perguntando se é valido novamente
clgeo_IsValid(setores_clean)

#### 3. Critério de Vizinhança ####
# Poligonos são vizinhos um do outro
  # Contiguidade:
    # Rook (torre)
    # Queen (rainha)

# Para rainha os vizinhos são todas as 8 casas ao seu redor
# Para a torre os vizinhos são apenas 4 casas, uma vez que ele só anda nestas direções (imagem 35 min)

#### Matriz de vizinhança pelo critério de contiguidade (Se toca um ao outro é vizinho) ####
# Para fazer a matriz de vizinhança colocamos todos os elementos nas colunas e todos os elementos nas linhas repetindo
# Caso tenhamos 8 poligonos (1 até 8) teriamos uma matriz de 8 colunas e 8 linhas
# Quando eles forem vizinhos coloca o valor 1, quando não forem vizinhos e for elemento contra ele mesmo coloca 0
# imagem (37:40)

#### Matriz de vizinhança normalizada ####
# O mesmo criterio de formação é feito, se temos 8 poligonos teremos uma matriz de 5 colunas e 5 linhas
# Contudo o critério é que cada linha tem que ter uma soma final de 1
# Logo se um poligono tem 4 vizinhos, cada um pode ter peso de 0.25
# Matriz normalizada é bom para saber a influencia dos vizinhos no poligono (Este poligono influencia 25%, o outro 35% etc.)
# imagem (38:50)

#### Relação de vizinhança ####
# Cria matriz de vizinhança
vizinhanca <- poly2nb(setores_clean) # Formato SP
view(vizinhanca)

# Cada elemento da lista é um poligono e temos o valor do lado, quais poligonos ele é vizinho (poligono 1 é vizinho do 108)
# Trouze informação de vizinhanca por contiguidade

#### Contar quantos vizinhos tem cada elemento ####
view(card(vizinhanca)) 

# Elemento 1 tem 1 vizinho ...

# Criando nova coluna com quantidade de vizinhos
setoes_clean$vizinhos <- card(vizinhanca)
view(setores_clean@data) # Formato SP

# Pegando setores que tem algum vizinho, conectados entre si (Tirando setores sem vizinhos) 
setores_juntos <- subset(setores_clean, setores_clean$vizinhos != 0)
view(setores_juntos@data) # Todos poligonos tem ao menos 1 vizinho

# Criando nova matriz de vizinhanca para aqueles que tem vizinho
vizinhanca2 <- poly2nb(setores_juntos)
setores_xy <- coordinates(setores_juntos) # Cria matriz de X e Y com latitude e longitude

# Plotando setores censitarios
plot(setores_sp, border = "red")

# Plotando relações de vizinhança, indicando as coordenadas que ligam estas vizinhanças (Centroid de cada setor censitário sendo latitude e longitude)
plot(x = vizinhanca2, coord = setores_xy, cex = 0.6, add = TRUE) # Visualizando aqueles que são vizinhos entre si (Se ta ligado é pq tem vizinhança)

# Exportar grafico de vizinhanca para sp, precisa ter matriz de vizinhança, coordenadas (lat lon centroid), projeção (crs)
vizinhanca_sp <- nb2lines(vizinhanca2, coords = setores_xy, proj4string = crs(setores_valido))
plot(vizinhanca_sp)

#### Vizinhança normalizada (pesos) ####
vizinhanca_peso <- nb2listw(vizinhanca2)
view(vizinhanca_pesos) # Temos lista de vizinhos e peso (Quanto tenho 1 vizinh o peso é 1, quanto tenho 2 vizinhos o peso é 0.5)

plot(setores_sp, border = "red")

plot(vizinhanca_pesos, setores_xy, cex = 0.6, add = TRUE)

vizinhanca_pesos_sp <- listw2lines(vizinhanca_peso, coords = setores_xy, proj4string = crs(setores_sp))

plot(vizinhanca_pesos_sp)

#### 4. Vizinhança por distancia ####
# Pode utilizar bases pontuais ou centroides de poligonos
# Métodos:
  # "n" vizinhos mais proximos (considera como viinho 4 ou 5 pontos mais proximos)
  # distancia maxima (considera como vizinho todos que estão a 1km de distancia do nosso ponto)
  # Funções de distancia (wij com valores continuos) (Diferenciar o peso de cada vizinho em relação a distancia, mais proximos tem mais peso) (precisa explicitar funcao matematica, decaimento linear, decaiment exponencial negativo da distancia, decaimento negtivo exponencial da distancia) (como que a distancia afeta o peso)
  # Imagem (54min)

#### Analise de distancia de vizinhanca ####
# Entra com info de quem é vizinho e as coordenadas de cada elemento, dai ele ve a coord de cada elemento e ve a distancia para atribuir funcao
distancias <- nbdists(vizinhanca2, setores_xy)
View(distancias)

# Elemento 1 vai ter uma distancia de tanto de outros elementos
View(unlist(distancias)) # O numero 1 esta a tantos metros do elemento mais proximo
summary(unlist(distancias))

#### Vizinhos por raio de distancia ####
# Considerar como vizinho aqueles que estao a tantos metros de distancia
# Procure o vizinho mais proximo dentro da coordenada xy d1 dist min d2 dist max
vizinhanca_400m <- dnearneigh(setores_xy, d1=0, d2=40)
View(vizinhanca_400m)

# Elemento 1 esta a menos de 400 metros deste, deste
plot(vizinhanca_400m, setores_xy, cex = 0.3)

#### K vizinhos mais proximos ####
# Qual o numero de vizinhos que queremos (considere commo vizinhs os 4 pontos de centroides mais proximos)
vizinhos_4 <- knearneigh(setores_xy, k = 4)
View(vizinhos_4$nn)

# Elemento 1 - estes são os 4 vizinhos mais proximos

vizinhanca_4 <- knn2nb(vizinhos_4)
View(vizinhanca_4)

plot(setores_sp, border = "red")
plot(vizinhanca_4, setores_xy, cex = 0.6, add = TRUE) # Eu posso ser vizinho do outro mas o outro pode não ser meu vizinh

# Neste metodo k vizinhos mais proximos todos tem algum vizinh, diferente dos outros metodos (Resolve prob de vizinho isolado, se tem mts ilhas é boa opcao)

#### 5. Correlação espacial ####
#### Dependencia espacial ####
# As coisas mais proximas se parecem mais entre si do que as mais distantes - Wald Tbler (1970)
# Auto correlacao espacial (grau dedependencia espacial)
# Tobler W R 1970 A computer movie simulating urban growth in the detrid regiono 46, 234-40

# Autocorrelacao positiva (Lei de tobler) - Feicoes similares em localizacao tambem sao similares em atributos
# Autocorrelacao negativa (Oposicao a lei de tobler) - Feicoes similares em lcalizacao tendem a ter atributos menos similares do que feicoes mais distantes (O vizinho tende a ser mais diferente do que o elemento mais distante)
# Ausencia de autocorrelação - Atributos sao independentes da lcalizacao
# imagem 1h07

#### Duas formas de cacular indices de autocorrelacao espacial ####
# 1. Indices globais de associacao espacial
  # Apresenta uma medida unica para toda a area analisada (Autcorrelacao positiva, negativa ou sem autocorrelacao)
  # Indice global de moran

# 2. Indices locais de associacao espacial (LISA)
  # Decomposicoes ds indices globais, podem ser visualizados na forma de mapas
  # Permite a identificacao de diferentes regimes de associacao espacial
  # Indice local de Moran (Pode ter locais que tem autocorrelacao positiva, outro local com negativa)

#### Indice global de moran ####
  # Indice global de autocorrelaca espacial, que varia entre -1 e 1
  # Correlacao de um atributo de um elemento no espaco em relacao ao mesmo atributo nos vizinhos (Renda- "Se for vizinho sera q tende a ser semelhante?, qual a correlacao entre o elemento do poligono e o valoor dos elementos viinhos. Sera que os vizinhos são semelhantes?")
  # Extrema autocorrelacao psitiva (lei de tobler) I = 1 (Feeicoes similares em localizacao tambem sao similares em atributos)
  # Extrema autocorrelacao negativa (Oposicao a lei de Tobler) I = -1 Ficoes similares em localizacao endem a ter atributos menos similares do que feicoes mais distantes
  # Ausencia de autcorrelcao I = 0 Quando atributos sao independentes da localizacao

#### Calculando indice de moran ####
# Qual atributo estmos medindo ,ver se a renda dos vizinhos é semelhante
# Utilizar matrix normalizada (Todos os vizinhos tem influencia semelhante quando comparado com o poligono central)
moran.test(x = setores_juntos$Renda, listw = vizinhanca_pesos)

# P < 0.05 é significativo
# Moran I > 0 autocorrelacao espacial positiva (Viinho tende a ser semelhante)

#### Correlograma de indice de moran ####
# Idea do correlograma é o seguinte, primeiro voce analise em relacao ao vizinho depois voce ve em relação aos vizinhos de segundo grau, vizinho do vizinho
# O valor do rosa,o quanto ele é semelhante aos verdinhos (img 1h17)
# O valor do rosa, o quanto ele é semelhante aos amarelos
# Conforme afasta o grau de vizinhanca como é a relação

correlograma_contiguidade <- sp.correlogram(neighbours = vizinhanca2, var = setores_juntos$Renda, order = 5, method = "l")
plot(correlograma_contiguidade)

# Quando passa para o segundo vizinho (lags) temos uma caida da semelhanca, quanto esta no 5 grau de vizinhaca a vizinhanca nao influencia tanto o atributo de renda
# Para todos eles temos P < 0.5 (n aleatorio)
# Ele usa a distancia (Todos os vizinhos q estao a 100 metros qual é autocorrelacao espacial, depois 200 metros (no nosso caso a distanncia entre os pontos, centroides dos poligonos))
correlograma_distancia <- correlog(setores_xy, setores_juntos$Renda)
plot(correlograma_distancia)
correlograma_distancia

# Começa alto e vai diminuindo com aumento da distancia
# nem todos são P < 0.05

#### Matriz viinhanc para ordens superiores #### 
# Vamo falar que é vizinho nao so o primeiro mas o segund vizinho
vizinhanca_1e2 <- nblag(vizinhanca2, maxlag = 2) # expande nivel vizinhanca

# Pega a matriz de vizinhanca e acumula ate chegar a ordem 2
# 2 atributos (Primeiro para 1 vizinho eo segundo para o vizinho do vizinho, vizinhanca 2)

# Juntar vizinho de primeiro e segundo grau e falar q é tudo vizinho
vizinhanca_ordem_2 <- nblag_cumul(vizinhanca_1e2)
View(vizinhanca_ordem_2)

#### Vizinhanca acumulada de ordens superiores (2 ordes agrupadas) ####
# Normalizar primeiro
vizinhanca_peso_ordem_2 <- nb2listw(vizinhanca_ordem_2) # Se tiver 10 vizinhos cada um vai ter 0.1 de peso e em seguida calcula teste de moran (considerando vizinho 1 e 2 junts)
moran.test(x = setores_juntos$Renda, listw = vizinhanca_peso_ordem_2)

# Autocorrelacao positiva mas menor do que a de ordem 1
# P < 0.05 é significativo (Pode acontcer que a de ordem 2 ou 3 tenha mais autocorrelacao do que a de 1 ordem) (Qual tipo de vizinhanca traduz melhor a correlacao espacial)

#### Indicadors Locais de Associacao Espacil (LISA) ####
# Para cada objeto vamos considerar a relacao com os vizinhos e falar se este objeto esta autocorrelacionado com seus vizinhos
# Valor especifico para cada objeto
# Identificacao de:
  # Clusters: Objetos com valores de atributos semelhantes
  # Outliers: Objetos anomalos em relacao aos vizinhos
  # Regimes espaciais distintos

# Indice local de moran
# Como o atributo de um objeto esta correlacionado ou nao com os seus vizinhos
localmoran <- localmoran(x = setores_juntos$Renda, listw = vizinhanca_pesos)
View(localmoran)

# Resultdo tabela, onde cada elemento ele fala o indice de moran li, se 0,2 é positivo 0,5 positvo ee mais forte
# valor P < 0.05 é significativo 

class(localmoran)

localmoran_df <- as.data.frame(localmoran)
setores_juntos$moran <- localmoran_df$li
setores_juntos$moran_p <- localmoran_df$`Pr(z>0)`
View(setores_juntos@data)
summary(setores_juntos$moran)

# Indice local é decomposicao do indice geral

# Pega o arquivos setores juntos e desenhe o moran com intervalos fixo
tm_shape(setores_juntos) +
  tm_fill("moran", style = "fixed", breaks = c(-3, 0, 0.2, 0.5, 30),
  pallette = c("red", "lightblue", "blue", "blue4"))

# Lugares vermelhos sao discrepantes (correlacao negativa, muito dif dos vizinhos)

tm_shape(setores_juntos) +
  tm_fill("moran", style = "fixed", breaks = c(0, 0.01, 0.05, 1),
          pallette = c("darkblue", "blue", "blue4"))

# Tentar cruzar 2 mapas, onde é significativo

#### Diagrama de espalhamento de Moran ####
# img 1h39

# Cada ponto é o valor de um atributo (renda)
# Eixo y e a media dos valores dos vizinhos (Padronizar, 0 é a media)
# Criar quadrantes onde pode separar valor alto de renda no local e os vizinhos tmb tem valor alto Q1 autocorrelacao positiva
# Poligonos quem tem valor baixo de renda e os vizinhos tem valor baixo Q2
# Poligono tem renda baixa e os vizinhos tem renda alta Q3
# img 1h42

#### Spatial Lag - Média dos valores dos vizinhos ####
# Qual q é a media dos vizinhs
# Pega vizinhanca ponderada e usa ela pra calcular a media das rendas dos vizinhos
# Calcular o valor dos vizinhos e colocar em um setor censitario sem informacao
setores_juntos$lag_renda <- lag.listw(vizinhanca_pesos, var = setores_juntos$Renda)
View(setores_juntos@data)

tm_shape(setores_juntos) + tm_fill("lag_renda", style = "quantile")

#### Diagrama espalhamento de Moran ####
# Qual o atributo? Renda. Qual a matriz de vizinhanca
moran.plot(x = setores_juntos$Renda, listw = vizinhanca_pesos, cex = 0.6, labels = FALSE)

#### Lisa MAPS ####
# Cada fator H acima da media L abaixo da media no poligono
L1 <- factor(setores_juntos$Renda < mean(setores_juntos$Renda), labels = c("H", "L"))

# Vizinhos fator H acima da media L abaixo da media
L2 <- factor(setores_juntos$lag_renda < mean(setores_juntos$lag_renda), labels = c("H", "L"))

setores_juntos$lisa <- paste(L1, L2)

tm_shape(setores_juntos) +
  tm_fill("lisa", palette = c("blue", "green", "yellow", "red"))

View(setores_juntos@data)

# Mapear só setores com valor p abaixo de 0.05
lisa_map <- setores_juntos[setores_juntos$moran_p <= 0.05, ]
table(lisa_map$lisa)
tm_shape(setores_sp) + tm_borders() +
  tm_shape(lisa_map) + tm_fill("lisa", palette = c("blue", "red"))

#### Suavização espacial ####
# Suavvizacao por estimadores bayesianos empiricos 
# Casos de risco em lcais com baixa populacao
  # Germ altas taxxas de risco
  # Podem ser gerados por acaso

# as vezes uma ocorrencia em um local cm baixa populacao no momento que se divide pela populacao pode gerar um risco grande 
# Pode-se redistribuir o risco dos locais com baixa populacao para as demais areas
# Qual o risco real de aconteccer algo naquele local
  # Altera de risco observado (Qnt_casos/Pop) para previsão do risco (qual a chance de acontecer no futuro)

# Metodo global
  # Redistribuir para todas as demais regioes (Aconteceu aqui, uma parte do risco é ao acaso e poderia ter acontecido em qualquer local)

# Metodo local
  # Redistribui risco para os vizinhs
  # Usa estrutura de autocorrelacao espacial
# img 2h01
# Valores suavizados sao melhores para prever o futuro com base no que conhecemos do passado

# Suavizacao por estimadores bayesianos empiricos (Global)
bayes_global <- EBest(n = setores_juntos$deslizam, x = setores_juntos$Pessoas)
# n = Casos de risco
# x = Populacao de risco
View(bayes_global)
#df com 2 colunas (1 é risco observado n/x a segunda é o estimado, oq deve acontecer futuro)
setores_juntos$desl_pes <- bayes_global$raw
tm_shape(setores_juntos) + tm_fill("desl_pes", style = "fisher")

setores_juntos$beyes_gl <- bayes_global$estmm
tm_shape(setores_juntos) + tm_fill("beyes_gl", style = "fisher")

# Suavizacao por estimadores bayesiano empirico (Local)
bayes_local <- EBlocal(ri = setores_juntos$deslizam, ni = setores_juntos$Pessoas, nb = vizinhanca2)
View(bayes_local)

setore_juntos$bayes_lc <- bayes_local$est
tm_shape(setores_juntos) + tm_fill("bayes_lc", style = "fisher")

#### Regressao espacial ####
plot(data = setores_juntos, Renda ~ rede_esg)
regressao_convencional <- lm(data = setores_juntos, Renda ~ rede_esg)
summary(regressao_convencional)
AIC(regresao_convencional)

plot(data=setores_juntos, Renda ~ rede_esg)
abline(regressao_convencional, col = "red", lwd = 2)

# Analise dos residuos
  # Medir a autocorrelacao espacial dos residuos da regressao (ndice de moran dos residuos)

# Se houver autocorrelacao espacial
  # Pode haver alguma variavel ou padro espacial que nao foi investigada pelo modelo
  # Possibilidade de aplicar modelos de regressao espacial

# Analise dos residuos
View(regressao_convencional$residuals)

setores_juntos$residuos <- regressao_convencional$residuals
tm_shape(setores_juntos) +
  tm_fill("residuos", style = "quantile", pallete = heat.colors(5))

lm.morantest(regressao_convencional, listw = vizinhanca_pesos)
# Os vizinhos tem autocorrelacao positiva e o valor p é menor q 0.05

#### Regressao espacial ####
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

  #  Alternativas
  # Spatial lag models: Atribuem a autocorrelacao espacial a vriavel resposta Y (Se eu sei o valor y dos vizinhos, alem dos valores x que tenho no mapa inteiro, posso pegar o valor y dos meus vizinhos para calcular e ajudar a inferir o valor y do meu dado)
