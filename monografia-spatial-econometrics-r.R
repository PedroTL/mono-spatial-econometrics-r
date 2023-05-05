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

