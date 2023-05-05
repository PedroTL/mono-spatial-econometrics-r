#### 1. Exercicio 1 Fazer verificação de correção topologica, vizinhança e vizihança normalizada para os municipios da RMSP ####
mun_rmsp <- st_read("aula8.gpkg", layer = "mun_rmsp")

# Alguns setores tem informacoes zeradas e outros são NA, problemas de acesso, area rural, erros.
plot(mun_rmsp["rede_esg"], border = NA)
plot(mun_rmsp["renda"], border = NA)

#### Tratando dados nulos ####
# Pegando Renda que não seja nulo
setores_nao_nulo_rmsp <- subset(mun_rmsp, is.na(mun_rmsp$renda) == FALSE)

# Pegando não nulo e perguntando se é 0
setores_valido_rmsp <- subset(setores_nao_nulo_rmsp, setores_nao_nulo_rmsp$renda != 0)

#### Análise topológica - Corrigindo geometria invalida ####
# Vendo se um poligono 'bate' um com outro e não tem geometria invalida (Quando no desenho do poligono um linha se intersecta com a outra)
# Transformando em sp
setores_sp_rmsp <- as(setores_valido_rmsp, "Spatial")

# Fazemos pergunta se esta tudo certo
clgeo_IsValid(setores_sp_rmsp) # TRUE

# Análise topólogica
analise_topologica_rmsp <- clgeo_CollectionReport(setores_sp_rmsp)

# O summary traz info se esta faltando algo (40), estão OK (3990), isto acontece pois algumas geometrias podem ter juntado e dado nó, gerando problema
clgeo_SummaryReport(analise_topologica_rmsp)

# Tudo OK

#### Relação de vizinhança ####
# Cria matriz de vizinhança
vizinhanca_rmsp <- poly2nb(setores_sp_rmsp) # Formato SP

# Cada elemento da lista é um poligono e temos o valor do lado, quais poligonos ele é vizinho (poligono 1 é vizinho do 108)
# Trouze informação de vizinhanca por contiguidade

#### Contar quantos vizinhos tem cada elemento ####
View(card(vizinhanca)) 

# Elemento 1 tem 4 vizinho ...

# Criando nova coluna com quantidade de vizinhos
setores_sp_rmsp$vizinhos_rmsp <- card(vizinhanca_rmsp)
View(setores_sp_rmsp@data) # Formato SP

# Pegando setores que tem algum vizinho, conectados entre si (Tirando setores sem vizinhos) 
setores_juntos_rmsp <- subset(setores_sp_rmsp, setores_sp_rmsp$vizinhos_rmsp != 0)
View(setores_juntos@data) # Todos poligonos tem ao menos 1 vizinho

# Criando nova matriz de vizinhanca para aqueles que tem vizinho
vizinhanca2_rmsp <- poly2nb(setores_juntos_rmsp)
setores_xy_rmsp <- coordinates(setores_juntos_rmsp) # Cria matriz de X e Y com latitude e longitude

# Plotando setores censitarios
plot(setores_sp_rmsp, border = "red")

# Plotando relações de vizinhança, indicando as coordenadas que ligam estas vizinhanças (Centroid de cada setor censitário sendo latitude e longitude)
plot(x = vizinhanca2_rmsp, coord = setores_xy_rmsp, cex = 0.6, add = TRUE) # Visualizando aqueles que são vizinhos entre si (Se ta ligado é pq tem vizinhança)

# Exportar grafico de vizinhanca para sp, precisa ter matriz de vizinhança, coordenadas (lat lon centroid), projeção (crs)
vizinhanca_sp_rmsp <- nb2lines(vizinhanca2_rmsp, coords = setores_xy_rmsp, proj4string = crs(setores_valido_rmsp))
plot(vizinhanca_sp_rmsp)

#### Vizinhança normalizada (pesos) ####
vizinhanca_peso_rmsp <- nb2listw(vizinhanca2_rmsp)
View(vizinhanca_pesos_rmsp) # Temos lista de vizinhos e peso (Quanto tenho 1 vizinh o peso é 1, quanto tenho 2 vizinhos o peso é 0.5)

plot(setores_sp_rmsp, border = "red")

plot(vizinhanca_peso_rmsp, setores_xy_rmsp, cex = 0.6, add = TRUE)

vizinhanca_peso_sp_rmsp <- listw2lines(vizinhanca_peso_rmsp, coords = setores_xy_rmsp, proj4string = crs(setores_sp_rmsp))

plot(vizinhanca_peso_sp_rmsp)

#### 2. Fazer analise de distancias, vizinhanca com o raio de distancia media e com 3 vizinhos mais proximos dos municipios da RMSP, visualizando os mapas ####
