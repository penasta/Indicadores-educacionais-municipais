# Agora, vamos plotar os clusters num mapa para entender melhor a visualização

## Pacotes necessários
pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra,geobr,reshape2,sf,magrittr,purrr)
##

##Trocando o nome para não confundir
cluster2 <- teste12

#Códigos de municípios voltando a ser uma coluna para plotagem
cluster2$CO_MUNICIPIO <- nome2
rownames(cluster2) <- NULL

#bindando a tabela de clusters gerada com a de cluster artificial

cluster2 <- Reduce(rbind, list(cluster2,ca))

#Agora, plotando os municípios, cada um no cluster em que está inserido (1,2,3 ou 4)

options(timeout = 4000000)
metadata<-download_metadata()
head(metadata)

#Agora sequestrar a coluna de geoplotagem
all_mun <-read_municipality()
all_mun <- all_mun %>%
  select(code_muni,geom)

#Refazendo e ajustando o passo feito anteriormente para adequar ao nome do banco do {geobr}
#cluster2$code_muni <- nome2

colnames(cluster2) <- c('IVS','formesp','VL_OBSERVADO_2019','qtescola','cluster','code_muni')

cluster2 <- cluster2 %>%
  select(IVS,formesp,VL_OBSERVADO_2019,cluster,code_muni)

#Juntando agora o nosso banco com o banco do geobr
cluster2 <- merge(cluster2,all_mun, by = "code_muni")

#Agora, finalmente, a plotagem

#Mapa final:
ggplot() +
  geom_sf(data=cluster2, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)

clusterteste <- cluster2
cluster2$cluster <- as.factor(cluster2$cluster)

ggplot() +
  geom_sf(data=cluster2, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_manual(values = c("#386abb", "#fff505", "#f49d84","#a3a3a3"))+
  theme(panel.grid.major = element_blank(),line = element_blank(), rect = element_blank())


#Gerando a tabela para análise:

tabelateste <- cluster2 %>%
  select(IVS,formesp,VL_OBSERVADO_2019,cluster,code_muni)

#verificando o numero de municipios em cada cluster

verc1 <- tabelateste %>%
  filter (cluster == 1)
#cluster 1 = 1619 municípios
verc2 <- tabelateste %>%
  filter (cluster == 2)
#cluster 2 = 1472 municípios
verc3 <- tabelateste %>%
  filter (cluster == 3)
#cluster 3 = 1679 municípios
verc4 <- tabelateste %>%
  filter (cluster == 4)
#cluster 4 = 793 municípios

######

#Teste de  plotagem por região
#somente Cluster1; Formação genérica

#Criando coluna com codigo da  uf (2 dígitos)
cluster2$code_state <- str_sub(cluster2$code_muni,1,2)
cluster2$code_state <- as.numeric(cluster2$code_state)

#Sul
sul <- cluster2 %>%
  filter(code_state == "43" |code_state == "42" |code_state == "41")

ggplot() +
  geom_sf(data=sul, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_manual(values = c("#386abb", "#fff505", "#f49d84","#a3a3a3"))+
  theme(panel.grid.major = element_blank(),line = element_blank(), rect = element_blank())

#Sudeste
sudeste <- cluster2 %>%
  filter(code_state == "35" |code_state == "33" |code_state == "32" |code_state == "31")

ggplot() +
  geom_sf(data=sudeste, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_manual(values = c("#386abb", "#fff505", "#f49d84","#a3a3a3"))+
  theme(panel.grid.major = element_blank(),line = element_blank(), rect = element_blank())

#Centro-oeste

centro <- cluster2 %>%
  filter(code_state == "50" |code_state == "51" |code_state == "52" |code_state == "53")

ggplot() +
  geom_sf(data=centro, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_manual(values = c("#386abb", "#fff505", "#f49d84","#a3a3a3"))+
  theme(panel.grid.major = element_blank(),line = element_blank(), rect = element_blank())

#Nordeste
nordeste <- cluster2 %>%
  filter(code_state == "21" |code_state == "22" |code_state == "23" |code_state == "24" | code_state == "25" |code_state == "26" |
           code_state == "27" |code_state == "28" |code_state == "29" )

ggplot() +
  geom_sf(data=nordeste, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_manual(values = c("#386abb", "#fff505", "#f49d84","#a3a3a3"))+
  theme(panel.grid.major = element_blank(),line = element_blank(), rect = element_blank())

#Norte
norte <- cluster2 %>%
  filter(code_state == "11" |code_state == "12" |code_state == "13" |code_state == "14" | code_state == "15" |code_state == "16" |
           code_state == "17")

ggplot() +
  geom_sf(data=norte, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_manual(values = c("#386abb", "#fff505", "#f49d84","#a3a3a3"))+
  theme(panel.grid.major = element_blank(),line = element_blank(), rect = element_blank())
