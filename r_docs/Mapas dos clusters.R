# Agora, vamos plotar os clusters num mapa para entender melhor a visualização

## Pacotes necessários
pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra,geobr,reshape2,sf,magrittr)
##

##Trocando o nome para não confundir
cluster1 <- teste1
cluster2 <- teste12

#Códigos de municípios voltando a ser uma coluna para plotagem
cluster1$CO_MUNICIPIO <- nome
cluster2$CO_MUNICIPIO <- nome2
rownames(cluster1) <- NULL
rownames(cluster2) <- NULL

#Agora, plotando os municípios, cada um no cluster em que está inserido (1,2,3 ou 4)

options(timeout = 4000000)
metadata<-download_metadata()
head(metadata)

#Agora sequestrar a coluna de geoplotagem
all_mun <-read_municipality()
all_mun <- all_mun %>%
  select(code_muni,geom)

#Refazendo e ajustando o passo feito anteriormente para adequar ao nome do banco do {geobr}
cluster1$code_muni <- nome
cluster2$code_muni <- nome2
cluster1 <- cluster1 %>%
  select(IVS,form,VL_OBSERVADO_2019,cluster,code_muni)
cluster2 <- cluster2 %>%
  select(IVS,formesp,VL_OBSERVADO_2019,cluster,code_muni)

#Juntando agora o nosso banco com o banco do geobr
cluster1 <- merge(cluster1,all_mun, by = "code_muni")
cluster2 <- merge(cluster2,all_mun, by = "code_muni")

#Agora, finalmente, a plotagem

#Primeiro mapa: Municípios com formação docente superior qualquer
ggplot() +
  geom_sf(data=cluster1, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)

#Segundo mapa: Municípios com formação docente superior específica
ggplot() +
  geom_sf(data=cluster2, aes(geometry=geom,fill=cluster), color= NA, size=.15)+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)