# Agora, vamos plotar os clusters num mapa para entender melhor a visualização
pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra,geobr,reshape2)


brasil <- read_municipality(code_muni = "all", year = 2015, simplified = TRUE) 

cluster1 <- teste1
cluster2 <- teste12

cluster1$CO_MUNICIPIO <- nome
cluster2$CO_MUNICIPIO <- nome2
rownames(cluster1) <- NULL
rownames(cluster2) <- NULL

ggplot() +
  geom_sf(data=cluster1, aes(fill=cluster), color= NA, size=.15)