pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra)

idhivs <- readRDS("../Git/tabelas de indicadores em RDS/IDHM e IVS.rds")
gestao <- readRDS("../Git/tabelas de indicadores em RDS/Complexidade de gestão da escola.rds")
adequadoc <- readRDS("../Git/tabelas de indicadores em RDS/Indicador de adequação de formação do docente.rds")
esfdoc <- readRDS("../Git/tabelas de indicadores em RDS/Indicador de esforço docente.rds")
inse <- readRDS("../Git/tabelas de indicadores em RDS/INSE.rds")
ird <- readRDS("../Git/tabelas de indicadores em RDS/IRD.rds")
medaltur <- readRDS("../Git/tabelas de indicadores em RDS/Média de alunos por turma.rds")
medhad <- readRDS("../Git/tabelas de indicadores em RDS/Média de Horas-Aula Diária.rds")
perfundoc <- readRDS("../Git/tabelas de indicadores em RDS/Percentual de Funções Docentes com Curso Superior.rds")
proficiencia <- readRDS("../Git/tabelas de indicadores em RDS/Proficiencia por Municipios.rds")
distidser <- readRDS("../Git/tabelas de indicadores em RDS/Taxa de distorção idade série.rds")
nresp <- readRDS("../Git/tabelas de indicadores em RDS/Taxa de Não-Resposta.rds")
rendimento <- readRDS("../Git/tabelas de indicadores em RDS/Taxa de rendimento escolar.rds")

#preparando um DF teste; Arbitrariamente agregando os DF com informações de IDHM, IVS e Complexidade de gestão da escola...

teste <- merge(idhivs,gestao,
      by = "CO_MUNICIPIO")

testetotal <- teste %>%
  filter (NO_CATEGORIA=='Total') %>%
  filter (NO_DEPENDENCIA=='Total') %>%
  select(CO_MUNICIPIO,IVS,IDHM,EDU_BAS_CAT_1,EDU_BAS_CAT_2,EDU_BAS_CAT_3,EDU_BAS_CAT_4,EDU_BAS_CAT_5,EDU_BAS_CAT_6)

nome <- c(testetotal$CO_MUNICIPIO)

testetotal <- testetotal %>%
  select(IVS,IDHM,EDU_BAS_CAT_1,EDU_BAS_CAT_2,EDU_BAS_CAT_3,EDU_BAS_CAT_4,EDU_BAS_CAT_5,EDU_BAS_CAT_6)

row.names(testetotal) <- c(nome)

summary(testetotal)

testetotal <- testetotal[is.na(testetotal)] = 0

summary((centered.x <- scale(testetotal)))

#Testando a clusterização:


fviz_nbclust(centered.x, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)


testek <- kmeans(centered.x, centers=4, iter.max=10, nstart=1)

set.seed(123)
testek=kmeans(testek, 4, nstart=25)
print(testek)

infocluster <- aggregate(testetotal, by=list(cluster=testek$cluster), mean)

testetotal2=cbind(testetotal, cluster=testek$cluster)
head(testetotal2)

#Sucesso!

#Agora, a visualização:

fviz_cluster(testek, data=testetotal2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)