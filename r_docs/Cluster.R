## Pacotes necessários

pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra)

##

######### Leitura dos bancos: RODAR APENAS OS INDICADORES DESEJADOS

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
IDEB <- readRDS("../Git/tabelas de indicadores em RDS/IDEB.rds")

#########

#
# Indicadores escolhidos para este primeiro teste de clusterização: IDEB(IDEB),IVS(idhivs),adequação de formação do docente(adequadoc)
#

# Ajustando os nomes das colunas para facilitar a programação

colnames(IDEB) <- c('CO_MUNICIPIO','NO_DEPENDENCIA','VL_APROVACAO_2019_SI_4','VL_INDICADOR_REND_2019','VL_NOTA_MEDIA_2019','VL_OBSERVADO_2019','VL_PROJECAO_2019')

# Filtragens para trabalharmos apenas com escolas da rede pública, conforme deliberação

IDEB <- IDEB %>%
  filter (NO_DEPENDENCIA=='Pública')

adequadoc <- adequadoc %>%
  filter (NO_DEPENDENCIA=='Pública')%>%
  filter (NO_CATEGORIA=='Total')

# Criando uma variável 0-1 com a proporção de docentes com alguma formação superior qualquer

form <- adequadoc %>%
  select(CO_MUNICIPIO,MED_CAT_1,MED_CAT_2,MED_CAT_3,MED_CAT_4,MED_CAT_5)

form$form <- (100-form$MED_CAT_5)/100

summary(form)

form <- form %>%
  select(CO_MUNICIPIO,form)

# Criando uma variável 0-1 com a proporção de docentes com formação específica na área em que leciona

formesp <- adequadoc %>%
  select(CO_MUNICIPIO,MED_CAT_1,MED_CAT_2,MED_CAT_3,MED_CAT_4,MED_CAT_5)

formesp$formesp <- (100-(formesp$MED_CAT_3+formesp$MED_CAT_4+formesp$MED_CAT_5))/100

summary(formesp)

formesp <- formesp %>%
  select(CO_MUNICIPIO,formesp)

summary(formesp$formesp)
summary(form$form)

summary(idhivs)

# Selecionando somente o IVS 2015

IVS <- idhivs %>%
  select(CO_MUNICIPIO,IVS)

#Trocando a lógica do IVS: originalmente, quanto mais próximo de 1, pior. Vou inverter a lógica para se adaptar a lógica dos outros indicadores (quanto mais próximo de 0, pior)
IVS$IVS <- 1-(IVS$IVS)

summary(IDEB$VL_OBSERVADO_2019)


IDEB <- IDEB %>%
  select(CO_MUNICIPIO,VL_OBSERVADO_2019)
  
# Padronizando para se tornar uma variável 0-1

IDEB$VL_OBSERVADO_2019 <- IDEB$VL_OBSERVADO_2019/8

#Vamos aos testes os indicadores selecionados para o teste(IDEB, IVS, Formação docente) foram arbitrariamente padronizados para 0-1
#Primeiramente vamos testar com docentes com algum tipo de formação superior

summary(IDEB)
summary(IVS)
summary(form)

teste <- merge(IVS,form, by = "CO_MUNICIPIO")
teste <- merge(teste,IDEB, by = "CO_MUNICIPIO",all=TRUE)

nome <- c(teste$CO_MUNICIPIO)

summary(nome)
summary(teste)

teste <- teste %>%
  select(IVS,form,VL_OBSERVADO_2019)

row.names(teste) <- c(nome)

summary(teste)

teste[is.na(teste)] = 0

summary(teste)


summary((centered.x <- scale(teste)))

#Testando a clusterização:


fviz_nbclust(centered.x, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)


testek <- kmeans(centered.x, centers=4, iter.max=10, nstart=1)

set.seed(123)
testek=kmeans(testek, 4, nstart=25)
print(testek)

infocluster <- aggregate(teste, by=list(cluster=testek$cluster), mean)

teste1=cbind(teste, cluster=testek$cluster)
head(teste1)

#Sucesso!

#Agora, a visualização:

fviz_cluster(testek, data=teste1,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)



#Agora, o segundo teste, considerando formação específica


summary(IDEB)
summary(IVS)
summary(formesp)

teste2 <- merge(IVS,formesp, by = "CO_MUNICIPIO")
teste2 <- merge(teste2,IDEB, by = "CO_MUNICIPIO",all=TRUE)

nome2 <- c(teste2$CO_MUNICIPIO)

summary(nome2)
summary(teste2)

teste2 <- teste2 %>%
  select(IVS,formesp,VL_OBSERVADO_2019)

row.names(teste2) <- c(nome2)

summary(teste2)

teste2[is.na(teste2)] = 0

summary(teste2)


summary((centered.x2 <- scale(teste2)))

#Testando a clusterização:


fviz_nbclust(centered.x2, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)


testek2 <- kmeans(centered.x2, centers=4, iter.max=10, nstart=1)

set.seed(123)
testek2=kmeans(testek2, 4, nstart=25)
print(testek2)

infocluster2 <- aggregate(teste2, by=list(cluster=testek2$cluster), mean)

teste12=cbind(teste2, cluster=testek2$cluster)
head(teste12)

#Sucesso!

#Agora, a visualização:

fviz_cluster(testek2, data=teste12,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)

# QED