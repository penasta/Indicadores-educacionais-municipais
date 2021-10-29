## Pacotes necessários

pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra,readxl,xlsx)

##

######### Leitura dos bancos: RODAR APENAS OS INDICADORES DESEJADOS

idhivs <- readRDS("../Git/tabelas de indicadores em RDS/IDHM e IVS.rds")
adequadoc <- readRDS("../Git/tabelas de indicadores em RDS/Indicador de adequação de formação do docente.rds")
perfundoc <- readRDS("../Git/tabelas de indicadores em RDS/Percentual de Funções Docentes com Curso Superior.rds")
proficiencia <- readRDS("../Git/tabelas de indicadores em RDS/Proficiencia por Municipios.rds")

rendimento <- readRDS("../Git/tabelas de indicadores em RDS/Taxa de rendimento escolar.rds")
IDEB <- readRDS("../Git/tabelas de indicadores em RDS/IDEB.rds")
IVSCH <- readRDS("../Git/tabelas de indicadores em RDS/IVSCH.rds")

tam <- read_excel("../Git/Tabelas provisórias/Tab_municipio.xlsx")
colnames(tam) <- c('CO_MUNICIPIO','qtescola','tamanho','c4','c5','c6','c7')
tam$c4 <- NULL
tam$c5 <- NULL
tam$c6 <- NULL
tam$c7 <- NULL

summary(tam)
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
#
#IVS <- idhivs %>%
#  select(CO_MUNICIPIO,IVS)
#
#Primeiro teste de IVS: Descontinuado
#
#


# Preparando o IVS Capital Humano
colnames(IVSCH) <- c('UF','c2','CO_MUNICIPIO','c4','c5','c6','c7','IVS','c9','c10','c11','c12','c13','c14')
IVSCH <- as.data.frame(IVSCH)

IVSCH$CO_MUNICIPIO <- as.numeric(IVSCH$CO_MUNICIPIO)
IVSCH$IVS <- as.numeric(IVSCH$IVS)

IVS <- IVSCH %>%
  select(CO_MUNICIPIO,IVS)

IDEB <- IDEB %>%
  select(CO_MUNICIPIO,VL_OBSERVADO_2019)


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

#Agora, o segundo teste, considerando formação específica

#Criando o cluster artificial dos municípios sem nota:

ca <- merge(IVS,formesp, by = "CO_MUNICIPIO")
ca <- merge(ca,IDEB, by = "CO_MUNICIPIO",all=TRUE)
ca <- merge(ca,tam, by = "CO_MUNICIPIO",all=TRUE)

ca <- ca %>%
  select(CO_MUNICIPIO,IVS,formesp,VL_OBSERVADO_2019,qtescola)

ca[ca=='NA'] <- NA

ca <- ca[is.na(ca$VL_OBSERVADO_2019),]

ca$cluster <- 4


  ######
summary(IDEB)
summary(IVS)
summary(formesp)

teste2 <- merge(IVS,formesp, by = "CO_MUNICIPIO")
teste2 <- merge(teste2,IDEB, by = "CO_MUNICIPIO",all=TRUE)
teste2 <- merge(teste2,tam, by = "CO_MUNICIPIO",all=TRUE)

nome2 <- c(teste2$CO_MUNICIPIO)

summary(nome2)
summary(teste2)

teste2 <- teste2 %>%
  select(IVS,formesp,VL_OBSERVADO_2019,qtescola)

row.names(teste2) <- c(nome2)

summary(teste2)

teste2 <- na.omit(teste2)

summary(teste2)


summary((centered.x2 <- scale(teste2)))

#Testando a clusterização:


fviz_nbclust(centered.x2, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)


testek2 <- kmeans(centered.x2, centers= 3, iter.max=10, nstart=1)

set.seed(123)
testek2=kmeans(testek2, 3, nstart=25)
print(testek2)

infocluster2 <- aggregate(teste2, by=list(cluster=testek2$cluster), mean)

teste12=cbind(teste2, cluster=testek2$cluster)
head(teste12)

#Sucesso!

#Agora, a visualização:

fviz_cluster(testek2, data=teste12,
             palette = c("#2E9FDF", "black", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)

# QED


#Gerando a tabela para dar merge no excel:


tabelateste$code_muni <- as.numeric(tabelateste$code_muni)
summary(tabelateste)

colnames(tabelateste) <- c("IVS","formesp","VL_OBSERVADO_2019","cluster","CO_MUNICIPIO")

summary(IVSCH)

colnames(IVSCH) <- c('UF','Nome da UF','CO_MUNICIPIO','Nome do Município','Município com 6 dígitos','Ano',
                     'IVS','IVS Capital Humano','Taxa de analfabetismo - 18 anos ou mais','Taxa de analfabetismo - 25 anos ou mais',
                     '% dos ocupados com médio completo - 18 anos ou mais',
                     '% de pessoas de 15 a 24 anos que não estudam, não trabalham e possuem renda domiciliar per capita igual ou inferior a meio salário mínimo (de 2010)',
                     'Taxa de analfabetismo - 18 anos ou mais','c14')
IVSCH$c14 <- NULL

merge <- tabelateste %>%
  select(formesp,VL_OBSERVADO_2019,cluster,CO_MUNICIPIO)

tabelaf <- merge(IVSCH,merge, by = "CO_MUNICIPIO")
tabelaf <- merge(tabelaf,tam, by = "CO_MUNICIPIO")

write.xlsx(tabelaf, file = "tabela.xlsx",
           sheetName = "Tabela", append = FALSE)

#Agora, capturando as medidas de cada cluster

c1 <- tabelateste %>%
  filter (cluster == '1')%>%
  select(IVS,formesp,VL_OBSERVADO_2019)

sc1 <- summary(c1)
vc1  <- var(c1)

write.xlsx(sc1, file = "medidascluster1.xlsx",
           sheetName = "Tabela", append = FALSE)
write.xlsx(vc1, file = "varianciacluster1.xlsx",
           sheetName = "Tabela", append = FALSE)

c2 <- tabelateste %>%
  filter (cluster == '2')%>%
  select(IVS,formesp,VL_OBSERVADO_2019)

sc2 <- summary(c2)
vc2 <- var(c2)

write.xlsx(sc2, file = "medidascluster2.xlsx",
           sheetName = "Tabela", append = FALSE)
write.xlsx(vc2, file = "varianciacluster2.xlsx",
           sheetName = "Tabela", append = FALSE)

c3 <- tabelateste %>%
  filter (cluster == '3')%>%
  select(IVS,formesp,VL_OBSERVADO_2019)

sc3 <- summary(c3)
vc3 <- var(c3)

write.xlsx(sc3, file = "medidascluster3.xlsx",
           sheetName = "Tabela", append = FALSE)
write.xlsx(vc3, file = "varianciacluster3.xlsx",
           sheetName = "Tabela", append = FALSE)