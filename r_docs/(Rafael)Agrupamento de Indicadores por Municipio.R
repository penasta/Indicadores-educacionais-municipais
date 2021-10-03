library(readr)
library(dplyr)
TS_ALUNO_34EM <- read_csv("C:/Users/rafae/Downloads/microdados_saeb_2019/DADOS/TS_ALUNO_34EM.csv")


Resultados <- TS_ALUNO_34EM %>%
  select(ID_MUNICIPIO,PROFICIENCIA_LP,PROFICIENCIA_LP_SAEB,PROFICIENCIA_MT,PROFICIENCIA_MT_SAEB)%>%
  group_by(ID_MUNICIPIO)%>%
  summarise(PROFICIENCIA_LP = mean(PROFICIENCIA_LP,na.rm=T),
         PROFICIENCIA_LP_SAEB = mean(PROFICIENCIA_LP_SAEB,na.rm=T),
         PROFICIENCIA_MT = mean(PROFICIENCIA_MT,na.rm=T),
         PROFICIENCIA_MT_SAEB = mean(PROFICIENCIA_MT_SAEB,na.rm=T),
         ALUNOS=n())
write.csv(Resultados,"Proficiencia por Municipios.csv")
