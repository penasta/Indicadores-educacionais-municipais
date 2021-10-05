library(readr)
library(dplyr)
library(readxl)
library(tidyverse)

Proficiencia_por_Municipios <- read_csv("C:/Users/Bruno/Desktop/Nova pasta/Proficiencia por Municipios.csv")
write_rds(Proficiencia_por_Municipios,"Proficiencia por Municipios.rds")

Complexidade_de_gest?o_da_escola <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/Complexidade de gest?o da escola_MUNICIPIOS_2019.xlsx")

#rodei o c?digo abaixo umas 8 vezes para o cabe?alho ir subindo
names(Complexidade_de_gest?o_da_escola) <- Complexidade_de_gest?o_da_escola[1,]
Complexidade_de_gest?o_da_escola <- Complexidade_de_gest?o_da_escola[-1,]

summary(Complexidade_de_gest?o_da_escola)

Complexidade_de_gest?o_da_escola <- as_tibble(Complexidade_de_gest?o_da_escola)

Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_1 <- as.numeric(Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_1)
Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_2 <- as.numeric(Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_2)
Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_3 <- as.numeric(Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_3)
Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_4 <- as.numeric(Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_4)
Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_5 <- as.numeric(Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_5)
Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_6 <- as.numeric(Complexidade_de_gest?o_da_escola$EDU_BAS_CAT_6)

Complexidade_de_gest?o_da_escola <- Complexidade_de_gest?o_da_escola %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,EDU_BAS_CAT_1,EDU_BAS_CAT_2,EDU_BAS_CAT_3,EDU_BAS_CAT_4,EDU_BAS_CAT_5,EDU_BAS_CAT_6)

write_rds(Complexidade_de_gest?o_da_escola,"Complexidade de gest?o da escola.rds")

###########################################################################################################################################################
###########################################################################################################################################################


Indicador_de_adequa??o_de_forma??o_do_docente <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/Indicador de adequa??o de forma??o do docente_MUNICIPIOS_2019.xlsx")

names(Indicador_de_adequa??o_de_forma??o_do_docente) <- Indicador_de_adequa??o_de_forma??o_do_docente[1,]
Indicador_de_adequa??o_de_forma??o_do_docente <- Indicador_de_adequa??o_de_forma??o_do_docente[-1,]

summary(Indicador_de_adequa??o_de_forma??o_do_docente)

Indicador_de_adequa??o_de_forma??o_do_docente <- as_tibble(Indicador_de_adequa??o_de_forma??o_do_docente)

Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_1 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_1)
Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_2 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_2)
Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_3 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_3)
Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_4 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_4)
Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_5 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$MED_CAT_5)
Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_1 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_1)
Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_2 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_2)
Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_3 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_3)
Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_4 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_4)
Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_5 <- as.numeric(Indicador_de_adequa??o_de_forma??o_do_docente$EJA_MED_CAT_5)




Indicador_de_adequa??o_de_forma??o_do_docente <- Indicador_de_adequa??o_de_forma??o_do_docente %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,MED_CAT_1,MED_CAT_2,MED_CAT_3,MED_CAT_4,MED_CAT_5,EJA_MED_CAT_1,EJA_MED_CAT_2,EJA_MED_CAT_3,EJA_MED_CAT_4,EJA_MED_CAT_5)


write_rds(Indicador_de_adequa??o_de_forma??o_do_docente,"Indicador de adequa??o de forma??o do docente.rds")


###########################################################################################################################################################
###########################################################################################################################################################



Indicador_de_esfor?o_docente <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/Indicador de esfor?o docente_MUNICIPIOS_2019.xlsx")

names(Indicador_de_esfor?o_docente) <- Indicador_de_esfor?o_docente[1,]
Indicador_de_esfor?o_docente <- Indicador_de_esfor?o_docente[-1,]

summary(Indicador_de_esfor?o_docente)

Indicador_de_esfor?o_docente <- as_tibble(Indicador_de_esfor?o_docente)

Indicador_de_esfor?o_docente$MED_CAT_1 <- as.numeric(Indicador_de_esfor?o_docente$MED_CAT_1)
Indicador_de_esfor?o_docente$MED_CAT_2 <- as.numeric(Indicador_de_esfor?o_docente$MED_CAT_2)
Indicador_de_esfor?o_docente$MED_CAT_3 <- as.numeric(Indicador_de_esfor?o_docente$MED_CAT_3)
Indicador_de_esfor?o_docente$MED_CAT_4 <- as.numeric(Indicador_de_esfor?o_docente$MED_CAT_4)
Indicador_de_esfor?o_docente$MED_CAT_5 <- as.numeric(Indicador_de_esfor?o_docente$MED_CAT_5)
Indicador_de_esfor?o_docente$MED_CAT_6 <- as.numeric(Indicador_de_esfor?o_docente$MED_CAT_6)


Indicador_de_esfor?o_docente <- Indicador_de_esfor?o_docente %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,MED_CAT_1,MED_CAT_2,MED_CAT_3,MED_CAT_4,MED_CAT_5,MED_CAT_6)


write_rds(Indicador_de_esfor?o_docente,"Indicador de esfor?o docente.rds")


###########################################################################################################################################################
###########################################################################################################################################################


INSE <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/INSE_2019_MUNICIPIOS.xlsx")

names(INSE) <- INSE[1,]
INSE <- INSE[-1,]

summary(INSE)

INSE <- as_tibble(INSE)

INSE$QTD_ALUNOS_INSE <- as.numeric(INSE$QTD_ALUNOS_INSE)
INSE$MEDIA_INSE <- as.numeric(INSE$MEDIA_INSE)
INSE$TP_TIPO_REDE <- as.numeric(INSE$TP_TIPO_REDE)
INSE$TP_LOCALIZACAO <- as.numeric(INSE$TP_LOCALIZACAO)

INSE <- INSE %>%
  select(CO_MUNICIPIO,TP_TIPO_REDE,TP_LOCALIZACAO,QTD_ALUNOS_INSE,MEDIA_INSE)


write_rds(INSE,"INSE.rds")


###########################################################################################################################################################
###########################################################################################################################################################


IRD <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/IRD_MUNICIPIOS_2019.xlsx")

names(IRD) <- IRD[1,]
IRD <- IRD[-1,]

summary(IRD)

IRD <- as_tibble(IRD)

IRD$EDU_BAS_CAT_1 <- as.numeric(IRD$EDU_BAS_CAT_1)
IRD$EDU_BAS_CAT_2 <- as.numeric(IRD$EDU_BAS_CAT_2)
IRD$EDU_BAS_CAT_3 <- as.numeric(IRD$EDU_BAS_CAT_3)
IRD$EDU_BAS_CAT_4 <- as.numeric(IRD$EDU_BAS_CAT_4)


IRD <- IRD %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,EDU_BAS_CAT_1,EDU_BAS_CAT_2,EDU_BAS_CAT_3,EDU_BAS_CAT_4)

write_rds(IRD,"IRD.rds")


###########################################################################################################################################################
###########################################################################################################################################################


Media_de_Horas_Aula_Di?ria <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/M?dia de Horas-Aula Di?ria_MUNICIPIOS_2019.xlsx")

names(Media_de_Horas_Aula_Di?ria) <- Media_de_Horas_Aula_Di?ria[1,]
Media_de_Horas_Aula_Di?ria <- Media_de_Horas_Aula_Di?ria[-1,]

summary(Media_de_Horas_Aula_Di?ria)

colnames(Media_de_Horas_Aula_Di?ria) <- c('c1','c2','c3','CO_MUNICIPIO','c5','NO_CATEGORIA','NO_DEPENDENCIA','total','Serie1','Serie2','Serie3','c12','c13')

Media_de_Horas_Aula_Di?ria <- as_tibble(Media_de_Horas_Aula_Di?ria)

Media_de_Horas_Aula_Di?ria$total <- as.numeric(Media_de_Horas_Aula_Di?ria$total)
Media_de_Horas_Aula_Di?ria$Serie1 <- as.numeric(Media_de_Horas_Aula_Di?ria$Serie1)
Media_de_Horas_Aula_Di?ria$Serie2 <- as.numeric(Media_de_Horas_Aula_Di?ria$Serie2)
Media_de_Horas_Aula_Di?ria$Serie3 <- as.numeric(Media_de_Horas_Aula_Di?ria$Serie3)


Media_de_Horas_Aula_Di?ria <- Media_de_Horas_Aula_Di?ria %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,total,Serie1,Serie2,Serie3)

write_rds(Media_de_Horas_Aula_Di?ria,"M?dia de Horas-Aula Di?ria.rds")


###########################################################################################################################################################
###########################################################################################################################################################


M?dia_de_alunos_por_turma <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/M?dia de alunos por turma_MUNICIPIOS_2019.xlsx")

names(M?dia_de_alunos_por_turma) <- M?dia_de_alunos_por_turma[1,]
M?dia_de_alunos_por_turma <- M?dia_de_alunos_por_turma[-1,]

summary(M?dia_de_alunos_por_turma)

#colnames(M?dia_de_alunos_por_turma) <- c('c1','c2','c3','CO_MUNICIPIO','c5','c6','c7','Total','Serie1','Serie2','Serie3','c12','c13')

M?dia_de_alunos_por_turma <- as_tibble(M?dia_de_alunos_por_turma)

M?dia_de_alunos_por_turma$MED_CAT_0 <- as.numeric(M?dia_de_alunos_por_turma$MED_CAT_0)
M?dia_de_alunos_por_turma$MED_01_CAT_0 <- as.numeric(M?dia_de_alunos_por_turma$MED_01_CAT_0)
M?dia_de_alunos_por_turma$MED_02_CAT_0 <- as.numeric(M?dia_de_alunos_por_turma$MED_02_CAT_0)
M?dia_de_alunos_por_turma$MED_03_CAT_0 <- as.numeric(M?dia_de_alunos_por_turma$MED_03_CAT_0)


M?dia_de_alunos_por_turma <- M?dia_de_alunos_por_turma %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,MED_CAT_0,MED_01_CAT_0,MED_02_CAT_0,MED_03_CAT_0)


write_rds(M?dia_de_alunos_por_turma,"M?dia de alunos por turma.rds")


###########################################################################################################################################################
###########################################################################################################################################################


Percentual_de_Fun??es_Docentes_com_Curso_Superior <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/Percentual de Fun??es Docentes com Curso Superior_MUNICIPIOS_2019.xlsx")

names(Percentual_de_Fun??es_Docentes_com_Curso_Superior) <- Percentual_de_Fun??es_Docentes_com_Curso_Superior[1,]
Percentual_de_Fun??es_Docentes_com_Curso_Superior <- Percentual_de_Fun??es_Docentes_com_Curso_Superior[-1,]

summary(Percentual_de_Fun??es_Docentes_com_Curso_Superior)

# colnames(M?dia_de_alunos_por_turma) <- c('c1','c2','c3','CO_MUNICIPIO','c5','c6','c7','Total','Serie1','Serie2','Serie3','c12','c13')

#total: 8
#cdmun: 4

Percentual_de_Fun??es_Docentes_com_Curso_Superior <- as_tibble(Percentual_de_Fun??es_Docentes_com_Curso_Superior)

Percentual_de_Fun??es_Docentes_com_Curso_Superior$MED_CAT_0 <- as.numeric(Percentual_de_Fun??es_Docentes_com_Curso_Superior$MED_CAT_0)
Percentual_de_Fun??es_Docentes_com_Curso_Superior$EJA_CAT_0 <- as.numeric(Percentual_de_Fun??es_Docentes_com_Curso_Superior$EJA_CAT_0)



Percentual_de_Fun??es_Docentes_com_Curso_Superior <- Percentual_de_Fun??es_Docentes_com_Curso_Superior %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,MED_CAT_0,EJA_CAT_0)


write_rds(Percentual_de_Fun??es_Docentes_com_Curso_Superior,"Percentual de Fun??es Docentes com Curso Superior.rds")


###########################################################################################################################################################
###########################################################################################################################################################


Taxa_de_distor??o_idade_s?rie <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/Taxa de distor??o idade-s?rie_MUNICIPIOS_2019.xlsx")

names(Taxa_de_distor??o_idade_s?rie) <- Taxa_de_distor??o_idade_s?rie[1,]
Taxa_de_distor??o_idade_s?rie <- Taxa_de_distor??o_idade_s?rie[-1,]

summary(Taxa_de_distor??o_idade_s?rie)

colnames(Taxa_de_distor??o_idade_s?rie) <- c('c1','c2','c3','CO_MUNICIPIO','c5','NO_CATEGORIA','NO_DEPENDENCIA','Total','Serie1','Serie2','Serie3','c12')

Taxa_de_distor??o_idade_s?rie <- as_tibble(Taxa_de_distor??o_idade_s?rie)

Taxa_de_distor??o_idade_s?rie$Total <- as.numeric(Taxa_de_distor??o_idade_s?rie$Total)
Taxa_de_distor??o_idade_s?rie$Serie1 <- as.numeric(Taxa_de_distor??o_idade_s?rie$Serie1)
Taxa_de_distor??o_idade_s?rie$Serie2 <- as.numeric(Taxa_de_distor??o_idade_s?rie$Serie2)
Taxa_de_distor??o_idade_s?rie$Serie3 <- as.numeric(Taxa_de_distor??o_idade_s?rie$Serie3)


Taxa_de_distor??o_idade_s?rie <- Taxa_de_distor??o_idade_s?rie %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,Total,Serie1,Serie2,Serie3)

write_rds(Taxa_de_distor??o_idade_s?rie,"Taxa de distor??o idade s?rie.rds")


###########################################################################################################################################################
###########################################################################################################################################################


Taxa_de_N?o_Resposta <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/Taxa de N?o-Resposta_municipios_2019.xlsx")

names(Taxa_de_N?o_Resposta) <- Taxa_de_N?o_Resposta[1,]
Taxa_de_N?o_Resposta <- Taxa_de_N?o_Resposta[-1,]

summary(Taxa_de_N?o_Resposta)

#colnames(Taxa_de_distor??o_idade_s?rie) <- c('c1','c2','c3','CO_MUNICIPIO','c5','c6','c7','Total','Serie1','Serie2','Serie3','c12')

#total: 8
#cdmun: 4

Taxa_de_N?o_Resposta <- as_tibble(Taxa_de_N?o_Resposta)

Taxa_de_N?o_Resposta$TNR_MED <- as.numeric(Taxa_de_N?o_Resposta$TNR_MED)
Taxa_de_N?o_Resposta$TNR_M01 <- as.numeric(Taxa_de_N?o_Resposta$TNR_M01)
Taxa_de_N?o_Resposta$TNR_M02 <- as.numeric(Taxa_de_N?o_Resposta$TNR_M02)
Taxa_de_N?o_Resposta$TNR_M03 <- as.numeric(Taxa_de_N?o_Resposta$TNR_M03)


Taxa_de_N?o_Resposta <- Taxa_de_N?o_Resposta %>%
  select(CO_MUNICIPIO,TIPOLOCA,DEPENDAD,TNR_MED,TNR_M01,TNR_M02,TNR_M03)

write_rds(Taxa_de_N?o_Resposta,"Taxa de N?o-Resposta.rds")



###########################################################################################################################################################
###########################################################################################################################################################


Taxa_de_rendimento_escolar <- read_excel("D:/Arquivos/unb2021/LabEst/Atualiza??es/Tabelas/Taxa de rendimento escolar_municipios_2019.xlsx")

names(Taxa_de_rendimento_escolar) <- Taxa_de_rendimento_escolar[1,]
Taxa_de_rendimento_escolar <- Taxa_de_rendimento_escolar[-1,]

summary(Taxa_de_rendimento_escolar)

#4 = CO_MUNICIPIO
#6 = NO_CATEGORIA
#7 = NO_DEPENDENCIA
#20 = Taxa de Aprova??o EM TOTAL
#21 = Taxa de Aprova??o EM SERIE1
#22 = Taxa de Aprova??o EM SERIE2
#23 = Taxa de Aprova??o EM SERIE3
#38 = Taxa de Reprova??o EM TOTAL
#39 = Taxa de Reprova??o EM SERIE1
#40 = Taxa de Reprova??o EM SERIE2
#41 = Taxa de Reprova??o EM SERIE3
#56 = Taxa de Abandono EM TOTAL
#57 = Taxa de Abandono EM SERIE1
#58 = Taxa de Abandono EM SERIE2
#59 = Taxa de Abandono EM SERIE3

colnames(Taxa_de_rendimento_escolar) <- c('c1','c2','c3','CO_MUNICIPIO','c5','NO_CATEGORIA','NO_DEPENDENCIA','c8','c9','c10','c11','c12','c13',
                                             'c14','c15','c16','c17','c18','c19','Taxa_de_Aprova??o_EM_TOTAL','Taxa_de_Aprova??o_EM_SERIE1',
                                             'Taxa_de_Aprova??o_EM_SERIE2','Taxa_de_Aprova??o_EM_SERIE3','c24','c25','c26','c27','c28','c29',
                                             'c30','c31','c32','c33','c34','c35','c36','c37','Taxa_de_Reprova??o_EM_TOTAL','Taxa_de_Reprova??o_EM_SERIE1',
                                             'Taxa_de_Reprova??o_EM_SERIE2','Taxa_de_Reprova??o_EM_SERIE3','c42','c43','c44','c45','c46','c47','c48','c49',
                                             'c50','c51','c52','c53','c54','c55','Taxa_de_Abandono_EM_TOTAL','Taxa_de_Abandono_EM_SERIE1',
                                          'Taxa_de_Abandono_EM_SERIE2','Taxa_de_Abandono_EM_SERIE3','c60','c61')



Taxa_de_rendimento_escolar <- as_tibble(Taxa_de_rendimento_escolar)

Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_TOTAL <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_TOTAL)
Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_SERIE1 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_SERIE1)
Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_SERIE2 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_SERIE2)
Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_SERIE3 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Aprova??o_EM_SERIE3)
Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_TOTAL <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_TOTAL)
Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_SERIE1 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_SERIE1)
Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_SERIE2 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_SERIE2)
Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_SERIE3 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Reprova??o_EM_SERIE3)
Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_TOTAL <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_TOTAL)
Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_SERIE1 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_SERIE1)
Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_SERIE2 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_SERIE2)
Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_SERIE3 <- as.numeric(Taxa_de_rendimento_escolar$Taxa_de_Abandono_EM_SERIE3)


Taxa_de_rendimento_escolar <- Taxa_de_rendimento_escolar %>%
  select(CO_MUNICIPIO,NO_CATEGORIA,NO_DEPENDENCIA,Taxa_de_Aprova??o_EM_TOTAL,Taxa_de_Aprova??o_EM_SERIE1,Taxa_de_Aprova??o_EM_SERIE2,Taxa_de_Aprova??o_EM_SERIE3,
         Taxa_de_Reprova??o_EM_TOTAL,Taxa_de_Reprova??o_EM_SERIE1,Taxa_de_Reprova??o_EM_SERIE2,Taxa_de_Reprova??o_EM_SERIE3,
         Taxa_de_Abandono_EM_TOTAL,Taxa_de_Abandono_EM_SERIE1,Taxa_de_Abandono_EM_SERIE2,Taxa_de_Abandono_EM_SERIE3)

write_rds(Taxa_de_rendimento_escolar,"Taxa de rendimento escolar.rds")


##########


IDHM_e_IVS <- read_xlsx("C:/Users/Bruno/Desktop/dados.xlsx")

summary(IDHM_e_IVS)

colnames(IDHM_e_IVS) <- c('c1','c2','CO_MUNICIPIO','c4','c5','c6','IVS','IDHM')

IDHM_e_IVS <- as_tibble(IDHM_e_IVS)

IDHM_e_IVS$IVS <- as.numeric(IDHM_e_IVS$IVS)
IDHM_e_IVS$IDHM <- as.numeric(IDHM_e_IVS$IDHM)

IDHM_e_IVS <- IDHM_e_IVS %>%
  select(CO_MUNICIPIO,IVS,IDHM)

write_rds(IDHM_e_IVS,"IDHM e IVS.rds")


################################################################################
################################################################################

IDEB <- read_excel("D:/Arquivos/unb2021/LabEst/Tabelas xlsx/IDEBetal.xlsx")

names(IDEB) <- IDEB[1,]
IDEB <- IDEB[-1,]

summary(IDEB)

IDEB <- as_tibble(IDEB)

IDEB$VL_APROVACAO_2019_SI_4 <- as.numeric(IDEB$VL_APROVACAO_2019_SI_4)
IDEB$VL_INDICADOR_REND_2019 <- as.numeric(IDEB$VL_INDICADOR_REND_2019)
IDEB$VL_NOTA_MEDIA_2019 <- as.numeric(IDEB$VL_NOTA_MEDIA_2019)
IDEB$VL_OBSERVADO_2019 <- as.numeric(IDEB$VL_OBSERVADO_2019)
IDEB$VL_PROJECAO_2019 <- as.numeric(IDEB$VL_PROJECAO_2019)

IDEB <- IDEB %>%
  select(CO_MUNICIPIO,REDE,VL_APROVACAO_2019_SI_4,VL_INDICADOR_REND_2019,VL_NOTA_MEDIA_2019,VL_OBSERVADO_2019,VL_PROJECAO_2019)

write_rds(IDEB,"IDEB.rds")