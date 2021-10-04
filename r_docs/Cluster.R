pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2)

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

teste <- merge(idhivs,gestao,
      by = "CO_MUNICIPIO")