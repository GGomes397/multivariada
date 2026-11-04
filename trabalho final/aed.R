library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
Covid19MGSP <- read_excel("trabalho final/Covid19MGSP.xlsx")

str(Covid19MGSP)

eleicoes_mg <- read_delim('trabalho final/resultado-municipio_presidente_t2_2018_mg.csv', delim = ';', locale = locale(encoding = 'latin1'))
eleicoes_sp <- read_delim('trabalho final/resultado-municipio_presidente_t2_2018_sp.csv', delim = ';', locale = locale(encoding = 'latin1'))
glimpse(eleicoes_sp)

remove_col <- function(df) {
  manter <- c("sg_uf", "nm_municipio", "qt_votos_nom_validos","nm_candidato")
  
  # Seleciona apenas as colunas que estão presentes tanto no df quanto no vetor
  cols_presentes <- intersect(names(df), manter)
  
  df[, cols_presentes, drop = FALSE]
}
eleicoes_mg <- remove_col(eleicoes_mg)
eleicoes_sp <- remove_col(eleicoes_sp)


eleicoes <- bind_rows(eleicoes_mg, eleicoes_sp) %>%
  pivot_wider(
    names_from = nm_candidato,
    values_from = qt_votos_nom_validos,
    values_fill = 0
  )


eleicoes <- eleicoes %>%
  rename(
    Hadad = "FERNANDO HADDAD",
    Bolso = "JAIR MESSIAS BOLSONARO"
  )

# Exibir o resultado final
glimpse(eleicoes)

eleicoes <- eleicoes%>%
  mutate(Vit_bol = if_else(Bolso > Hadad, 1, 0)) %>%
  select(sg_uf, nm_municipio, Vit_bol)
glimpse(eleicoes)
# Padronizar nomes das colunas de cruzamento nas duas tabelas
all.equal(eleicoes$nm_municipio,Covid19MGSP$Município)
eleicoes$nm_municipio <- 

covid_mun_col <- if ("MUNICIPIO" %in% names(covid_data)) "MUNICIPIO" else if ("NOME" %in% names(covid_data)) "NOME" else names(covid_data)[1]
covid_uf_col <- if ("UF" %in% names(covid_data)) "UF" else if ("ESTADO" %in% names(covid_data)) "ESTADO" else names(covid_data)[2]

