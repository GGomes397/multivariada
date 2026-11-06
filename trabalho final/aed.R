library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(RColorBrewer)
Covid19MGSP <- read_excel("trabalho final/Covid19MGSP.xlsx")


eleicoes_mg <- read_delim('trabalho final/resultado-municipio_presidente_t2_2018_mg.csv', delim = ';', locale = locale(encoding = 'latin1'))
eleicoes_sp <- read_delim('trabalho final/resultado-municipio_presidente_t2_2018_sp.csv', delim = ';', locale = locale(encoding = 'latin1'))
glimpse(eleicoes_sp)
colnames(eleicoes_mg)
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

a <- 0
for(i in 1: length(Covid19MGSP$Município)){
  if(Covid19MGSP[i,1]==eleicoes[i,1])
    a <- a+1
}
a#se a tiver diferente de zero eu vou ter que ajustar os dados 

Covid19MGSP$Vit_bolso <- eleicoes$Vit_bol

write.csv2(Covid19MGSP, 
           file = "Covid19MGSP_modificado.csv", 
           row.names = FALSE, 
           fileEncoding = "UTF-8")
