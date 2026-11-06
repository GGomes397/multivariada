library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(RColorBrewer)
Covid19MGSP <- read.csv2("trabalho final/Covid19MGSP_modificado.csv", 
                         encoding = "UTF-8")
#paleta 
plt_seq <- viridis(n=length(Covid19MGSP$Município), 
                   alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
plt_cat <- plasma(n=length(Covid19MGSP$Município))
### aed
str(Covid19MGSP)
summary(Covid19MGSP)
#transformação
Covid19MGSP$IDHR <- trimws(Covid19MGSP$IDHR)
Covid19MGSP$IDHR <- as.numeric(Covid19MGSP$IDHR)
#na's
colSums(is.na(Covid19MGSP))
which(is.na(Covid19MGSP$IDHR)==TRUE)#"Coronel Pacheco"

#boxplot
#casos
boxplot(Casos~Estado, data = Covid19MGSP,col=plt_cat)
which(Covid19MGSP$Casos>200000)#denovo a mona 
nome <- Covid19MGSP[which(Covid19MGSP$Casos>200000),1]
boxplot(Casos~Estado, data = Covid19MGSP,col=plt_cat)
text(x = 2, 
     y = max(Covid19MGSP$Casos), 
     labels = nome, 
     pos = 4, 
     cex = 0.8, 
     col = plt_cat)
#obitos
#SÃO PAULO TEM UM OUTLIER 
boxplot(Óbitos~Estado, data = Covid19MGSP,col=plt_cat)

nome <- Covid19MGSP[which(Covid19MGSP$Óbitos > 8000),1]
boxplot(Óbitos~Estado, data = Covid19MGSP,col=plt_cat)
text(x = 2, 
     y = max(Covid19MGSP$Óbitos), 
     labels = nome, 
     pos = 4, 
     cex = 0.8, 
     col = plt_cat)
#sp
boxplot(Óbitos ~ Município, 
        data = Covid19MGSP, 
        subset = (Estado == "SP"), 
        col = plt_seq)
#mg
boxplot(Óbitos ~ Município, 
        data = Covid19MGSP, 
        subset = (Estado == "MG"), 
        col = plt_seq)

#POpulação

boxplot(População~Estado, data = Covid19MGSP ,col=plt_cat)
which.max(Covid19MGSP$População)
nome <- Covid19MGSP[which.max(Covid19MGSP$População),1]
name
max_mg <- which.max(Covid19MGSP$População[Covid19MGSP$Estado == "MG"])
#deu um erro aqui é bh e sp
text(x = 1, 
     y = max(Covid19MGSP$População[max_mg]), 
     labels = Covid19MGSP[max_mg,1], 
     pos = 4, 
     cex = 0.8, 
     col = plt_cat)
text(x = 2, 
     y = max(Covid19MGSP$População), 
     labels = nome, 
     pos = 4, 
     cex = 0.8, 
     col = plt_cat)

#idhr


#quando eu fiz essa transformação deu Na's
#Covid19MGSP$IDHR <- as.numeric(Covid19MGSP$IDHR)
boxplot(IDHR~Estado, data = Covid19MGSP,col=plt_cat)

#IDHL
summary(Covid19MGSP$IDHL)
boxplot(IDHL~Estado, data = Covid19MGSP,col=plt_cat)

#idhe
summary(Covid19MGSP$IDHE)
boxplot(IDHE~Estado, data = Covid19MGSP,col=plt_cat)

#prevalencia
summary(Covid19MGSP$Prevalência)
boxplot(Prevalência~Estado, data = Covid19MGSP,col=plt_cat)
which(Covid19MGSP$Prevalência >1700.6)
unique(which(Covid19MGSP$Prevalência >1700.6))
aggregate(Covid19MGSP$Prevalência ~ Covid19MGSP$Prevalência,
          FUN = sum)
#mortalidade
boxplot(Mortalidade~Estado, data = Covid19MGSP,col=plt_cat)
#letalidade
boxplot(Letalidade~Estado, data = Covid19MGSP,col=plt_cat)

#vitoria do bolsonaro


#pca
S <- cov(Covid19MGSP[,-(1:2)], use = "complete.obs")#deu uns bagulhos assutador 
S
eigen(S)
e<-eigen(S)$vectors
e
lambda <- eigen(S)$values
lambda

#media do escore principais 
x_bar <- colMeans(S)
medi_scores <-t(e)%*%x_bar
medi_scores
#variâncias explicadas pelas comoponentes principais
lambda/sum(lambda)
