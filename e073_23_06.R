#lendo os dados da tartaruga 
library(dplyr)
dados <- read.table("C:/Users/Acer/Downloads/T6-9.DAT", header = FALSE)
colnames(dados) <- c("comprimento","largura","altura","sexo")
colnames(dados)
#dados filtrados 
df <- dados %>%
  filter(sexo != "female")
#fazendo a parte de componentes principais 
S1 <- cov(log(df[-4]))
S1
E <- eigen(S1)$vectors
E
E <- round(E,3)
E
lambda <-eigen(S1)$values
lambda <- round(lambda,3)
#os ultimos valores muito proximos de 0 multicolinearinidade 
x <- log(df[-4])
x
S <- cov(x)
S
is.matrix(x)
Y <- as.matrix(x)%*%E
plot(x = Y[, 1], y = Y[, 2])
text(x = Y[1,1], y = Y[1,2], label = 1)
# plot do componente 1x2
plot(x=Y[,1],y=Y[,2],
     type = 'n')
text(x = Y[, 1], y = Y[, 2], labels = 1:nrow(Y), cex = 0.85)
#traço de S 
traco <- sum(lambda)

traco
#quanto é explicado pela componete 1 e 2
(lambda[1]+lambda[2])/traco #1 mané isso está errado 


#quanto é explicado pela componete 1 e 3
round((lambda[1]+lambda[3])/traco,3)#0.958 mané isso está errado 

#quanto é explicado pela componete 2 e 3
round((lambda[2]+lambda[3])/traco,3)# 0.042 mané isso está errado 

#plot do componente 1x3
plot(x=Y[,1],y=Y[,3],
     type = 'n')
text(x = Y[, 1], y = Y[, 3], labels = 1:nrow(Y), cex = 0.85)
#quem é essa vigésima de componente 
#qq plot porque eu estou fazendo isso 
qqnorm(Y[,1])
qqline(Y[,1])
#tem normalidade

#segunda componente 
qqnorm(Y[,2])
qqline(Y[,2])
#tem normalidade

#terceira componente 
qqnorm(Y[,3])
qqline(Y[,3])
#os dados não são normais 
#e percebemos a presença de uma curtose 


#quero fazer um teste de hipotese 
#h0: lambda[3]=0 vs h1 : lambda[3]!=0
# como os dados do componentes 3 não tem normalidade devemos fazer teste assintóticos 
#ele quer o p-valor e intervalo de confiança 
#e fazer alguma coisa com o iris 