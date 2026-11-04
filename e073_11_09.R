library(viridis)
cores <- plasma(n = 3, end = 0.9)
#começo de analise de componentes principais 
x <- as.matrix(iris[-5])
S <- cov(x)
x_av <- eigen(S)
x_av
lambda <- x_av$values
E <- x_av$vectors
var_tot <- sum(lambda)
lambda/sum(lambda)
round((E**2)*100,1)#aqui eu to com a porcentagem de variabu
Y <- x%*%E
Y
as.matrix(Y)#tem que garantir que é matriz
plot(x=Y[,1],y=Y[,2],col = cores[iris$Species])
colMeans(Y)
s1 <- cov(Y)
cor(Y[,1],x[,3])
#queremos achar a matriz que nos de as correlação entre as variáveis

