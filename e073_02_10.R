library(nortest)
library(ellipse)

dados <- read.table("T4-3.DAT")
original <- read.table("T4-3.DAT")
dados
dim(dados)
str(dados)
dados <- dados[-5]
dados
par(mfrow=c(2,2))
#univariada
for(i in  1:4){
  qqnorm(d1[,i], pch = 20)
  qqline(d1[,i],col = "red", lwd = 2)
  #shapiro e pegando o p-valor
  shapiro.test(d1[,i])
  obj.i <- shapiro.test(d1[,i])$p.value
  #ad.test
  ad.test(d1[,i])
  obj.ad.1 <-ad.test(d1[,i])$p.value
  #colocando isso no grafico
  legend("topleft", 
         legend = c(
           paste0("Shapiro-Wilk (p): ", format.pval(obj.i, digits = 3, eps = 0.001)),
           paste0("Anderson-Darling (p): ", format.pval(obj.ad.1, digits = 3, eps = 0.001))
         ),
         bty = "n",       
         cex = 0.85,      
         text.col = "blue")
  
}

#achando os outliers porque em todos os gráficos os maiores valores que estão nos dando outliers
which(dados[,1]==max(dados[,1]))



# titrando os outliers 
d1 <- dados[-9,]
#univariada
for ( i in 1:4) {
  qqnorm(d1[,i], pch = 20)
  qqline(d1[,i],col = "red", lwd = 2)
  #shapiro e pegando o p-valor
  shapiro.test(d1[,i])
  obj.i <- shapiro.test(d1[,i])$p.value
  #ad.test
  ad.test(d1[,i])
  obj.ad.1 <-ad.test(d1[,i])$p.value
  #colocando isso no grafico
  legend("topleft", 
         legend = c(
           paste0("Shapiro-Wilk (p): ", format.pval(obj.i, digits = 3, eps = 0.001)),
           paste0("Anderson-Darling (p): ", format.pval(obj.ad.1, digits = 3, eps = 0.001))
         ),
         bty = "n",       
         cex = 0.85,      
         text.col = "blue")
}



#bivariado 
par(mfrow=c(1,1))
#(x1,x2)
plot(dados[,1]~dados[,2])
#elipse
sub_dados <- dados[, c(2, 1)]
sigma <- cov(sub_dados)
mu    <- colMeans(sub_dados)

elipse_95 <- ellipse(sigma, centre = mu, level = 0.95)
elipse_50 <- ellipse(sigma, centre = mu, level = 0.5)

# 4. Desenha a elipse e o centroide
lines(elipse_95, col = "red", lwd = 2)
lines(elipse_50,col='blue', lwd=1)
points(mu[1], mu[2], col = "red", pch = 4, lwd = 2)
points(dados[16, 2], dados[16, 1], col = "purple", pch = 17, cex = 1.6)

#calculando os pontos fora da elipse
#fiz alguma coisa errado nesse calculo
#t(as.matrix(dados[,c(1,2)]-mean(mu)))%*%sigma%*%(as.matrix(dados[,c(1,2)]-mu))

dm <- mahalanobis(sub_dados, center = mu, cov = sigma)
c_50 <- qchisq(0.50, df = 2)
c_95 <- qchisq(0.95, df = 2)

# 4. Contagem e proporções observadas
n_total <- nrow(sub_dados)

dentro_50 <-dm <= c_50
dentro_95 <- dm <= c_95
#(x1,x3)
plot(dados[,1]~dados[,3])
#elipse
sub_dados <- dados[, c(2, 1)]
sigma <- cov(sub_dados)
mu    <- colMeans(sub_dados)

elipse_95 <- ellipse(sigma, centre = mu, level = 0.95)
elipse_50 <- ellipse(sigma, centre = mu, level = 0.5)

# 4. Desenha a elipse e o centroide
lines(elipse_95, col = "red", lwd = 2)
lines(elipse_50,col='blue', lwd=1)
points(mu[1], mu[2], col = "red", pch = 4, lwd = 2)
points(dados[16, 2], dados[16, 1], col = "purple", pch = 17, cex = 1.6)

#calculando os pontos fora da elipse
#fiz alguma coisa errado nesse calculo
#pra fazer as contas vou ter que centrar os meu dados 
S <- cov(dados)
xc <- scale(dados,center = TRUE,scale = F)
matriz <- xc%*%solve(S)%*%t(xc)
matriz
dj2 <- diag(matriz)
dj2
all.equal(original[,5],dj2)
sort(dj2)
dm <- mahalanobis(sub_dados, center = mu, cov = sigma)
c_50 <- qchisq(0.50, df = 2)
c_95 <- qchisq(0.95, df = 2)

# 4. Contagem e proporções observadas
n_total <- nrow(sub_dados)

dentro_50 <-dm <= c_50
dentro_95 <- dm <= c_95

#(x2,x3)
