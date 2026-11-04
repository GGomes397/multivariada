
library(viridis)
k <- 0.5
cores <- viridis(100)
#projeção do vetor x no  vetor y 
names(iris)
centroide <- colMeans(x)
x <- as.matrix(iris[,3:4])
plot(Petal.Width ~ Petal.Length, data=x)
points(centroide,pch=3,lwd=2)
#só queremos centrar os dados sem mudar a escala 
xc <- scale(x,center = T,scale = F )
plot(Petal.Width ~ Petal.Length, data=xc)
points(x=0,y=0,pch=3,lwd=2)
arrows(x=0,y=0,x1=xc[,1],y1=xc[,2], col=cores, lenght=0.1,angle=15)
#escolhendo vetores de xc
pontos <- c(44, 62, 123, 135)
# Xc - dados como vetores
plot(x = xc, xlim = lim.x, ylim = lim.y, pch = '.',
     col = cores[as.numeric(iris$Species)])
arrows(x0 = 0, y0 = 0, x1 = xc[, 1], y1 = xc[, 2], lty = 1, length = 0.15, 
       angle = 15, col = cores[as.numeric(iris$Species)])

# Localização dos pontos escolhidos
points(x = xc[pontos, 1], y = xc[pontos, 2], pch = 3, cex = 1)
text(x = xc[pontos, 1], y = xc[pontos, 2], label = pontos, cex = .85, 
     pos = 3)
lim.x <- c(-1,1)*3.2
lim.y <- c(-1,1)*1.3
is.matrix(xc)
#intensidade = comprimento 

#fazendo a projeção dos pontos 44,123 e e1 e e2
#e1 e e2 são ortogonais e tem norma 1
e1 <- c(sqrt(2)/2, sqrt(2)/2)
e2 <- c(-sqrt(2)/2, sqrt(2)/2)
t(xc[44,])%*%e1
t(xc[44,])%*%e2

t(xc[62,])%*%e1
#fazendo a projeção de todos os pontos que selecionamos 

xp <- xc[pontos,]
proj <- t(xp[,])%*%as.matrix(c(e1,e2))
proj
#
plot(Petal.Width~Petal.Length, data = Xc)
#arrows(x0 = 0, y0 = 0, x1 = Xc[,1], y1 = Xc[,2], col = "gray", length = 0.1, angle = 15)
abline(h = 0, v = 0, lty = 2)

arrows(x0 = 0, y0 = 0, 
       x1 = c(3*e1[1], 3*e2[1])*k, y1 = c(3*e1[2], 3*e2[2])*k, 
       col = cores[6], angle = 20, length = .25)
E <- cbind(e1,e2)
Xr <- xc%*%E
#Nós estamos tentando achar os 
#a direção de maior variabilidade é a direção dos grupos 
S <- cov(xc)
auto_vel <- eigen(S)$vectors
auto_vel
t(S)%*%auto_vel
plot(Xr, col =as.numeric(iris$Species), pch = 19)
points(auto_vel,col=cores)
