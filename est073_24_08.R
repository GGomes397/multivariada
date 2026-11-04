data("iris")
set.seed(666)
idx <- sample(1:150,size = 10,replace = F)
plot(iris[-5])
x <- iris[idx,3:4]
#eu quero os dados na origem pra eu calcular o angulo mas não quero que padronize
#so centralize
x_c <- scale(x,scale = F,center = T)
#aqui eu to tentando vizualizar meus pontos como vetores 
plot(Petal.Length~ Petal.Width,data = x_c, type='n', asp=1)
# asp me dá a proporção y/x
arrows(x0=0,y0=0,x1=x_c[,2],y1=x_c[,1], length = 0.15)
text(x=x_c[,2],y=x_c[,1],labels = 1:10,cex=0.85,pos=3)
#produto interno
xis <- x_c[6,]
lx <- sqrt(t(xis)%*%xis)
lx
#produto interno matricialmente 
diag(x_c%*%t(x_c))

#26/06
#comprimento dos vetores 
#ele calculo entre os vetores do vetor xc


f <- function(vet){
  n <- length(vet)
  a <- numeric(n/2)
  vet <- vet
  for(i in 1:10){
    xis <- vet[i, ]
    a[i] <- sqrt(xis %*% xis)
  }
  
  print(a)
  
}


######################################
# AULA 26/08
######################################

xc[6, ]/3.75 
c(-0.914, -0.408)
atan(.408/.914)

mod <- function(vet){ #calcula o modulo (comprimento) de um vetor
  return(sqrt(sum(vet^2)))
}

mod(c(-0.914, -0.408))

mod(c(0.57, 0.17))
a <- c(-0.408, 0.914)
mod(a)

t(a) %*% c(0.914, 0.408)



#como plotar dois vetores
#tem que ir da origem (0)até cad aum deles

x0 <- 0; y0 <- 0
x1 <- 0.914; y1 <- 0.408
x2 <- 0; y2 <- 0
x3 <- -0.408; y3 <- 0.914

plot(c(-1, 1), c(-1, 1), type = "n", xlab = "X", ylab = "Y", 
     main = "")
grid()

arrows(x0, y0, x1, y1, col = "blue", lwd = 2, length = 0.1)
arrows(x2, y2, x3, y3, col = "red", lwd = 2, length = 0.1)
#ENCONTRANDO UMA MATRIZ QUADRADA PARA FAZER DECOMPOSIÇÃO ESPECTRAL 
S <- cov(iris[-5])
S
iris_av <- eigen(S)
iris_av
lambda <- iris_av$values
E <- iris_av$vectors
lambda
E
#traço da matriz
tr <- sum(diag(S))#variância total 
tr
sum(E)
prod(lambda)
det(S)# variancia generalizada
E %*% diag(lambda) %*% t(E)
