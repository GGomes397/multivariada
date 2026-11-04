acoes <- read.table("T8-4.DAT", sep = "\t")
pairs(acoes)
acaoes.padro <- scale(acoes)
#verificaçãp de correlaçao entre as observação 
acf(acoes[1])$acf # to calcula
corr_geral<- sapply(acoes, acf)
# dimensoes
n <- dim(acoes)[1]
m <- dim(acoes)[2]

# matriz de correlação 
R <- cor(acoes)
R
# estimaçao por componentes principai 
acoes.av <- eigen(R)
lambda <- acoes.av$values
E <- acoes.av$vectors

L.cp <- E %*% diag(sqrt(lambda))
L.cp
nomes <- c('JP Morgan', 'Citibank', 'W.Fargo', 'Shell', 'Exxon')

rownames(L.cp) <- nomes
colnames(L.cp) <- paste0("F", 1:5)
round(L.cp, 3)


#modelo fatorial ortogonal (m = 2)

L.cp2 <- L.cp[, 1:2]


#comunalidades

comum <- rowSums(L.cp2**2)
comum

#porcentagem de variabilidade 
colSums(L.cp2**2)#variabilidade
sum(colSums(L.cp2**2))# o total de variabilidade
colSums(L.cp2**2)/sum(colSums(L.cp2**2))

# variancia especifica

unica <- 1 - comum
unica


#tabela ajuste - componentes principais (m = 2)

tab.cp2 <- cbind(L.cp2, comum, unica)
round(tab.cp2, 3)


#residuos do ajuste

res.cp2 <- R - L.cp2 %*% t(L.cp2) - diag(unica)
sum(res.cp2**2)

R - crossprod(L.cp2) - diag(unica)


#estimação de máxima Verossimilhança 
# modelo fatorial prtogonal  (m=2)
acoes.fat2 <- factanal(x=acoes,factors = 2,
                       scores = "none",
                       rotation = "none")
str(acoes.fat2)

L.mv2 <- loadings(acoes.fat2)
row.names(L.mv2) <- 
colnames(L.mv2) <- paste0("F",1:2)
