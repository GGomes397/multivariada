dados <- read.table("T4-3.DAT")[-5]
dados

# fazr um painel utilizando a função pairs
pairs(dados, lower.panel = panel.smooth) # suavisa
#O pairs coloca os gráficos de dispersão como em um apinel 
panel.elipses <- function(x, y){
  points(x, y, pch = 19, cex = 1, col = "gray")
  cjto <- cbind(x, y)
  centro <- colMeans(cjto)
  desvios <- apply(cjto, 2, sd)
  corr <- cor(x, y)
  elipse.conf <- ellipse::ellipse(corr, scale = desvios, center = centro, level = 0.95)
  lines(elipse.conf, col = "blue", lwd = 1, lty = 2)
  out <- c(9, 16)
  points(x[out], y[out], pch = 19, col = "red")
  text(x[out], y[out], label = out, cex = 0.85, font = 2, pos = 2)
}
pairs(dados, lower.panel = panel.elipses)

panel.scat <- function(x, y){
  points(x = x, y = y, pch = 19, cex = 1, col = "blue")
}
pairs(dados, lower.panel = panel.elipses, upper.panel = panel.scat) 

panel.corr <-  function(x, y){
  usr <-  par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits = 3)
  txt <- paste0("r = ", r)
  text(0.5, 0.5, txt, cex = 1.25, font = 2)
}
pairs(dados, lower.panel = panel.elipses, upper.panel = panel.corr)

panel.hist <- function(x){
  usr <-  par("usr"); on.exit(par(usr = usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <-  hist(x, plot = F)
  breaks <- h$breaks
  len <- length(breaks)
  y <- h$counts/max(h$counts)
  rect(breaks[-len], 0, breaks[-1], y, col = "lightblue")
}
pairs(dados, 
      lower.panel = panel.elipses, 
      upper.panel = panel.corr, 
      diag.panel = panel.hist,
      labels = c("Choque", "Vibração", "Físico #1", "Fisico #2"),
      gap = 0.3, # controla os espaços entre cada quadro
      main = "Matriz de Diagramas de Dispersão de Medidas")

## normalidade bivariada

X <- dados[, 1]
dim(X)
Y <-  dados[, 2]
S12 <- cov(cbind(X, Y))

plot(x = X, y = Y)
subcjt <- cbind(X, Y)
centro <- colMeans(subcjt)
desvios <- apply(subcjt, 2, sd)
corr <- cor(X, Y)
elipse.conf <- ellipse::ellipse(corr, scale = desvios, center = centro, level = 0.5)
lines(elipse.conf, col = "blue", lwd = 2)

dj.12 <- mahalanobis(subcjt, center = centro, cov = S12, inverted = F)
mean(dj.12 < qchisq(p = 0.5, df = 2))

## gráfico qui-quadrado 

Sx <- cov(dados)
Sx
D2 <- mahalanobis(dados, colMeans(dados), Sx)
D2
is.vector(D2)

all.equal(D2, dados[,5])

amostra <- D2
tam <- length(amostra)
p.emp <- ppoints(tam, a = 1/2)
q.teor <- qchisq(p.emp, df = 4)
q.emp <- sort(amostra)

# construção do gráfico quantil-quantil
plot(q.teor, q.emp, pch = 19, col = "royalblue4",
     xlab = "Quantis Teóricos", ylab = "Quantis Amostrais",
     main = "Q-Q plot Qui-quadrado")
quantilica <- function(x) qchisq(x, df = 4)
qqline(amostra, distribution = quantilica)

# pontos atípicos

atp <- c(9, 16)
absc <- amostra[atp]
absc <- rank(amostra)[atp]
points(x = q.teor[absc], y = q.emp[absc], pch = 19, col = "red")
text(x = q.teor[absc], y = q.emp[absc], label = atp, font = 2, pos = 2)

