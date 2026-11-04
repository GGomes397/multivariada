library(corrplot)
library(viridis)
library(gplots)
cores <- viridis(10)
brand.ratings <- read.csv("C:/Users/Acer/Downloads/rintro-chapter8.csv")
summary(brand.ratings)
str(brand.ratings)
# centrando os daddos
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
cor(brand.ratings[,1:9], use = "complete.obs")
corrplot(cor(brand.sc[, 1:9]), order = "hclust")
summary(brand.sc)

brand.mean <- aggregate(. ~brand, data = brand.sc, mean)
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1] 
brand.mean

heatmap.2(as.matrix(brand.mean), col = cores, trace ="none")
#componentes principais 
brand.pc <- prcomp(brand.sc[,1:9])
summary(brand.pc)
#screeplot
plot(brand.pc, type = "l")
# como eu tenho três componente maior do que 1 nessa aa=nalise está indicando que podemos reduzir em três
#biplot
biplot(brand.pc, cex = 0.75, expand = 1, arrow.len = 0.15)
# eu não consegui vizualizar nada aí vamos média das brands pra deixar os dados
#agregados pra conseguir enxergar no biplot as arcas 

brand.mu.pc <- prcomp(brand.mean, scale = TRUE)
summary(brand.mu.pc)       
biplot(brand.mu.pc, cex = 0.75, expand = 1, arrow.len = 0.15)
# a marca e não tem caracteristica significativas 