library(magrittr)
library(tidyverse)
# cargar datos
data <- read.csv("cob_datos.csv")
data <- column_to_rownames(data,'X') 
rownames(data) <- substr(row.names(data), 3, 8)

# muestreo conjunto de datos chiquito de Punta Restinga - PIMCPA
chiq <- read.csv("PRES_ch.csv")
chiq <- column_to_rownames(chiq,'X') 
rownames(chiq) <- substr(row.names(chiq), 3, 8)

# datos por localidad
ML <- data[data$locality== "MONTE LEON",]
MK <- data[data$locality== "MAKENKE",]
IL <- data[data$locality== "ISLOTE LOBOS",]
PIMCPA <- data[data$locality== "PIMCPA",]

# separar cobertura "esp" de var ambientales "amb"
amb <- data[,c(1:10)]
esp <- data[,c(11:29)]

ambML <- ML[,c(1:10)]
espML <- ML[,c(11:29)]

ML.V <- data[data$site == "VENTANA",]
ambML.V <- ML.V[,c(1:10)]
espML.V <- ML.V[,c(11:29)]

MK.P <- MK[MK$site== "PENINSULA",]
ambMK.P <- MK.P[,c(1:10)]
espMK.P <- MK.P[,c(11:29)]

ambMK <- MK[,c(1:10)]
espMK <- MK[,c(11:29)]
ambch <- chiq[,c(1:10)]
espch <- chiq[,c(11:29)]



# 1. disimilitud ----------------------------------------------------------

library(vegan)

# caso chiquito
jac <- betadiver(espch,method="g")
round(jac,3)
as.matrix(jac)
heatmap(as.matrix(jac),scale = "none")
heatmap(as.matrix(jac),scale = "none",col = colorRampPalette(c("white", "red"))(256),
        Rowv = NA, Colv = NA)
heatmap(as.matrix(jac),scale = "none",col = terrain.colors(256),
        Rowv = NA, Colv = NA)
terrain.colors(256)[256]

heatmap(as.matrix(jac),scale = "none",
        col = colorRampPalette(c("white", "red"))(256),
        Rowv = NA, Colv = NA)

euc <- vegdist(espch, method = "euclidean",diag=T)
b <- vegdist(espch, method = "bray", diag = TRUE)
heatmap(as.matrix(euc),scale = "none",col = colorRampPalette(c("white", "red"))(256),
        Rowv = NA, Colv = NA)
heatmap(as.matrix(b),scale = "none",col = colorRampPalette(c("white", "red"))(256),
        Rowv = NA, Colv = NA)

# casos grandes
pastosa <- data[data$site== "LA PASTOSA",]
ambpast <- pastosa[,c(1:10)]
esppast <- pastosa[,c(11:29)]

jac <- betadiver(esppast,method="g")
heatmap(as.matrix(jac),scale = "none",
        col = colorRampPalette(c("white", "red"))(256),
        Rowv = NA, Colv = NA)


# 2. cluster -------------------------------------------------------------

euc.cl <- hclust(euc, method = "complete")
str(euc.cl)
plot(euc.cl)

b.cl <- hclust(b, method = "complete")
plot(b.cl)
rect.hclust(b.cl,h=0.5)

jac.cl <- hclust(jac, method = "complete")
plot(jac.cl)
rect.hclust(jac.cl,k=3)

cutree(euc.cl,k=1:6)

# métodos de ligamiento
b.cl.s <- hclust(b, method = "single")
plot(b.cl)
plot(b.cl.s)

# estandarizar
# al valor máximo de cada variable
espchdec <- decostand(espch,method = "max",digits=2)
# para Jaccard:
jac.dec <- betadiver(espchdec,method="g")
jac.cl.dec <- hclust(jac.dec,method="complete")
plot(jac.cl.dec)
# para Bray-Curtis
b.dec <- vegdist(espchdec, method = "bray", diag = TRUE)
b.cl.dec <- hclust(b.dec,method = "complete")
plot(b.cl.dec)
plot(b.cl)

library(ggdendro)
ggdendrogram(euc.cl)

# 3. K-means --------------------------------------------------------------
set.seed(3)
cluster.km <- kmeans(espch, centers = 3)
