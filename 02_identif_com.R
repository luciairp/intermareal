library(magrittr)
# cargar datos
data <- read.csv("cob_datos.csv")
data <- column_to_rownames(data,'X') 

ML <- data[data$locality== "MONTE LEON",]
MK <- data[data$locality== "MAKENKE",]
IL <- data[data$locality== "ISLOTE LOBOS",]
PIMCPA <- data[data$locality== "PIMCPA",]

amb <- data[,c(1:10)]
esp <- data[,c(11:29)]

ambML <- ML[,c(1:10)]
espML <- ML[,c(11:29)]

ML.V <- ML[ML$site == "VENTANA",]
ambML.V <- ML.V[,c(1:10)]
espML.V <- ML.V[,c(11:29)]


MK.P <- MK[MK$site== "PENINSULA",]
ambMK.P <- MK.P[,c(1:10)]
espMK.P <- MK.P[,c(11:29)]

library(vegan)
jac <- betadiver(espML.V,method="g")
heatmap(as.matrix(jac),scale = "none",col = terrain.colors(256),
        Rowv = NA, Colv = NA)
terrain.colors(256)[256]
as.matrix(jac)

euc <- vegdist(espML.V, method = "euclidean")
b <- vegdist(espML.V, method = "bray", diag = TRUE)
heatmap(as.matrix(euc),scale = "none",col = terrain.colors(256),
        Rowv = NA, Colv = NA)
heatmap(as.matrix(b),scale = "none",col = terrain.colors(256),
        Rowv = NA, Colv = NA)
