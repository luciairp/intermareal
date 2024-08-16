library(tidyverse)
library(lubridate)

gral <- read_csv("metadata.csv",
                 col_types = cols(
                   country = col_factor(),
                   state = col_factor(),
                   locality = col_factor(),
                   strata = col_factor(),
                   site = col_factor(),
                   Date = col_date()
                 ))
cob <- read_csv("percent_covers.csv",
                col_types = cols(
                  "Annotation status" = col_factor()
                ),
                n_max = 15695)

# en gral están los datos de cada muestreo
# en cob están las cob de cada unidad

# elijo con qué trabajar en gral
# consulto esas ID en los datos de cob

names(gral)
summary(gral)
gral <- gral %>% 
  select(Name, Date, state, locality, site, strata, 
                        Comments, Latitude, Longitude) %>% 
  mutate(yr=year(Date),
         fotoID = str_sub(Name,1,-5))# achico nombres de fotos
str(gral)

tabla <- group_by(gral,state, locality, site) %>% summarise(n())
tabla_yr <- group_by(gral,state, locality, site, yr) %>% summarise(n())
tabla_strata <- group_by(gral,state,locality,site,yr,strata) %>% summarise(n()) %>% 
  filter(state != "BUENOS AIRES") %>% filter(yr == "2023")

# fabrico selección de datos para usar en el tp Eco Com 2024
# solo 2023
# sin BsAs, locality por provincia vinculada a APN
# RN - Islote Lobos
# CH - PIMCPA
# SC - PNML y PIMM

datos <- gral %>% 
  filter(yr == 2023) %>% 
  filter(locality == "ISLOTE LOBOS"|locality =="PIMCPA"|locality =="MONTE LEON"|locality =="MAKENKE")
unique(datos$locality)

names(cob)
cob <-  cob %>% 
  select(-Points) %>% 
  rename(Name = "Image name") %>% 
  mutate(fotoID = str_sub(Name, 1,-5))

# fabrico archivo de datos extras, a partir de Name de cob
# cob_gral <- cob %>% left_join(gral) %>% 
#   select(`Annotation status`,4:22,Date, state, locality,site, strata,
#          Latitude, Longitude, yr, fotoID) %>% 
#   column_to_rownames("fotoID")

# fabrico archivo de datos de cobertura a partir de selección para tp EcoCom2024
cob_datos <- datos %>% left_join(cob) %>% 
  select(-`Annotation status`,-`Comments`) %>% 
  column_to_rownames("fotoID")
write.csv(cob_datos,"cob_datos.csv", row.names = T)

# LISTO

# para fabricar archivos de datos por especie (19), y transpuesto
todo <- cob %>% select(-`Image ID`, -`Annotation status`) %>% 
  column_to_rownames("fotoID")
todo.t <- t(todo)

library(vegan)
# riqueza
specpool(todo,cob_gral$state)
# diversidad
diversity(todo, index = "shannon", groups = cob_gral$state, MARGIN = 1)

# agregar datos por locality
cob_loc <- aggregate(cob_gral[,2:19], by = list(cob_gral$locality), FUN='sum') %>% 
  rename(loc = "Group.1") %>% 
  column_to_rownames("loc")

# cluster k means
cluster.km <- kmeans(cob_loc, centers = 5)
cluster.km$cluster

# distancia y 
jac <- betadiver(cob_loc,method="g")
euc <- vegdist(cob_loc, method = "euclidean")
b <- vegdist(cob_loc, method = "bray", diag = TRUE)

euc.cl <- hclust(euc, method = "complete")
b.cl <- hclust(b, method = "complete")
jac.cl <- hclust(jac, method = "complete")
plot(euc.cl, cex = .8)
plot(b.cl, cex = .8)
plot(jac.cl)

# filtrar por año
yr21 <- cob_gral %>% filter(yr == '2021')

specpool(yr21[,2:19], yr21$state)
diversity(yr21[,2:19], index = "shannon", groups = yr21$state, MARGIN = 1)

yr23 <- cob_gral %>% filter(yr == '2023')

specpool(yr23[,2:19], yr23$state)
specpool(yr23[,2:19], yr23$locality)
diversity(yr23[,2:19], index = "shannon", groups = yr23$state, MARGIN = 1)
diversity(yr23[,2:19], index = "shannon", groups = yr23$locality, MARGIN = 1)
diversity(yr23[,2:19], index = "simpson", groups = yr23$locality, MARGIN = 1)

yr24 <- cob_gral %>% filter(yr == '2024')

specpool(yr24[,2:19], yr24$state)
diversity(yr24[,2:19], index = "shannon", groups = yr24$state, MARGIN = 1)


# cluster k means
cluster.km <- kmeans(yr23[,2:19], centers = 5)
cluster.km$cluster

# distancia y 
jac <- betadiver(yr23[,2:19],method="g")
euc <- vegdist(yr23[,2:19], method = "euclidean")
b <- vegdist(yr23[,2:19], method = "bray", diag = TRUE)

euc.cl <- hclust(euc, method = "complete")
plot(euc.cl, cex = .3)
hcd <- as.dendrogram(euc.cl)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.2, col = "blue")
# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
plot(hcd,  xlab = "Height", nodePar = nodePar, horiz = TRUE)
library(ggdendro)
ggdendrogram(euc.cl)
