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

# muestreo conjunto de datos chiquito 
set.seed(3)
int <- cob_datos %>% filter(site == "PUNTA RESTINGA")
L <- int[sample(which(int$strata == "LOWTIDE"),5),]
M <- int[sample(which(int$strata == "MIDTIDE"),5),]
H <- int[sample(which(int$strata == "HIGHTIDE"),5),]
ch <-bind_rows(L,M,H) 
write.csv(ch,"PRES_ch.csv")

# LISTO

# miro resumen de cantidad de imágenes por estrato
num_fotos <- group_by(datos,locality,site, strata) %>% summarise(n())
num_fotos
mean(num_fotos$`n()`)

library(vegan)
# riqueza
specpool(todo,cob_gral$state)
# diversidad
diversity(todo, index = "shannon", groups = cob_gral$state, MARGIN = 1)

# agregar datos por locality
cob_loc <- aggregate(cob_gral[,2:19], by = list(cob_gral$locality), FUN='sum') %>% 
  rename(loc = "Group.1") %>% 
  column_to_rownames("loc")