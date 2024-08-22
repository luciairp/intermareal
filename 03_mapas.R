
# Cargar mapa base Argentina

# opción 1
library(maps)
pat_map <- map_data("world", region = c("Argentina"))

# Filtrar el mapa a la región de la Patagonia
pat_map <- pat_map %>%
  filter(long < -62 & long > -75 & lat < -37 & lat > -55)

# Mapa gral con sitios
# Necesita objeto "data" de 02
ggplot() +
  geom_polygon(data = pat_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white") +
  geom_point(data = data, aes(x = Longitude, y = Latitude), 
             size = 3) +
  coord_fixed(1.3) +  # Mantiene la proporción entre latitud y longitud
  theme_minimal() +
  labs(title = "sitios")

# opción 2: mapa base con geoAr
# install.packages("geoAr", 
#                  repos = c("https://politicaargentina.r-universe.dev", 
#                            "https://cloud.r-project.org"))
library(geoAr)
library(sf)
library(ggrepel)
# Mapa por provincia
# SC
mapa_SC <- get_geo("SANTA CRUZ", level = "departamento", simplified = T)
data_sc <- data %>% filter(state == "SANTA CRUZ") 
ggplot()+
  geom_sf(data = mapa_SC, fill = "#D4D4D4", color = "white")+
  geom_point(data = data_sc, aes(x = Longitude, y = Latitude, col= site),
             size = 3)+
   geom_text_repel(data = data_sc,size=3,
                  aes(x = Longitude, 
                      y = Latitude, 
                      label = site),
                  max.overlaps = 25)+
  coord_sf(xlim = c(-69.25,-67.25),ylim = c(-50.6,-49))

# CH
mapa_CH <- get_geo("CHUBUT", level = "departamento", simplified = T)

# Mapa PIMCPA Punta Restinga
lim_n <- max(chiq$Latitude)
lim_s <- min(chiq$Latitude)
lim_e <- max(chiq$Longitude)
lim_o <- min(chiq$Longitude)

PIMCPA_map <- pat_map %>%
  filter(long < lim_o & long > lim_e & lat < lim_s & lat > lim_n)

ggplot() +
  geom_polygon(data = ML_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white") +
  geom_point(data = chiq, aes(x = Longitude, y = Latitude, col= strata), 
             size = 3) +
  geom_text_repel(data = chiq,size=3,
                  aes(x = Longitude, 
                      y = Latitude, 
                      label = rownames(chiq)),
                  max.overlaps = 15)+
  coord_fixed(1.3) +  # Mantiene la proporción entre latitud y longitud
  theme_minimal() +
  labs(x = "", y = "", title = "PUNTA RESTINGA - PIMCPA")