
library(openxlsx)
library(geodata)
library(terra)
library(ggplot2)
library(tidyterra)
library(ggspatial)
library(ggthemes)
library(grid)


setwd('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\')

veg = rast('input-data\\vegetation\\vegetation_current 6 Mar 2024.tiff')


localites = read.xlsx('3. Localites\\Sélection localités\\Localités sélectionnées oui non végétation 6 Mars 2024.xlsx', sheet = 1)

loc = subset(localites, subset = Selection == "Oui")
oth = subset(localites, subset = Selection == "Non")


cog0 = gadm(country='COG', level = 0, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")
cog1 = gadm(country='COG', level = 1, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")
cog2 = gadm(country='COG', level = 2, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")

elevation = geodata::elevation_30s("COG", path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\elevation')

rd = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\road\\Route.shp")


cog1$NAME_1

dpt = "Bouenza"


rd_sub = terra::subset(rd, subset = rd$SubtypeCD == "1401" | rd$SubtypeCD == "1404" | 
                         rd$SubtypeCD == "1405" | 
                         # rd$SubtypeCD == "1409" | 
                         rd$SubtypeCD == "1410" | rd$SubtypeCD == "1411" |
                         rd$SubtypeCD == "1412" | rd$SubtypeCD == "1413" | 
                         rd$SubtypeCD == "1414" | rd$SubtypeCD == "1420" |
                         rd$SubtypeCD == "1421" | rd$SubtypeCD == "1423")


rv = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\hydrography\\Hydrographie_polygonale\\Hydrographie_polygonale.shp")

rv_sub = terra::subset(rv, subset = rv$Categorie == "Cours d'eau permanent" |
                         rv$Categorie == "Marais permanents" |
                         rv$Categorie == "Nappe d'eau permanente")


cog1 = subset(cog1, subset = cog1$NAME_1 == dpt) 
cog2 = subset(cog2, subset = cog2$NAME_1 == dpt) 


elevation = terra::crop(elevation, cog1)
elevation = terra::mask(elevation, cog1)


veg = terra::crop(veg, cog1)
veg = terra::mask(veg, cog1)


rd_sub = project(rd_sub, "EPSG:4326")

rd_sub = terra::crop(rd_sub, cog1)
rd_sub = terra::mask(rd_sub, cog1)


rv_sub = project(rv_sub, "EPSG:4326")

rv_sub = terra::crop(rv_sub, cog1)
rv_sub = terra::mask(rv_sub, cog1)


loc = subset(loc, subset = Départeme == dpt)

loc_spatial = loc %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

loc_spatial = vect(loc_spatial)


oth = subset(oth, subset = Départeme == dpt)

oth_spatial = oth %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

oth_spatial = vect(oth_spatial)


elevation_df = as.data.frame(elevation, xy = TRUE)

veg_df = as.data.frame(veg, xy = TRUE)


loc_centro = as.data.frame(geom(loc_spatial))
loc_centro = cbind(loc_centro, loc_spatial$Toponyme)
names(loc_centro)[6] = "Toponyme"


ggplot() + ggtitle("Département de la Bouenza") + theme_bw() +
  geom_raster(data = na.omit(elevation_df), aes(x = x, y = y, fill = COG_elv_msk)) +
  geom_spatvector(data = rd_sub, fill = NA, linewidth = 1, color = "grey70") +
  geom_spatvector(data = rv_sub, fill = NA, linewidth = 1, color = "cornflowerblue") +
  geom_spatvector(data = cog2, fill = NA, linewidth = 0.7, color = "grey30") +
  geom_spatvector(data = cog1, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = oth_spatial, shape = 4, size = 1, color = "grey30") +
  geom_spatvector(data = loc_spatial, size = 3) +
  geom_text(data = loc_centro, aes(x = x + 0.05 , y = y + 0.05, label = Toponyme),
           color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(fill = "Altitude") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))
  
ggsave("4. Departements//Bouenza.jpeg", units = "cm", width = 20, height = 20, dpi = 320)


ggplot() + ggtitle("Département de la Bouenza") + theme_bw() +
  geom_raster(data = na.omit(veg_df), aes(x = x, y = y, fill = vegetation)) +
  geom_spatvector(data = rd_sub, fill = NA, linewidth = 1, color = "grey70") +
  geom_spatvector(data = rv_sub, fill = NA, linewidth = 1, color = "cornflowerblue") +
  geom_spatvector(data = cog2, fill = NA, linewidth = 0.7, color = "grey30") +
  geom_spatvector(data = cog1, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = oth_spatial, shape = 4, size = 1, color = "grey30") +
  geom_spatvector(data = loc_spatial, size = 3) +
  geom_text(data = loc_centro, aes(x = x + 0.05 , y = y + 0.05, label = Toponyme),
            color = "black", fontface = "bold.italic", size = 3, check_overlap = FALSE) +
  scale_fill_manual(values = c("#fee8c8", "#fdbb84", "#e34a33", "#006837", "#31a354",
                               "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
                               "#ece7f2", "#a6bddb", "orangered", "orangered", "cornflowerblue")) +
  labs(fill = "Vegetation") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

ggsave("4. Departements//Bouenza vegetation.jpeg", units = "cm", width = 20, height = 20, dpi = 320)



