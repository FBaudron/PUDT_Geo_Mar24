
library(geodata)
library(terra)
library(sf)
library(raster)
library(splitstackshape)
library(openxlsx)

setwd("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo")

veg = rast('input-data\\vegetation\\vegetation_modelled 6 Mar 2024.tiff')

loc = vect("3. Localites\\LOCALITE_new.shp")

cog0 = gadm(country = 'COG', level = 0, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")
cog1 = gadm(country = 'COG', level = 1, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")


plot(veg)
plot(cog1, add = T)
plot(loc, add = T)

loc_df = cbind(as.data.frame(loc), geom(loc), terra::extract(veg, loc))


table(loc_df$vegetation)
nrow(loc_df)


# SELECTION D'UN ECHANTILLON STRATIFIE PAR CLASSES DE VEGETATION SEULEMENT------

set.seed(1234)

SAMPLE1 = as.data.frame(stratified(loc_df, "vegetation", 100/nrow(loc_df)))

SAMPLE1

SAMPLE_spatial1 <- SAMPLE1 %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

SAMPLE_spatial1 = vect(SAMPLE_spatial1)


plot(veg, add = F,
     col = c("#006837", "#31a354",
             "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
             "#ece7f2", "#a6bddb"),
     main = 'Stratified sample of 100 localities')
plot(SAMPLE_spatial1, add = T)
plot(cog1, add = T)


# SELECTION D'UN ECHANTILLON STRATIFIE PAR DOMAIS ET DEPARTEMENTS---------------

set.seed(1234)

SAMPLE2 = as.data.frame(stratified(loc_df, c("vegetation", "Départeme"), 105/nrow(loc_df)))

SAMPLE2

SAMPLE_spatial2 <- SAMPLE2 %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

SAMPLE_spatial2 = vect(SAMPLE_spatial2)


plot(veg, add = F,
     col = c("#006837", "#31a354",
             "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
             "#ece7f2", "#a6bddb"),
     main = 'Stratified sample of 100 localities')
plot(SAMPLE_spatial2, add = T)
plot(cog1, add = T)


write.xlsx(SAMPLE2, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Sélection localités\\Sample of localities végétation 6 Mars 2024.xlsx")

names(SAMPLE2)

SAMPLE2 = SAMPLE2[, c(2:4)]

SAMPLE2$Selection = rep("Oui", nrow(SAMPLE2))

loc_df = merge(loc_df, SAMPLE2, by = c("Toponyme", "District", "Départeme"), all.x = T)

loc_df$Selection[is.na(loc_df$Selection)] = "Non"

write.xlsx(loc_df, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Sélection localités\\Localités sélectionnées oui non végétation 6 Mars 2024.xlsx")

