
library(geodata)
library(terra)
library(sf)
library(raster)
library(splitstackshape)
library(openxlsx)

pam = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\pam\\zae_pam.shp")

plot(pam)

loc = vect("3. Localites\\LOCALITE_new.shp")

cog0 = gadm(country = 'COG', level = 0, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")
cog1 = gadm(country = 'COG', level = 1, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")


pal = colorRampPalette(c('yellow', 'orange', 'red'))

plot(pam, col = pal(8))
plot(cog1, add = T)
plot(loc, add = T)

loc_df = cbind(as.data.frame(loc), geom(loc), terra::extract(pam, loc))


# SELECTION D'UN ECHANTILLON STRATIFIE PAR DOMAINS SEULEMENT--------------------

set.seed(1234)

SAMPLE1 = as.data.frame(stratified(loc_df, "ZAE", 100/nrow(loc_df)))

SAMPLE1

SAMPLE_spatial1 <- SAMPLE1 %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

SAMPLE_spatial1 = vect(SAMPLE_spatial1)

plot(pam, add = F, col = pal(8), main = 'Stratified sample of 100 localities')
plot(SAMPLE_spatial1, add = T)
plot(cog1, add = T)


# SELECTION D'UN ECHANTILLON STRATIFIE PAR DOMAIS ET DEPARTEMENTS---------------

set.seed(1234)

SAMPLE2 = as.data.frame(stratified(loc_df, c("ZAE", "Départeme"), 100/nrow(loc_df)))

SAMPLE2

SAMPLE_spatial2 <- SAMPLE2 %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

SAMPLE_spatial2 = vect(SAMPLE_spatial2)

plot(pam, add = F, col = pal(8), main = 'Stratified sample of 100 localities')
plot(SAMPLE_spatial2, add = T)
plot(cog1, add = T)

write.xlsx(SAMPLE2, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Sélection localités\\Sample of localities PAM 6 Mars 2024.xlsx")

names(SAMPLE2)

SAMPLE2 = SAMPLE2[, c(2:4)]

SAMPLE2$Selection = rep("Oui", nrow(SAMPLE2))

loc_df = merge(loc_df, SAMPLE2, by = c("Toponyme", "District", "Départeme"), all.x = T)

loc_df$Selection[is.na(loc_df$Selection)] = "Non"

write.xlsx(loc_df, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Sélection localités\\Localités sélectionnées oui non PAM 6 Mars 2024.xlsx")
