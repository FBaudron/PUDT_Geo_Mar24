
library(geodata)
library(terra)
library(sf)
library(raster)
library(openxlsx)


BOUENZA = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\BOUENZA\\localités.shp")
CUVETTE = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\CUVETTE\\localités.shp")
CUVETTE_OUEST = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\CUVETTE_OUEST\\localités.shp")
KOUILOU = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\KOUILOU\\localités.shp")
LEKOUMOU = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\LEKOUMOU\\localités.shp")
LIKOUALA = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\LIKOUALA\\localités.shp")
NIARI = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\NIARI\\localités.shp")
PLATEAUX = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\PLATEAUX\\localités.shp")
POOL = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\POOL\\localités.shp")
SANGHA = vect("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\Shapefiles_localites\\SANGHA\\localités.shp")

cog0 = gadm(country = 'COG', level = 0, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")
cog1 = gadm(country = 'COG', level = 1, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")

BOUENZA = project(BOUENZA, "EPSG:4326")
CUVETTE = project(CUVETTE, "EPSG:4326")
CUVETTE_OUEST = project(CUVETTE_OUEST, "EPSG:4326")
KOUILOU = project(KOUILOU, "EPSG:4326")
LEKOUMOU = project(LEKOUMOU, "EPSG:4326")
LIKOUALA = project(LIKOUALA, "EPSG:4326")
NIARI = project(NIARI, "EPSG:4326")
PLATEAUX = project(PLATEAUX, "EPSG:4326")
POOL = project(POOL, "EPSG:4326")
SANGHA = project(SANGHA, "EPSG:4326")

BOUENZA_df = cbind(as.data.frame(BOUENZA), terra::extract(cog1, BOUENZA))
CUVETTE_df = cbind(as.data.frame(CUVETTE), terra::extract(cog1, CUVETTE))
CUVETTE_OUEST_df = cbind(as.data.frame(CUVETTE_OUEST), terra::extract(cog1, CUVETTE_OUEST))
KOUILOU_df = cbind(as.data.frame(KOUILOU), terra::extract(cog1, KOUILOU))
LEKOUMOU_df = cbind(as.data.frame(LEKOUMOU), terra::extract(cog1, LEKOUMOU))
LIKOUALA_df = cbind(as.data.frame(LIKOUALA), terra::extract(cog1, LIKOUALA))
NIARI_df = cbind(as.data.frame(NIARI), terra::extract(cog1, NIARI))
PLATEAUX_df = cbind(as.data.frame(PLATEAUX), terra::extract(cog1, PLATEAUX))
POOL_df = cbind(as.data.frame(POOL), terra::extract(cog1, POOL))
SANGHA_df = cbind(as.data.frame(SANGHA), terra::extract(cog1, SANGHA))

LOCALITE_df = rbind(BOUENZA_df, CUVETTE_df, CUVETTE_OUEST_df, KOUILOU_df, LEKOUMOU_df,
                    LIKOUALA_df, NIARI_df, PLATEAUX_df, POOL_df, SANGHA_df)

table(LOCALITE_df$Départemen)
table(LOCALITE_df$NAME_1)

LOCALITE_df$Départemen = LOCALITE_df$NAME_1

LOCALITE_df = subset(LOCALITE_df, subset = Départemen != "Brazzaville")


LOCALITE_df = subset(LOCALITE_df, subset = Type == "Autre localité") 

LOCALITE_df = LOCALITE_df %>% group_by(Toponyme, District, Départemen) %>% sample_n(1)

LOCALITE_df = LOCALITE_df[, c(1:7)]

write.xlsx(LOCALITE_df, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\LOCALITE_new.xlsx")

LOCALITE_new = LOCALITE_df %>%
  sf::st_as_sf(coords = c("X", "Y")) %>%
  sf::st_set_crs(32632)

LOCALITE_new = vect(LOCALITE_new)

LOCALITE_new = project(LOCALITE_new, "EPSG:4326")

 terra::writeVector(LOCALITE_new, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\3. Localites\\LOCALITE_new.shp", overwrite = T)


plot(cog0, main = 'Toutes les localités (3470)')
plot(LOCALITE_new, add = T)
plot(cog1, add = T)

