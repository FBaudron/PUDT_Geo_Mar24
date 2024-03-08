
library(geodata)
library(terra)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(ggspatial)
library(ggthemes)


setwd("D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo")

veg = rast('input-data\\vegetation\\dataset-satellite-land-cover\\C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc')

cog0 = gadm(country = 'COG', level = 0, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")
cog1 = gadm(country = 'COG', level = 1, path = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions\\gadm")

veg = terra::crop(veg, cog1)
veg = terra::mask(veg, cog1)

plot(veg)

veg = project(veg, "EPSG:4326")

plot(veg$lccs_class)
plot(cog1, add = T)

veg_df = as.data.frame(veg)
table(veg_df$lccs_class)

veg = veg[[1]]
names(veg) = 'lccs_class'


cls = data.frame(id = c(10, 11, 12, 30, 40, 50, 60, 61, 62, 100, 110, 120, 122, 130, 160, 170, 180, 190, 200, 201, 210),
                 vegetation = c('cropland_rainfed', 'cropland_rainfed', 'cropland_rainfed', 'cropland_mosaic', 'natural_veg_mosaic',
                                'tree_cover_broadleaved_evergreen', 'tree_cover_broadleaved_deciduous', 'tree_cover_broadleaved_deciduous',
                                'tree_cover_broadleaved_deciduous', 'tree_shrub_mosaic', 'herbaceous_cover_mosaic',
                                'shrubland', 'shrubland', 'grassland', 'tree_cover_flooded_fresh', 'tree_cover_flooded_saline',
                                'shrub_herbaceous_flooded', 'urban', 'bare', 'bare', 'water'))

levels(veg) = cls

levels(veg)
cats(veg)

terra::writeRaster(veg, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\vegetation\\vegetation_current 6 Mar 2024.tiff", overwrite=T)




# png("map_vegetation_current.png", units="in", width=7, height=7.5, res=1000)
par(mfrow = c(1,1), mar=c(0.1,0.1,0.1,1), cex.main=1.95, cex.axis=1.6)
plot(veg, legend = T, axes = F, add = F,
            col = c("wheat", "wheat", "wheat", "darkgreen", "forestgreen",
                    "yellowgreen", "orange", "orangered","yellow", "darkblue",
                    "purple", "blue", "red", "red", "lightblue"),
     main = 'Végétation')
plot(cog1, axes = F, add = T)
# dev.off()

# png("3. Localites//Gap filling//map_vegetation_current.png", units="in", width=7, height=7.5, res=1000)
par(mfrow = c(1,1), mar=c(0.1,0.1,0.1,1), cex.main=1.95, cex.axis=1.6)
plot(veg, legend = T, axes = F, add = F,
     col = c("#e34a33", "#fdbb84", "#fee8c8", "#006837", "#31a354",
             "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
             "#ece7f2", "#a6bddb", "orangered", "orangered", "cornflowerblue"),
     main = 'Végétation')
plot(cog1, axes = F, add = T)
# dev.off()


terra::cellSize(veg, unit = "m")
sqrt(94621.50)



# Turn a category into NA
veg_na = veg

veg_na = ifel(veg_na == 10, NA, veg_na) 
veg_na = ifel(veg_na == 11, NA, veg_na) 
veg_na = ifel(veg_na == 12, NA, veg_na) 
veg_na = ifel(veg_na == 30, NA, veg_na) 
veg_na = ifel(veg_na == 40, NA, veg_na) 
veg_na = ifel(veg_na == 190, NA, veg_na) 
veg_na = ifel(veg_na == 200, NA, veg_na) 
veg_na = ifel(veg_na == 201, NA, veg_na) 
veg_na = ifel(veg_na == 210, NA, veg_na) 

# png("3. Localites//Gap filling//map_natural_vegetation_current.png", units="in", width=7, height=7.5, res=1000)
par(mfrow=c(1,1), mar = c(0.1,0.1,0.1,0.1), cex.main=1.95, cex.axis=1.6)
plot(veg_na, legend = T, axes = F, add = F,
     col = c("#006837", "#31a354",
             "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
             "#ece7f2", "#a6bddb"),
     main = 'Végétation naturelle actuelle')
plot(cog1, axes=F, add=T)
# dev.off()


# https://stackoverflow.com/questions/75936000/how-to-fill-na-values-in-raster-with-nearest-neighbor-based-on-categorical-raste


veg1 = focal(veg_na, 11, "modal", na.policy = "only")


veg1_df = as.data.frame(veg1)
veg1_df = unique(veg1_df)

cls = data.frame(id = c(50, 60, 61, 62, 100, 110, 120, 122, 130, 160, 170, 180),
                 vegetation = c('tree_cover_broadleaved_evergreen', 'tree_cover_broadleaved_deciduous',
                                'tree_cover_broadleaved_deciduous', 'tree_cover_broadleaved_deciduous', 'tree_shrub_mosaic',
                                'herbaceous_cover_mosaic', 'shrubland', 'shrubland', 'grassland', 'tree_cover_flooded_fresh',
                                'tree_cover_flooded_saline', 'shrub_herbaceous_flooded'))

levels(veg1) = cls

levels(veg1)
cats(veg1)


# png("map_natural_vegetation_modelled.png", units="in", width=7, height=7.5, res=1000)
par(mfrow=c(1,1), mar = c(0.1,0.1,0.1,0.1), cex.main=1.95, cex.axis=1.6)
plot(veg1, legend = T, axes = F, add = F,
     col = c("#006837", "#31a354",
             "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
             "#ece7f2", "#a6bddb"),
     main = 'Végétation naturelle modelisée')
plot(cog1, axes=F, add=T)
# dev.off()

# veg11 = terra::crop(veg1, cog1)
# veg11 = terra::mask(veg1, cog1)

# terra::writeRaster(veg1, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\vegetation\\vegetation_modelled 6 Mar 2024.tiff", overwrite=T)

veg_df = as.data.frame(veg, xy = TRUE)

veg1_df = as.data.frame(veg1, xy = TRUE)


ggplot() + ggtitle("Carte de végétation actuelle") + theme_bw() +
  geom_raster(data = na.omit(veg_df), aes(x = x, y = y, fill = vegetation)) +
  geom_spatvector(data = cog1, fill = NA, linewidth = 1, color = "black") +
  scale_fill_manual(values = c("#e34a33", "#fdbb84", "#fee8c8", "#006837", "#31a354",
                               "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
                               "#ece7f2", "#a6bddb", "orangered", "orangered", "cornflowerblue")) +
  labs(fill = "Classes") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

ggsave("3. Localites//Gap filling//Vegetation actuelle.jpeg", units = "cm", width = 30, height = 25, dpi = 320)


ggplot() + ggtitle("Carte de végétation naturelle modélisée") + theme_bw() +
  geom_raster(data = na.omit(veg1_df), aes(x = x, y = y, fill = vegetation)) +
  geom_spatvector(data = cog1, fill = NA, linewidth = 1, color = "black") +
  scale_fill_manual(values = c("#006837", "#31a354",
                               "#78c679", "#d9f0a3", "#addd8e","#ffffcc", "#2b8cbe",
                               "#ece7f2", "#a6bddb")) +
  labs(fill = "Classes") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

ggsave("3. Localites//Gap filling//Vegetation naturelle modelisée.jpeg", units = "cm", width = 30, height = 25, dpi = 320)




