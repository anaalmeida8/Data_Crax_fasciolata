# -------------------------------------------------------------
# Cropping ensemble maps to Brazil and biomes extent
# 06 nov 2020
# Ana Claudia de Almeida
# -------------------------------------------------------------
#



# Loading packages --------------------------------------
library(colorRamps)
library(mapr)
library(raster)
library(sf)
library(tidyverse)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles")
getwd()

# Loading shapefile Brazil
bra <- raster::shapefile("Biome.shp") %>% 
  sf::st_as_sf()
bra

# plotting shapefile
ggplot() +
  geom_sf(data = bra) +
  theme_minimal()

# exporting limit
# sf::st_write(bra, "limit_bra_longlat_wgs84.shp")
# dir()


# adjusting variables extent 

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")
getwd()
dir()
ens <- raster::stack(c("CUR.bin_Crax_fasciolata.tif", "CUR.cont_Crax_fasciolata.tif",           
                       "Future_370_2050.bin_Crax_fasciolata.tif", "Future_370_2050.cont_Crax_fasciolata.tif",
                       "Future_585_2050.bin_Crax_fasciolata.tif", "Future_585_2050.cont_Crax_fasciolata.tif"))


# Cropping maps to Brazil extent
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles")
bra = shapefile("limit_bra_longlat_wgs84.shp")
ens.bra <- raster::crop(ens, bra) %>% 
  raster::mask(bra)
ens.bra

# plot
plot(ens.bra)

# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")
raster::writeRaster(x = ens.bra, filename = names(ens.bra), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)


###############################################################

# Amazon

# setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/AM")
# getwd()
# 
# 
# bra <- raster::shapefile("biome.shp") %>% 
#   sf::st_as_sf()
# bra
# 
# bra <- bra[bra$ID=="1",]
#
# 
# ggplot() +
#   geom_sf(data = bra) +
#   theme_minimal()
# 
# 
# # export limit
# sf::st_write(bra, "limit_Amazonia_longlat_wgs84.shp")
# dir()

# adjust variables extent

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

ens <- raster::stack(c("CUR.bin_Crax_fasciolata.tif", "CUR.cont_Crax_fasciolata.tif",           
                       "Future_370_2050.bin_Crax_fasciolata.tif", "Future_370_2050.cont_Crax_fasciolata.tif",
                       "Future_585_2050.bin_Crax_fasciolata.tif", "Future_585_2050.cont_Crax_fasciolata.tif"))


# crop
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/AM")
bra = shapefile("limit_Amazonia_longlat_wgs84.shp")
ens.bra <- raster::crop(ens, bra) %>% 
  raster::mask(bra)
ens.bra

# plot
plot(ens.bra)

# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")
raster::writeRaster(x = ens.bra, filename = names(ens.bra), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

###############################################################

# Pantanal

# setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/PA")
# getwd()
# 
# bra <- raster::shapefile("PA.shp") %>% 
#   sf::st_as_sf()
# bra
# 
# plot
# ggplot() +
#   geom_sf(data = bra) +
#   theme_minimal()
# 
# # export limit
# sf::st_write(bra, "limit_pantanal_longlat_wgs84.shp")
# dir()


# adjust variables extent


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

ens <- raster::stack(c("CUR.bin_Crax_fasciolata.tif", "CUR.cont_Crax_fasciolata.tif",           
                       "Future_370_2050.bin_Crax_fasciolata.tif", "Future_370_2050.cont_Crax_fasciolata.tif",
                       "Future_585_2050.bin_Crax_fasciolata.tif", "Future_585_2050.cont_Crax_fasciolata.tif"))


# crop
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/PA")
bra = shapefile("limit_pantanal_longlat_wgs84.shp")
ens.bra <- raster::crop(ens, bra) %>% 
  raster::mask(bra)
ens.bra

# plot
plot(ens.bra)

# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")
raster::writeRaster(x = ens.bra, filename = names(ens.bra), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

###############################################################

# Cerrado

# setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/CE")
# getwd()
#
# bra <- raster::shapefile("CE.shp") %>% 
#   sf::st_as_sf()
# bra
# 
# plot
# ggplot() +
#   geom_sf(data = bra) +
#   theme_minimal()
# 
# # export limit
# sf::st_write(bra, "limit_cerrado_longlat_wgs84.shp")
# dir()


# adjust variables extent

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

ens <- raster::stack(c("CUR.bin_Crax_fasciolata.tif", "CUR.cont_Crax_fasciolata.tif",           
                       "Future_370_2050.bin_Crax_fasciolata.tif", "Future_370_2050.cont_Crax_fasciolata.tif",
                       "Future_585_2050.bin_Crax_fasciolata.tif", "Future_585_2050.cont_Crax_fasciolata.tif",
                       "ens.cur.sd.w_Crax_fasciolata.tif", "ens.Future_370_2050.sd.w_Crax_fasciolata.tif",
                       "ens.Future_585_2050.sd.w_Crax_fasciolata.tif"))


# crop
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/CE")
bra = shapefile("limit_cerrado_longlat_wgs84.shp")
ens.bra <- raster::crop(ens, bra) %>% 
  raster::mask(bra)
ens.bra

# plot
plot(ens.bra)

# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")
raster::writeRaster(x = ens.bra, filename = names(ens.bra), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

###############################################################

# Atlantic Forest

# setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/MA")
# getwd()
#
# bra <- raster::shapefile("MA.shp") %>% 
#   sf::st_as_sf()
# bra
# 
## plot
# ggplot() +
#   geom_sf(data = bra) +
#   theme_minimal()
# 
# # export limit
# sf::st_write(bra, "limit_mata_atlantica_longlat_wgs84.shp")
# dir()
# 

# adjust variables extent


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")
getwd()
dir()
ens <- raster::stack(c("CUR.bin_Crax_fasciolata.tif", "CUR.cont_Crax_fasciolata.tif",           
                       "Future_370_2050.bin_Crax_fasciolata.tif", "Future_370_2050.cont_Crax_fasciolata.tif",
                       "Future_585_2050.bin_Crax_fasciolata.tif", "Future_585_2050.cont_Crax_fasciolata.tif"))


# crop
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/MA")
bra = shapefile("limit_mata_atlantica_longlat_wgs84.shp")
ens.bra <- raster::crop(ens, bra) %>% 
  raster::mask(bra)
ens.bra

# plot
plot(ens.bra)

# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")
raster::writeRaster(x = ens.bra, filename = names(ens.bra), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

###############################################################

###############################################################

# Caatinga

## adjust variables extent


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

ens <- raster::stack(c("CUR.bin_Crax_fasciolata.tif", "CUR.cont_Crax_fasciolata.tif",           
                       "Future_370_2050.bin_Crax_fasciolata.tif", "Future_370_2050.cont_Crax_fasciolata.tif",
                       "Future_585_2050.bin_Crax_fasciolata.tif", "Future_585_2050.cont_Crax_fasciolata.tif"))


# crop
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/CA")
bra = shapefile("caatinga_border.shp")
ens.bra <- raster::crop(ens, bra) %>% 
  raster::mask(bra)
ens.bra

# plot
plot(ens.bra)

# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")
raster::writeRaster(x = ens.bra, filename = names(ens.bra), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

###############################################################

# Pampa


## adjust variables extent

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

ens <- raster::stack(c("CUR.bin_Crax_fasciolata.tif", "CUR.cont_Crax_fasciolata.tif",           
                       "Future_370_2050.bin_Crax_fasciolata.tif", "Future_370_2050.cont_Crax_fasciolata.tif",
                       "Future_585_2050.bin_Crax_fasciolata.tif", "Future_585_2050.cont_Crax_fasciolata.tif"))


# crop
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/PAM")
bra = shapefile("biome_border.shp")
ens.bra <- raster::crop(ens, bra) %>% 
  raster::mask(bra)
ens.bra

# plot
plot(ens.bra)

# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")
raster::writeRaster(x = ens.bra, filename = names(ens.bra), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

###############################################################
