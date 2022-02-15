# -------------------------------------------------------------
# Adjusting future variables to mcp + buffer
# 23 out 2020
# Ana Claudia de Almeida
# -------------------------------------------------------------
#


library(raster)
library(rgdal)
library(sp)
library(rgeos)


# MIROC6 (ssp585) --------------------------------------

# Extracting bands from raster
getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/MIROC6/ssp585")

miroc6 <- raster("MIROC6_ssp585.tif")

bio_1 <- raster("MIROC6_ssp585.tif", band = 1)
plot(bio_1)
bio_2 <- raster("MIROC6_ssp585.tif", band = 2)
bio_3 <- raster("MIROC6_ssp585.tif", band = 3)
bio_4 <- raster("MIROC6_ssp585.tif", band = 4)
bio_5 <- raster("MIROC6_ssp585.tif", band = 5)
bio_6 <- raster("MIROC6_ssp585.tif", band = 6)
bio_7 <- raster("MIROC6_ssp585.tif", band = 7)
bio_8 <- raster("MIROC6_ssp585.tif", band = 8)
bio_9 <- raster("MIROC6_ssp585.tif", band = 9)
bio_10 <- raster("MIROC6_ssp585.tif", band = 10)
bio_11 <- raster("MIROC6_ssp585.tif", band = 11)
bio_12 <- raster("MIROC6_ssp585.tif", band = 12)
bio_13 <- raster("MIROC6_ssp585.tif", band = 13)
bio_14 <- raster("MIROC6_ssp585.tif", band = 14)
bio_15 <- raster("MIROC6_ssp585.tif", band = 15)
bio_16 <- raster("MIROC6_ssp585.tif", band = 16)
bio_17 <- raster("MIROC6_ssp585.tif", band = 17)
bio_18 <- raster("MIROC6_ssp585.tif", band = 18)
bio_19 <- raster("MIROC6_ssp585.tif", band = 19)

# Building mcp + buffer 20% ------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new")
Crax_fasciolata <- read.csv("Crax_fasciolata_thin1-5.csv")

crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
pres = Crax_fasciolata
coordinates(pres) <- c("lon", "lat")
proj4string(pres) <- crs.wgs84  
pres.albers <- spTransform(pres, crs.albers)  

mpc <- gConvexHull(pres.albers) 
b <- (gArea(mpc)*2e-07) 
buf <- gBuffer(mpc, width = b) 
pol.wgs <- spTransform(buf, crs.wgs84)


# Cropping the maps to mcp + buffer extent ------------------------------------------
e <- extent(pol.wgs) 
var1 <- crop(bio_1, e)
var2 <- crop(bio_2, e)
var3 <- crop(bio_3, e)
var4 <- crop(bio_4, e)
var5 <- crop(bio_5, e)
var6 <- crop(bio_6, e)
var7 <- crop(bio_7, e)
var8 <- crop(bio_8, e)
var9 <- crop(bio_9, e)
var10 <- crop(bio_10, e)
var11 <- crop(bio_11, e)
var12 <- crop(bio_12, e)
var13 <- crop(bio_13, e)
var14 <- crop(bio_14, e)
var15 <- crop(bio_15, e)
var16 <- crop(bio_16, e)
var17 <- crop(bio_17, e)
var18 <- crop(bio_18, e)
var19 <- crop(bio_19, e)


# Projecting records and maps
nome <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(var1)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var3)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var4)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var5)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var6)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var7)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var8)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var9)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var10)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var11)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var12)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var13)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var14)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var15)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var16)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var17)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var18)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var19)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(pol.wgs)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

# Cropping biovars to mask (mcp + buffer)
bio_1 <- mask(crop(var1, extent(pol.wgs)),pol.wgs)
plot(bio_1)
bio_2 <- mask(crop(var2, extent(pol.wgs)),pol.wgs)
bio_3 <- mask(crop(var3, extent(pol.wgs)),pol.wgs)
bio_4 <- mask(crop(var4, extent(pol.wgs)),pol.wgs)
bio_5 <- mask(crop(var5, extent(pol.wgs)),pol.wgs)
bio_6 <- mask(crop(var6, extent(pol.wgs)),pol.wgs)
bio_7 <- mask(crop(var7, extent(pol.wgs)),pol.wgs)
bio_8 <- mask(crop(var8, extent(pol.wgs)),pol.wgs)
bio_9 <- mask(crop(var9, extent(pol.wgs)),pol.wgs)
bio_10 <- mask(crop(var10, extent(pol.wgs)),pol.wgs)
bio_11 <- mask(crop(var11, extent(pol.wgs)),pol.wgs)
bio_12 <- mask(crop(var12, extent(pol.wgs)),pol.wgs)
bio_13 <- mask(crop(var13, extent(pol.wgs)),pol.wgs)
bio_14 <- mask(crop(var14, extent(pol.wgs)),pol.wgs)
bio_15 <- mask(crop(var15, extent(pol.wgs)),pol.wgs)
bio_16 <- mask(crop(var16, extent(pol.wgs)),pol.wgs)
bio_17 <- mask(crop(var17, extent(pol.wgs)),pol.wgs)
bio_18 <- mask(crop(var18, extent(pol.wgs)),pol.wgs)
bio_19 <- mask(crop(var19, extent(pol.wgs)),pol.wgs)
plot(bio_19)

# Stacking the variables

var <- stack(bio_1,bio_2,bio_3,bio_4,bio_5,bio_6,bio_7,bio_8,bio_9,bio_10,
             bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,bio_17,bio_18,bio_19)

plot(var)


# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/MIROC6/ssp585/5km")
raster::writeRaster(x = var, filename = names(bio_1), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)




# BCC-CSM2-MR (ssp585) --------------------------------------

getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/BCC-CSM2-MR/ssp585")

bio_1 <- raster("BCC-CSM2-MR_ssp585.tif", band = 1)
plot(bio_1)
bio_2 <- raster("BCC-CSM2-MR_ssp585.tif", band = 2)
bio_3 <- raster("BCC-CSM2-MR_ssp585.tif", band = 3)
bio_4 <- raster("BCC-CSM2-MR_ssp585.tif", band = 4)
bio_5 <- raster("BCC-CSM2-MR_ssp585.tif", band = 5)
bio_6 <- raster("BCC-CSM2-MR_ssp585.tif", band = 6)
bio_7 <- raster("BCC-CSM2-MR_ssp585.tif", band = 7)
bio_8 <- raster("BCC-CSM2-MR_ssp585.tif", band = 8)
bio_9 <- raster("BCC-CSM2-MR_ssp585.tif", band = 9)
bio_10 <- raster("BCC-CSM2-MR_ssp585.tif", band = 10)
bio_11 <- raster("BCC-CSM2-MR_ssp585.tif", band = 11)
bio_12 <- raster("BCC-CSM2-MR_ssp585.tif", band = 12)
bio_13 <- raster("BCC-CSM2-MR_ssp585.tif", band = 13)
bio_14 <- raster("BCC-CSM2-MR_ssp585.tif", band = 14)
bio_15 <- raster("BCC-CSM2-MR_ssp585.tif", band = 15)
bio_16 <- raster("BCC-CSM2-MR_ssp585.tif", band = 16)
bio_17 <- raster("BCC-CSM2-MR_ssp585.tif", band = 17)
bio_18 <- raster("BCC-CSM2-MR_ssp585.tif", band = 18)
bio_19 <- raster("BCC-CSM2-MR_ssp585.tif", band = 19)

crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
pres = Crax_fasciolata
coordinates(pres) <- c("lon", "lat")
proj4string(pres) <- crs.wgs84  
pres.albers <- spTransform(pres, crs.albers)  

mpc <- gConvexHull(pres.albers)

b <- (gArea(mpc)*2e-07)
buf <- gBuffer(mpc, width = b) 
pol.wgs <- spTransform(buf, crs.wgs84)


# Cropping the maps to mcp + buffer extent ------------------------------------------
e <- extent(pol.wgs) 
var1 <- crop(bio_1, e)
var2 <- crop(bio_2, e)
var3 <- crop(bio_3, e)
var4 <- crop(bio_4, e)
var5 <- crop(bio_5, e)
var6 <- crop(bio_6, e)
var7 <- crop(bio_7, e)
var8 <- crop(bio_8, e)
var9 <- crop(bio_9, e)
var10 <- crop(bio_10, e)
var11 <- crop(bio_11, e)
var12 <- crop(bio_12, e)
var13 <- crop(bio_13, e)
var14 <- crop(bio_14, e)
var15 <- crop(bio_15, e)
var16 <- crop(bio_16, e)
var17 <- crop(bio_17, e)
var18 <- crop(bio_18, e)
var19 <- crop(bio_19, e)


# Projecting records and maps
nome <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(var1)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var3)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var4)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var5)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var6)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var7)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var8)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var9)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var10)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var11)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var12)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var13)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var14)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var15)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var16)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var17)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var18)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var19)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(pol.wgs)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

# Cropping biovars to mask (mcp + buffer)
bio_1 <- mask(crop(var1, extent(pol.wgs)),pol.wgs)
plot(bio_1)
bio_2 <- mask(crop(var2, extent(pol.wgs)),pol.wgs)
bio_3 <- mask(crop(var3, extent(pol.wgs)),pol.wgs)
bio_4 <- mask(crop(var4, extent(pol.wgs)),pol.wgs)
bio_5 <- mask(crop(var5, extent(pol.wgs)),pol.wgs)
bio_6 <- mask(crop(var6, extent(pol.wgs)),pol.wgs)
bio_7 <- mask(crop(var7, extent(pol.wgs)),pol.wgs)
bio_8 <- mask(crop(var8, extent(pol.wgs)),pol.wgs)
bio_9 <- mask(crop(var9, extent(pol.wgs)),pol.wgs)
bio_10 <- mask(crop(var10, extent(pol.wgs)),pol.wgs)
bio_11 <- mask(crop(var11, extent(pol.wgs)),pol.wgs)
bio_12 <- mask(crop(var12, extent(pol.wgs)),pol.wgs)
bio_13 <- mask(crop(var13, extent(pol.wgs)),pol.wgs)
bio_14 <- mask(crop(var14, extent(pol.wgs)),pol.wgs)
bio_15 <- mask(crop(var15, extent(pol.wgs)),pol.wgs)
bio_16 <- mask(crop(var16, extent(pol.wgs)),pol.wgs)
bio_17 <- mask(crop(var17, extent(pol.wgs)),pol.wgs)
bio_18 <- mask(crop(var18, extent(pol.wgs)),pol.wgs)
bio_19 <- mask(crop(var19, extent(pol.wgs)),pol.wgs)
plot(bio_19)

# Stacking the variables
var <- stack(bio_1,bio_2,bio_3,bio_4,bio_5,bio_6,bio_7,bio_8,bio_9,bio_10,
             bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,bio_17,bio_18,bio_19)

plot(var)


# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/BCC-CSM2-MR/ssp585/5km")
raster::writeRaster(x = var, filename = names(bio_1), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)




# IPSL-CM6A-LR (ssp585) --------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/IPSL-CM6A-LR/ssp585")

bio_1 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 1)
plot(bio_1)
bio_2 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 2)
bio_3 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 3)
bio_4 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 4)
bio_5 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 5)
bio_6 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 6)
bio_7 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 7)
bio_8 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 8)
bio_9 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 9)
bio_10 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 10)
bio_11 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 11)
bio_12 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 12)
bio_13 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 13)
bio_14 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 14)
bio_15 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 15)
bio_16 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 16)
bio_17 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 17)
bio_18 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 18)
bio_19 <- raster("IPSL-CM6A-LR_ssp585.tif", band = 19)


crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
pres = Crax_fasciolata
coordinates(pres) <- c("lon", "lat")
proj4string(pres) <- crs.wgs84  
pres.albers <- spTransform(pres, crs.albers)  

mpc <- gConvexHull(pres.albers) 
b <- (gArea(mpc)*2e-07) 
buf <- gBuffer(mpc, width = b) 
pol.wgs <- spTransform(buf, crs.wgs84)

# Cropping the maps to mcp + buffer extent ------------------------------------------
e <- extent(pol.wgs) 
var1 <- crop(bio_1, e)
var2 <- crop(bio_2, e)
var3 <- crop(bio_3, e)
var4 <- crop(bio_4, e)
var5 <- crop(bio_5, e)
var6 <- crop(bio_6, e)
var7 <- crop(bio_7, e)
var8 <- crop(bio_8, e)
var9 <- crop(bio_9, e)
var10 <- crop(bio_10, e)
var11 <- crop(bio_11, e)
var12 <- crop(bio_12, e)
var13 <- crop(bio_13, e)
var14 <- crop(bio_14, e)
var15 <- crop(bio_15, e)
var16 <- crop(bio_16, e)
var17 <- crop(bio_17, e)
var18 <- crop(bio_18, e)
var19 <- crop(bio_19, e)


# Projecting records and maps
nome <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(var1)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var3)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var4)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var5)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var6)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var7)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var8)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var9)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var10)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var11)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var12)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var13)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var14)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var15)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var16)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var17)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var18)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var19)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(pol.wgs)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

# Cropping biovars to mask (mcp + buffer)
bio_1 <- mask(crop(var1, extent(pol.wgs)),pol.wgs)
plot(bio_1)
bio_2 <- mask(crop(var2, extent(pol.wgs)),pol.wgs)
bio_3 <- mask(crop(var3, extent(pol.wgs)),pol.wgs)
bio_4 <- mask(crop(var4, extent(pol.wgs)),pol.wgs)
bio_5 <- mask(crop(var5, extent(pol.wgs)),pol.wgs)
bio_6 <- mask(crop(var6, extent(pol.wgs)),pol.wgs)
bio_7 <- mask(crop(var7, extent(pol.wgs)),pol.wgs)
bio_8 <- mask(crop(var8, extent(pol.wgs)),pol.wgs)
bio_9 <- mask(crop(var9, extent(pol.wgs)),pol.wgs)
bio_10 <- mask(crop(var10, extent(pol.wgs)),pol.wgs)
bio_11 <- mask(crop(var11, extent(pol.wgs)),pol.wgs)
bio_12 <- mask(crop(var12, extent(pol.wgs)),pol.wgs)
bio_13 <- mask(crop(var13, extent(pol.wgs)),pol.wgs)
bio_14 <- mask(crop(var14, extent(pol.wgs)),pol.wgs)
bio_15 <- mask(crop(var15, extent(pol.wgs)),pol.wgs)
bio_16 <- mask(crop(var16, extent(pol.wgs)),pol.wgs)
bio_17 <- mask(crop(var17, extent(pol.wgs)),pol.wgs)
bio_18 <- mask(crop(var18, extent(pol.wgs)),pol.wgs)
bio_19 <- mask(crop(var19, extent(pol.wgs)),pol.wgs)
plot(bio_19)

# Stacking the variables

var <- stack(bio_1,bio_2,bio_3,bio_4,bio_5,bio_6,bio_7,bio_8,bio_9,bio_10,
             bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,bio_17,bio_18,bio_19)

plot(var)


# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/IPSL-CM6A-LR/ssp585/5km")
raster::writeRaster(x = var, filename = names(bio_1), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)


# MIROC6 (ssp370) --------------------------------------

getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/MIROC6/ssp370")

bio_1 <- raster("MIROC6_ssp370.tif", band = 1)
bio_2 <- raster("MIROC6_ssp370.tif", band = 2)
bio_3 <- raster("MIROC6_ssp370.tif", band = 3)
bio_4 <- raster("MIROC6_ssp370.tif", band = 4)
bio_5 <- raster("MIROC6_ssp370.tif", band = 5)
bio_6 <- raster("MIROC6_ssp370.tif", band = 6)
bio_7 <- raster("MIROC6_ssp370.tif", band = 7)
bio_8 <- raster("MIROC6_ssp370.tif", band = 8)
bio_9 <- raster("MIROC6_ssp370.tif", band = 9)
bio_10 <- raster("MIROC6_ssp370.tif", band = 10)
bio_11 <- raster("MIROC6_ssp370.tif", band = 11)
bio_12 <- raster("MIROC6_ssp370.tif", band = 12)
bio_13 <- raster("MIROC6_ssp370.tif", band = 13)
bio_14 <- raster("MIROC6_ssp370.tif", band = 14)
bio_15 <- raster("MIROC6_ssp370.tif", band = 15)
bio_16 <- raster("MIROC6_ssp370.tif", band = 16)
bio_17 <- raster("MIROC6_ssp370.tif", band = 17)
bio_18 <- raster("MIROC6_ssp370.tif", band = 18)
bio_19 <- raster("MIROC6_ssp370.tif", band = 19)

# Building a minimum convex polygon + buffer 20% ------------------------------------------

crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
pres = Crax_fasciolata
coordinates(pres) <- c("lon", "lat")
proj4string(pres) <- crs.wgs84  
pres.albers <- spTransform(pres, crs.albers)  

mpc <- gConvexHull(pres.albers) 
b <- (gArea(mpc)*2e-07)
buf <- gBuffer(mpc, width = b) 
pol.wgs <- spTransform(buf, crs.wgs84)


# Cropping the maps to mcp + buffer extent ------------------------------------------
e <- extent(pol.wgs) 
var1 <- crop(bio_1, e)
var2 <- crop(bio_2, e)
var3 <- crop(bio_3, e)
var4 <- crop(bio_4, e)
var5 <- crop(bio_5, e)
var6 <- crop(bio_6, e)
var7 <- crop(bio_7, e)
var8 <- crop(bio_8, e)
var9 <- crop(bio_9, e)
var10 <- crop(bio_10, e)
var11 <- crop(bio_11, e)
var12 <- crop(bio_12, e)
var13 <- crop(bio_13, e)
var14 <- crop(bio_14, e)
var15 <- crop(bio_15, e)
var16 <- crop(bio_16, e)
var17 <- crop(bio_17, e)
var18 <- crop(bio_18, e)
var19 <- crop(bio_19, e)


# Projecting records and maps
nome <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(var1)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var3)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var4)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var5)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var6)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var7)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var8)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var9)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var10)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var11)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var12)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var13)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var14)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var15)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var16)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var17)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var18)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var19)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
#proj4string(elev)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(pol.wgs)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

# Cropping biovars to mask (mcp + buffer)
bio_1 <- mask(crop(var1, extent(pol.wgs)),pol.wgs)
plot(bio_1)
bio_2 <- mask(crop(var2, extent(pol.wgs)),pol.wgs)
bio_3 <- mask(crop(var3, extent(pol.wgs)),pol.wgs)
bio_4 <- mask(crop(var4, extent(pol.wgs)),pol.wgs)
bio_5 <- mask(crop(var5, extent(pol.wgs)),pol.wgs)
bio_6 <- mask(crop(var6, extent(pol.wgs)),pol.wgs)
bio_7 <- mask(crop(var7, extent(pol.wgs)),pol.wgs)
bio_8 <- mask(crop(var8, extent(pol.wgs)),pol.wgs)
bio_9 <- mask(crop(var9, extent(pol.wgs)),pol.wgs)
bio_10 <- mask(crop(var10, extent(pol.wgs)),pol.wgs)
bio_11 <- mask(crop(var11, extent(pol.wgs)),pol.wgs)
bio_12 <- mask(crop(var12, extent(pol.wgs)),pol.wgs)
bio_13 <- mask(crop(var13, extent(pol.wgs)),pol.wgs)
bio_14 <- mask(crop(var14, extent(pol.wgs)),pol.wgs)
bio_15 <- mask(crop(var15, extent(pol.wgs)),pol.wgs)
bio_16 <- mask(crop(var16, extent(pol.wgs)),pol.wgs)
bio_17 <- mask(crop(var17, extent(pol.wgs)),pol.wgs)
bio_18 <- mask(crop(var18, extent(pol.wgs)),pol.wgs)
bio_19 <- mask(crop(var19, extent(pol.wgs)),pol.wgs)
plot(bio_19)

# Stacking the variables

var <- stack(bio_1,bio_2,bio_3,bio_4,bio_5,bio_6,bio_7,bio_8,bio_9,bio_10,
             bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,bio_17,bio_18,bio_19)

plot(var)


# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/MIROC6/ssp370/5km")
raster::writeRaster(x = var, filename = names(bio_1), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)



# BCC-CSM2-MR (ssp370) --------------------------------------

getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/BCC-CSM2-MR/ssp370")

bio_1 <- raster("BCC-CSM2-MR_ssp370.tif", band = 1)
bio_2 <- raster("BCC-CSM2-MR_ssp370.tif", band = 2)
bio_3 <- raster("BCC-CSM2-MR_ssp370.tif", band = 3)
bio_4 <- raster("BCC-CSM2-MR_ssp370.tif", band = 4)
bio_5 <- raster("BCC-CSM2-MR_ssp370.tif", band = 5)
bio_6 <- raster("BCC-CSM2-MR_ssp370.tif", band = 6)
bio_7 <- raster("BCC-CSM2-MR_ssp370.tif", band = 7)
bio_8 <- raster("BCC-CSM2-MR_ssp370.tif", band = 8)
bio_9 <- raster("BCC-CSM2-MR_ssp370.tif", band = 9)
bio_10 <- raster("BCC-CSM2-MR_ssp370.tif", band = 10)
bio_11 <- raster("BCC-CSM2-MR_ssp370.tif", band = 11)
bio_12 <- raster("BCC-CSM2-MR_ssp370.tif", band = 12)
bio_13 <- raster("BCC-CSM2-MR_ssp370.tif", band = 13)
bio_14 <- raster("BCC-CSM2-MR_ssp370.tif", band = 14)
bio_15 <- raster("BCC-CSM2-MR_ssp370.tif", band = 15)
bio_16 <- raster("BCC-CSM2-MR_ssp370.tif", band = 16)
bio_17 <- raster("BCC-CSM2-MR_ssp370.tif", band = 17)
bio_18 <- raster("BCC-CSM2-MR_ssp370.tif", band = 18)
bio_19 <- raster("BCC-CSM2-MR_ssp370.tif", band = 19)

# Building a minimum convex polygon + buffer 20% ------------------------------------------

crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
pres = Crax_fasciolata
coordinates(pres) <- c("lon", "lat")
proj4string(pres) <- crs.wgs84  
pres.albers <- spTransform(pres, crs.albers)

mpc <- gConvexHull(pres.albers) 
b <- (gArea(mpc)*2e-07) 
buf <- gBuffer(mpc, width = b) 
pol.wgs <- spTransform(buf, crs.wgs84)


# Cropping the maps to mcp + buffer extent ------------------------------------------
e <- extent(pol.wgs) 
var1 <- crop(bio_1, e)
var2 <- crop(bio_2, e)
var3 <- crop(bio_3, e)
var4 <- crop(bio_4, e)
var5 <- crop(bio_5, e)
var6 <- crop(bio_6, e)
var7 <- crop(bio_7, e)
var8 <- crop(bio_8, e)
var9 <- crop(bio_9, e)
var10 <- crop(bio_10, e)
var11 <- crop(bio_11, e)
var12 <- crop(bio_12, e)
var13 <- crop(bio_13, e)
var14 <- crop(bio_14, e)
var15 <- crop(bio_15, e)
var16 <- crop(bio_16, e)
var17 <- crop(bio_17, e)
var18 <- crop(bio_18, e)
var19 <- crop(bio_19, e)

# Projecting records and maps
nome <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(var1)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var3)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var4)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var5)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var6)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var7)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var8)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var9)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var10)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var11)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var12)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var13)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var14)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var15)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var16)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var17)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var18)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var19)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(pol.wgs)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

# Cropping biovars to mask (mcp + buffer)
bio_1 <- mask(crop(var1, extent(pol.wgs)),pol.wgs)
bio_2 <- mask(crop(var2, extent(pol.wgs)),pol.wgs)
bio_3 <- mask(crop(var3, extent(pol.wgs)),pol.wgs)
bio_4 <- mask(crop(var4, extent(pol.wgs)),pol.wgs)
bio_5 <- mask(crop(var5, extent(pol.wgs)),pol.wgs)
bio_6 <- mask(crop(var6, extent(pol.wgs)),pol.wgs)
bio_7 <- mask(crop(var7, extent(pol.wgs)),pol.wgs)
bio_8 <- mask(crop(var8, extent(pol.wgs)),pol.wgs)
bio_9 <- mask(crop(var9, extent(pol.wgs)),pol.wgs)
bio_10 <- mask(crop(var10, extent(pol.wgs)),pol.wgs)
bio_11 <- mask(crop(var11, extent(pol.wgs)),pol.wgs)
bio_12 <- mask(crop(var12, extent(pol.wgs)),pol.wgs)
bio_13 <- mask(crop(var13, extent(pol.wgs)),pol.wgs)
bio_14 <- mask(crop(var14, extent(pol.wgs)),pol.wgs)
bio_15 <- mask(crop(var15, extent(pol.wgs)),pol.wgs)
bio_16 <- mask(crop(var16, extent(pol.wgs)),pol.wgs)
bio_17 <- mask(crop(var17, extent(pol.wgs)),pol.wgs)
bio_18 <- mask(crop(var18, extent(pol.wgs)),pol.wgs)
bio_19 <- mask(crop(var19, extent(pol.wgs)),pol.wgs)
plot(bio_19)

# Stacking the variables

var <- stack(bio_1,bio_2,bio_3,bio_4,bio_5,bio_6,bio_7,bio_8,bio_9,bio_10,
             bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,bio_17,bio_18,bio_19)

plot(var)


# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/BCC-CSM2-MR/ssp370/5km")
raster::writeRaster(x = var, filename = names(bio_1), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)



# IPSL-CM6A-LR (ssp370) --------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/IPSL-CM6A-LR/ssp370")

bio_1 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 1)
bio_2 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 2)
bio_3 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 3)
bio_4 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 4)
bio_5 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 5)
bio_6 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 6)
bio_7 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 7)
bio_8 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 8)
bio_9 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 9)
bio_10 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 10)
bio_11 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 11)
bio_12 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 12)
bio_13 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 13)
bio_14 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 14)
bio_15 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 15)
bio_16 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 16)
bio_17 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 17)
bio_18 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 18)
bio_19 <- raster("IPSL-CM6A-LR_ssp370.tif", band = 19)

# Building a minimum convex polygon + buffer 20% ------------------------------------------

crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
pres = Crax_fasciolata
coordinates(pres) <- c("lon", "lat")
proj4string(pres) <- crs.wgs84
pres.albers <- spTransform(pres, crs.albers)

mpc <- gConvexHull(pres.albers)
b <- (gArea(mpc)*2e-07)
buf <- gBuffer(mpc, width = b) 
pol.wgs <- spTransform(buf, crs.wgs84)


# Cropping the maps to mcp + buffer extent ------------------------------------------
e <- extent(pol.wgs) 
var1 <- crop(bio_1, e)
var2 <- crop(bio_2, e)
var3 <- crop(bio_3, e)
var4 <- crop(bio_4, e)
var5 <- crop(bio_5, e)
var6 <- crop(bio_6, e)
var7 <- crop(bio_7, e)
var8 <- crop(bio_8, e)
var9 <- crop(bio_9, e)
var10 <- crop(bio_10, e)
var11 <- crop(bio_11, e)
var12 <- crop(bio_12, e)
var13 <- crop(bio_13, e)
var14 <- crop(bio_14, e)
var15 <- crop(bio_15, e)
var16 <- crop(bio_16, e)
var17 <- crop(bio_17, e)
var18 <- crop(bio_18, e)
var19 <- crop(bio_19, e)


# Projecting records and maps
nome <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(var1)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var2)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var3)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var4)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var5)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var6)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var7)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var8)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var9)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var10)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var11)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var12)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var13)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var14)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var15)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var16)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var17)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var18)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(var19)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
proj4string(pol.wgs)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

# Cropping biovars to mask (mcp + buffer)
bio_1 <- mask(crop(var1, extent(pol.wgs)),pol.wgs)
bio_2 <- mask(crop(var2, extent(pol.wgs)),pol.wgs)
bio_3 <- mask(crop(var3, extent(pol.wgs)),pol.wgs)
bio_4 <- mask(crop(var4, extent(pol.wgs)),pol.wgs)
bio_5 <- mask(crop(var5, extent(pol.wgs)),pol.wgs)
bio_6 <- mask(crop(var6, extent(pol.wgs)),pol.wgs)
bio_7 <- mask(crop(var7, extent(pol.wgs)),pol.wgs)
bio_8 <- mask(crop(var8, extent(pol.wgs)),pol.wgs)
bio_9 <- mask(crop(var9, extent(pol.wgs)),pol.wgs)
bio_10 <- mask(crop(var10, extent(pol.wgs)),pol.wgs)
bio_11 <- mask(crop(var11, extent(pol.wgs)),pol.wgs)
bio_12 <- mask(crop(var12, extent(pol.wgs)),pol.wgs)
bio_13 <- mask(crop(var13, extent(pol.wgs)),pol.wgs)
bio_14 <- mask(crop(var14, extent(pol.wgs)),pol.wgs)
bio_15 <- mask(crop(var15, extent(pol.wgs)),pol.wgs)
bio_16 <- mask(crop(var16, extent(pol.wgs)),pol.wgs)
bio_17 <- mask(crop(var17, extent(pol.wgs)),pol.wgs)
bio_18 <- mask(crop(var18, extent(pol.wgs)),pol.wgs)
bio_19 <- mask(crop(var19, extent(pol.wgs)),pol.wgs)
plot(bio_19)

# Stacking the variables

var <- stack(bio_1,bio_2,bio_3,bio_4,bio_5,bio_6,bio_7,bio_8,bio_9,bio_10,
             bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,bio_17,bio_18,bio_19)

plot(var)


# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/Future/IPSL-CM6A-LR/ssp370/5km")
raster::writeRaster(x = var, filename = names(bio_1), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

##############################################################################################
