# ---------------------------------------
# Bioclimatic variables selection
# 27 mai 2020
# ACA
# ---------------------------------------
#

# memory
rm(list = ls())

library(raster)
library(rgdal)
library(rgeos)
library(corrplot)
library(sp)

getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m")


# Loading biovars ------------------------------------------
# WorldClim version 2.1
var1 <- raster("bio_1.tif")
var2 <- raster("bio_2.tif")
var3 <- raster("bio_3.tif")
var4 <- raster("bio_4.tif")
var5 <- raster("bio_5.tif")
var6 <- raster("bio_6.tif")
var7 <- raster("bio_7.tif")
var8 <- raster("bio_8.tif")
var9 <- raster("bio_9.tif")
var10 <- raster("bio_10.tif")
var11 <- raster("bio_11.tif")
var12 <- raster("bio_12.tif")
var13 <- raster("bio_13.tif")
var14 <- raster("bio_14.tif")
var15 <- raster("bio_15.tif")
var16 <- raster("bio_16.tif")
var17 <- raster("bio_17.tif")
var18 <- raster("bio_18.tif")
var19 <- raster("bio_19.tif")

# Building a minimum convex polygon + buffer 20% ------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new")
Crax_fasciolata <- read.csv("Crax_fasciolata_thin1-5.csv")

crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
pres = Crax_fasciolata
coordinates(pres) <- c("lon", "lat")
proj4string(pres) <- crs.wgs84
pres.albers <- spTransform(pres, crs.albers)

mpc <- gConvexHull(pres.albers)
b <- (gArea(mpc)*2e-07)
buf <- gBuffer(mpc, width = b) # draw a buffer of 20% in km around of the minimum convex polygon (Barve et al. 2011)
pol.wgs <- spTransform(buf, crs.wgs84)

# Cropping the maps to mcp + buffer extent ------------------------------------------
e <- extent(pol.wgs) 
var1 <- crop(var1, e)
var2 <- crop(var2, e)
var3 <- crop(var3, e)
var4 <- crop(var4, e)
var5 <- crop(var5, e)
var6 <- crop(var6, e)
var7 <- crop(var7, e)
var8 <- crop(var8, e)
var9 <- crop(var9, e)
var10 <- crop(var10, e)
var11 <- crop(var11, e)
var12 <- crop(var12, e)
var13 <- crop(var13, e)
var14 <- crop(var14, e)
var15 <- crop(var15, e)
var16 <- crop(var16, e)
var17 <- crop(var17, e)
var18 <- crop(var18, e)
var19 <- crop(var19, e)

# Visualizing the two first variables
plot(var1)
plot(var2)


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
var1 <- mask(crop(var1, extent(pol.wgs)),pol.wgs)
plot(var1)
var2 <- mask(crop(var2, extent(pol.wgs)),pol.wgs)
var3 <- mask(crop(var3, extent(pol.wgs)),pol.wgs)
var4 <- mask(crop(var4, extent(pol.wgs)),pol.wgs)
var5 <- mask(crop(var5, extent(pol.wgs)),pol.wgs)
var6 <- mask(crop(var6, extent(pol.wgs)),pol.wgs)
var7 <- mask(crop(var7, extent(pol.wgs)),pol.wgs)
var8 <- mask(crop(var8, extent(pol.wgs)),pol.wgs)
var9 <- mask(crop(var9, extent(pol.wgs)),pol.wgs)
var10 <- mask(crop(var10, extent(pol.wgs)),pol.wgs)
var11 <- mask(crop(var11, extent(pol.wgs)),pol.wgs)
var12 <- mask(crop(var12, extent(pol.wgs)),pol.wgs)
var13 <- mask(crop(var13, extent(pol.wgs)),pol.wgs)
var14 <- mask(crop(var14, extent(pol.wgs)),pol.wgs)
var15 <- mask(crop(var15, extent(pol.wgs)),pol.wgs)
var16 <- mask(crop(var16, extent(pol.wgs)),pol.wgs)
var17 <- mask(crop(var17, extent(pol.wgs)),pol.wgs)
var18 <- mask(crop(var18, extent(pol.wgs)),pol.wgs)
var19 <- mask(crop(var19, extent(pol.wgs)),pol.wgs)
plot(var19)


# Stacking the variables

var <- stack(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,
             var13,var14,var15,var16,var17,var18,var19)         
         
plot(var)


# export
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/variaveis_cortadas/new/5km")
raster::writeRaster(x = var, filename = names(var), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

#Checking if records are inside the area
coordinates (Crax_fasciolata) <- ~lon+lat
proj4string (Crax_fasciolata)= CRS("+proj=longlat +datum=WGS84") # set it to UTM
sp_projetado <- spTransform(Crax_fasciolata,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(var1)
plot(sp_projetado, add=T, col= "red", pch=20)

# extracting values from biovars in each valid record
pixels.validos <- which(!is.na(var1[]))
random.points <- sample(pixels.validos, 100000, replace=T)


# extrair valores das biovariaveis para cada ponto
bio1 <- extract(var1, random.points)
bio2 <- extract(var2, random.points)
bio3 <- extract(var3, random.points)
bio4 <- extract(var4, random.points)
bio5 <- extract(var5, random.points)
bio6 <- extract(var6, random.points)
bio7 <- extract(var7, random.points)
bio8 <- extract(var8, random.points)
bio9 <- extract(var9, random.points)
bio10 <- extract(var10, random.points)
bio11 <- extract(var11, random.points)
bio12 <- extract(var12, random.points)
bio13 <- extract(var13, random.points)
bio14 <- extract(var14, random.points)
bio15 <- extract(var15, random.points)
bio16 <- extract(var16, random.points)
bio17 <- extract(var17, random.points)
bio18 <- extract(var18, random.points)
bio19 <- extract(var19, random.points)

# Excluding highly correlated variables

#install.packages("Rcpp")
#install.packages("caret")
library ('Rcpp')
library('caret')

#TODAS as variaveis
data <- data.frame(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, 
                   bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, 
                   bio19)

M <- cor(data)
corrplot(M,method="square")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new/5km")
tiff("corr.tif", w = 18, he = 18, units = "cm", res = 300)
corrplot(M,method="square")
dev.off()

df2 = cor(data)
hc = findCorrelation(df2, cutoff=0.6) #
hc = sort(hc)

reduced_Data = data[,-c(hc)]
reduced_Data
names(reduced_Data)

# [1] "bio4"  "bio8"  "bio14" "bio18" "bio19"

# bio4 = Temperature Seasonality (standard deviation ×100)
# bio8 = Mean Temperature of Wettest Quarter
# bio14 = Precipitation of Driest Month
# bio18 = Precipitation of Warmest Quarter
# bio19 = Precipitation of Coldest Quarter

# Visualizing correlation between selected variables
data <- data.frame(bio4, bio8, bio14, bio18, bio19)
m <- cor(data)
corrplot(m,method="square")
tiff("corr2.tif", w = 18, he = 18, units = "cm", res = 300)
corrplot(m,method="square")
dev.off()

###############################################################