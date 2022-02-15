# -------------------------------------------------------------
# Getting GLOBIOM data
# 18 nov 2020
# Ana Claudia de Almeida
# -------------------------------------------------------------
#



# Carregar os pacotes: ----------------------------------------------------
library(raster)
library(rgdal)
library(rgeos)

# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m")


# Loading bioclimatic variable base raster -----------------
clima <- raster("bio_1.tif")


# Brazil map ----------------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles")
Brasil <- readOGR(".", "estadosl_2007")
plot (Brasil)


# Crop base raster to Brazil extent -----------------------
clima2 <- mask(crop(clima, extent(Brasil)),Brasil)
plot(clima2)

# GLOBIOM scenarios


#  IDC Imperfect3 - Moderate ------------------------------

setwd("./GLOBIOM/Dados_atualizados/LUC_IDCImperfect3")

IDCImperfect3_original <- readOGR(".", "LUC_IDCImperfect3")
names(IDCImperfect3_original)

ProtectedAreas <- readOGR("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/GLOBIOM/Dados_atualizados/ProtectedAreas", "ProtectedAreas")
names(ProtectedAreas)


# Current (native vegetation in 2000 - the best representation of Brazil native forest in 2000 - Soterroni et al., 2018)


IDC1 <- rasterize(IDCImperfect3_original, clima2, "MngFor2000")
IDC2 <- rasterize(IDCImperfect3_original, clima2, "PriFor2000")
PA <- rasterize(ProtectedAreas, clima2, "Val")

plot(IDC1)
plot(IDC2)
plot(PA)

IDCImperfect32000_native_forest <- mosaic(IDC1,IDC2,PA,fun=max)

plot(IDCImperfect32000_native_forest)

# Future (native forest in 2050)

IDC12050 <- rasterize(IDCImperfect3_original, clima2, "MngFor2050")
IDC22050 <- rasterize(IDCImperfect3_original, clima2, "PriFor2050")

IDCImperfect32050_native_forest <- mosaic(IDC12050, IDC22050, PA, fun=max)
 
plot(IDCImperfect32050_native_forest)


# No Forest Code scenario - Pessimistic --------------------------------------

NoFC <- readOGR("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/GLOBIOM/Dados_atualizados/LUC_NoFC", "LUC_NoFC")
#names(NoFC)


# Future NoFC (native forest in 2050) 


NoFC12050 <- rasterize(NoFC, clima2, "MngFor2050")
NoFC22050 <- rasterize(NoFC, clima2, "PriFor2050")

NoFC2050_native_forest <- mosaic(NoFC12050, NoFC22050, PA, fun=max)

plot(NoFC2050_native_forest)


# Forest Code - optimistic --------------------------

FC_original <- readOGR("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/GLOBIOM/Dados_atualizados/LUC_FC", "LUC_FC")
names(FC_original)

# Future FC (native forest in 2050)

FC1 <- rasterize(FC_original, clima2, "MngFor2050") 
FC2 <- rasterize(FC_original, clima2, "PriFor2050") 

FC2050_native_forest <- mosaic(FC1, FC2, PA, fun=max)

plot(FC2050_native_forest)


# Saving maps ---------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_0.7")
dir.create("GLOBIOM_Brasil")
setwd("GLOBIOM_Brasil")

writeRaster(IDCImperfect32000_native_forest, "Cover2000_nativa.tif")
writeRaster(IDCImperfect32050_native_forest, "CoverIDC2050_nativa.tif")
writeRaster(NoFC2050_native_forest, "CoverNoFC2050_nativa.tif")
writeRaster(FC2050_native_forest,"CoverFC2050_nativa.tif")
