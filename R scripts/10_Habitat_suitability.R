# -------------------------------------------------------------
# Calculating suitability habitat for Brazil and Brazilian
# domains
# 14 jan 2021
# Ana Claudia de Almeida
# -------------------------------------------------------------


# Loading packages -----------------------------------------------------
library(raster)
library(rgdal)
library (beepr)
library(GISTools)
library(prettymapr)


# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")


# Land use -------------------------------------------------------------

# Current ----------------------------------------------------------------

adeq_clim_cur <- raster("CUR.cont_Crax_fasciolata.tif") 
plot(adeq_clim_cur)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil")
cur_cover <- raster("Cover2000_nativa.tif")
plot(cur_cover)

setMinMax(cur_cover) 
cur_cover <- crop(cur_cover, extent(adeq_clim_cur))
cur_cover <- mask (cur_cover, adeq_clim_cur)/320
plot(cur_cover)

raster(cur_cover)
raster(adeq_clim_cur)


# Future (moderate) ----------------------------------------------------
fut_cover_IDC <- raster("./CoverIDC2050_nativa.tif")
fut_cover_IDC <- crop(fut_cover_IDC, extent(adeq_clim_cur))
fut_cover_IDC <- mask (fut_cover_IDC, adeq_clim_cur)/320

# Future (pessimistic) ----------------------------------------------------
fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
fut_cover_NoFC <- crop(fut_cover_NoFC, extent(adeq_clim_cur))
fut_cover_NoFC <- mask (fut_cover_NoFC, adeq_clim_cur)/320

# Future (Optimistic) --------------------------------------------------------
fut_cover_FC <- raster("./CoverFC2050_nativa.tif")
fut_cover_FC <- crop(fut_cover_FC, extent(adeq_clim_cur))
fut_cover_FC <- mask (fut_cover_FC, adeq_clim_cur)/320


#  Calculating % of native vegetation change

# Moderate -------------------------------------------------------------
valid.pixel <- !is.na(cur_cover[])
valid.pixel2 <- !is.na(fut_cover_IDC[])
forest.change.IDC <- (sum(fut_cover_IDC[valid.pixel2])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.IDC #-14.56275


# Pessimistic -------------------------------------------------------------
valid.pixel3 <- !is.na(fut_cover_NoFC[])
forest.change.NoFC <- (sum(fut_cover_NoFC[valid.pixel3])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.NoFC #-18.15397

# Optimistic ---------------------------------------------------------
valid.pixel4 <- !is.na(fut_cover_FC[])
forest.change.FC <- (sum(fut_cover_FC[valid.pixel4])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.FC #-10.63481


# Saving maps ---------------------------------------------------------

getwd()
dir.create("Maps")
setwd("Maps")

writeRaster(cur_cover, "forest.current_Crax_fasciolata.tif")
writeRaster(fut_cover_IDC, "forest.future.IDCImperfect3_Crax_fasciolata.tif")
writeRaster(fut_cover_NoFC, "forest.future.NoFC_Crax_fasciolata.tif")
writeRaster(fut_cover_FC, "forest.future.FC_Crax_fasciolata.tif")

# AREA

# current

Crax_cur_area <- cur_cover
# Crax_cur_bin #898, 832, 747136  (nrow, ncol, ncell)
Crax_cur_area[Crax_cur_area == 0] <- NA
plot(Crax_cur_area)

r = raster(nrow=898, ncol=832, xmn=-73.54167, xmx=-38.875, ymn=-33.75, ymx=3.666667) 
x = raster::area(r) 
plot(x)
occurrence_area = x * Crax_cur_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #2,803,689 KM2


# Moderate

Crax_IDC_area <- fut_cover_IDC
plot(Crax_IDC_area)
occurrence_area = x * Crax_IDC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #2,396,645 KM2

# Pessimistic
Crax_NoFC_area <- fut_cover_NoFC
plot(Crax_NoFC_area)
occurrence_area = x * Crax_NoFC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #2,295,441 KM2

# Optimistic
Crax_FC_area <- fut_cover_FC
plot(Crax_FC_area)
occurrence_area = x * Crax_FC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #2,507,086 KM2


fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
cur_cover <- raster("./forest.current_Crax_fasciolata.tif")
fut_cover_IDC <- raster("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
fut_cover_NoFC <- raster("./forest.future.NoFC_Crax_fasciolata.tif")
fut_cover_FC <- raster("./forest.future.FC_Crax_fasciolata.tif")

mudancaIDC <- fut_cover_IDC - cur_cover
plot(cur_cover)
plot(fut_cover_IDC)
plot(mudancaIDC)

mudancaFC <- fut_cover_FC - cur_cover
plot(cur_cover)
plot(fut_cover_FC)
plot(mudancaFC)

mudancaNoFC <- fut_cover_NoFC - cur_cover
plot(cur_cover)
plot(fut_cover_NoFC)
plot(mudancaNoFC)

writeRaster(mudancaIDC, "mudancaIDC.tif")
writeRaster(mudancaFC, "mudancaFC.tif")
writeRaster(mudancaNoFC, "mudancaNoFC.tif")

###############################################################


# BRAZILIAN DOMAINS --------------------------------------

# 1) Atlantic Forest --------------------------------------

# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")



# Current ----------------------------------------------------------------

adeq_clim_cur <- raster("CUR.cont_Crax_fasciolata.tif") 
plot(adeq_clim_cur)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil")
cur_cover <- raster("Cover2000_nativa.tif")
plot(cur_cover)

setMinMax(cur_cover) 
cur_cover <- crop(cur_cover, extent(adeq_clim_cur))
cur_cover <- mask (cur_cover, adeq_clim_cur)/320
cur_cover 
plot(cur_cover)

raster(cur_cover)
raster(adeq_clim_cur)


# Future (moderate) ----------------------------------------------------
fut_cover_IDC <- raster("./CoverIDC2050_nativa.tif")
fut_cover_IDC <- crop(fut_cover_IDC, extent(adeq_clim_cur))
fut_cover_IDC <- mask(fut_cover_IDC, adeq_clim_cur)/320
plot(fut_cover_IDC)

# Future (pessimistic) ----------------------------------------------------
fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
fut_cover_NoFC <- crop(fut_cover_NoFC, extent(adeq_clim_cur))
fut_cover_NoFC <- mask (fut_cover_NoFC, adeq_clim_cur)/320
plot(fut_cover_NoFC) 

# Future (optimistic) --------------------------------------------------------
fut_cover_FC <- raster("./CoverFC2050_nativa.tif")
fut_cover_FC <- crop(fut_cover_FC, extent(adeq_clim_cur))
fut_cover_FC <- mask (fut_cover_FC, adeq_clim_cur)/320
plot(fut_cover_FC) 


# Moderate -------------------------------------------------------------
valid.pixel <- !is.na(cur_cover[])
which(valid.pixel)
valid.pixel2 <- !is.na(fut_cover_IDC[])
which(valid.pixel2)
forest.change.IDC <- (sum(fut_cover_IDC[valid.pixel2])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.IDC #0.1266551

# Pessimistic -------------------------------------------------------------
valid.pixel3 <- !is.na(fut_cover_NoFC[])
forest.change.NoFC <- (sum(fut_cover_NoFC[valid.pixel3])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.NoFC #0.1346607


# Optimistic ---------------------------------------------------------
valid.pixel4 <- !is.na(fut_cover_FC[])
forest.change.FC <- (sum(fut_cover_FC[valid.pixel4])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.FC #0.08088855


# Saving maps ---------------------------------------------------------

getwd()
setwd("./Maps")
dir.create("AF")
setwd("AF")

writeRaster(cur_cover, "forest.current_Crax_fasciolata.tif")
writeRaster(fut_cover_IDC, "forest.future.IDCImperfect3_Crax_fasciolata.tif")
writeRaster(fut_cover_NoFC, "forest.future.NoFC_Crax_fasciolata.tif")
writeRaster(fut_cover_FC, "forest.future.FC_Crax_fasciolata.tif")

# Current
Crax_cur_area <- cur_cover
plot(Crax_cur_area)
r = raster(nrow=596, ncol=403, xmn=-55.66667, xmx=-38.875, ymn=-29.95833, ymx=-5.125) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #134,032.3 KM2

# Moderate

Crax_IDC_area <- fut_cover_IDC
plot(Crax_IDC_area)
occurrence_area = x * Crax_IDC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #134,201 KM2

# Pessimistic
Crax_NoFC_area <- fut_cover_NoFC
plot(Crax_NoFC_area)
occurrence_area = x * Crax_NoFC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #134,212 KM2

# optimistic
Crax_FC_area <- fut_cover_FC
plot(Crax_FC_area)
occurrence_area = x * Crax_FC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #134,135.2 KM2


# 2) Pantanal --------------------------------------

# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")


# Current ----------------------------------------------------------------

adeq_clim_cur <- raster("CUR.cont_Crax_fasciolata.tif") 
plot(adeq_clim_cur)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil")
cur_cover <- raster("Cover2000_nativa.tif")
plot(cur_cover)

setMinMax(cur_cover) 
cur_cover <- crop(cur_cover, extent(adeq_clim_cur))
setMinMax(cur_cover)
cur_cover <- mask (cur_cover, adeq_clim_cur)/320
plot(cur_cover)

colors <- c("white","orange","darkgreen")
colors <- colorRampPalette(colors)(50)
plot(cur_cover,col=colors)

raster(cur_cover)
raster(adeq_clim_cur)

# Future (moderate) ----------------------------------------------------
fut_cover_IDC <- raster("./CoverIDC2050_nativa.tif")
fut_cover_IDC <- crop(fut_cover_IDC, extent(adeq_clim_cur))
fut_cover_IDC <- mask (fut_cover_IDC, adeq_clim_cur)/320
plot(fut_cover_IDC,col=colors)

# Future (pessimistic) ----------------------------------------------------
fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
fut_cover_NoFC <- crop(fut_cover_NoFC, extent(adeq_clim_cur))
fut_cover_NoFC <- mask (fut_cover_NoFC, adeq_clim_cur)/320
plot(fut_cover_NoFC,col=colors)

# Future (Optimistic) --------------------------------------------------------
fut_cover_FC <- raster("./CoverFC2050_nativa.tif")
fut_cover_FC <- crop(fut_cover_FC, extent(adeq_clim_cur))
fut_cover_FC <- mask (fut_cover_FC, adeq_clim_cur)/320
plot(fut_cover_FC,col=colors)


# Moderate -------------------------------------------------------------
valid.pixel <- !is.na(fut_cover_IDC[])
pv <- !is.na(cur_cover[])
forest.change.IDC <- (sum(fut_cover_IDC[valid.pixel])-sum(cur_cover[pv]))/sum(cur_cover[pv])*100
forest.change.IDC #-33.84535

# Pessimistic -------------------------------------------------------------
valid.pixel <- !is.na(fut_cover_NoFC[])
forest.change.NoFC <- (sum(fut_cover_NoFC[valid.pixel])-sum(cur_cover[pv]))/sum(cur_cover[pv])*100
forest.change.NoFC #-30.50292


# Optimistic ---------------------------------------------------------
valid.pixel <- !is.na(fut_cover_FC[])
forest.change.FC <- (sum(fut_cover_FC[valid.pixel])-sum(cur_cover[pv]))/sum(cur_cover[pv])*100
forest.change.FC #-42.60578


# Saving maps ---------------------------------------------------------

getwd()
setwd("./Maps")
dir.create("Pantanal")
setwd("Pantanal")

writeRaster(cur_cover, "forest.current_Crax_fasciolata.tif")
writeRaster(fut_cover_IDC, "forest.future.IDCImperfect3_Crax_fasciolata.tif")
writeRaster(fut_cover_NoFC, "forest.future.NoFC_Crax_fasciolata.tif")
writeRaster(fut_cover_FC, "forest.future.FC_Crax_fasciolata.tif")


# Current
Crax_cur_area <- cur_cover
plot(Crax_cur_area)
r = raster(nrow=159, ncol=102, xmn=-59.16667, xmx=-54.91667, ymn=-22.16667, ymx=-15.54167) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #41,823.03 KM2


# Moderate

Crax_IDC_area <- fut_cover_IDC
plot(Crax_IDC_area)
occurrence_area = x * Crax_IDC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #27,613.24 KM2

# Pessimistic
Crax_NoFC_area <- fut_cover_NoFC
plot(Crax_NoFC_area)
occurrence_area = x * Crax_NoFC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #28,989.6 KM2

# Optimistic
Crax_FC_area <- fut_cover_FC
plot(Crax_FC_area)
occurrence_area = x * Crax_FC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #23,970.1 KM2

# 3) Cerrado --------------------------------------

# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")


# Current ----------------------------------------------------------------

adeq_clim_cur <- raster("CUR.cont_Crax_fasciolata.tif") 
plot(adeq_clim_cur)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil")
cur_cover <- raster("Cover2000_nativa.tif")
plot(cur_cover)

setMinMax(cur_cover) 
cur_cover <- crop(cur_cover, extent(adeq_clim_cur))
setMinMax(cur_cover)
cur_cover <- mask (cur_cover, adeq_clim_cur)/320
plot(cur_cover)

raster(cur_cover)
raster(adeq_clim_cur)


# Future (Moderate) ----------------------------------------------------
fut_cover_IDC <- raster("./CoverIDC2050_nativa.tif")
fut_cover_IDC <- crop(fut_cover_IDC, extent(adeq_clim_cur))
fut_cover_IDC <- mask (fut_cover_IDC, adeq_clim_cur)/320
plot(fut_cover_IDC)

# Future (Pessimistic) ----------------------------------------------------
fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
fut_cover_NoFC <- crop(fut_cover_NoFC, extent(adeq_clim_cur))
fut_cover_NoFC <- mask (fut_cover_NoFC, adeq_clim_cur)/320
plot(fut_cover_NoFC)

# Future (Optimistic) --------------------------------------------------------
fut_cover_FC <- raster("./CoverFC2050_nativa.tif")
fut_cover_FC <- crop(fut_cover_FC, extent(adeq_clim_cur))
fut_cover_FC <- mask (fut_cover_FC, adeq_clim_cur)/320
plot(fut_cover_FC)


# Moderate -------------------------------------------------------------
valid.pixel2 <- !is.na(fut_cover_IDC[])
valid.pixel <- !is.na(cur_cover[])
forest.change.IDC <- (sum(fut_cover_IDC[valid.pixel2])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.IDC #-30.33722

# Pessimistic -------------------------------------------------------------
valid.pixel3 <- !is.na(fut_cover_NoFC[])
forest.change.NoFC <- (sum(fut_cover_NoFC[valid.pixel3])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.NoFC #-28.91175

# Optimistic ---------------------------------------------------------
valid.pixel4 <- !is.na(fut_cover_FC[])
forest.change.FC <- (sum(fut_cover_FC[valid.pixel4])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.FC #-23.4029


# Saving maps ---------------------------------------------------------

getwd()
setwd("./Maps")
dir.create("Cerrado")
setwd("Cerrado")

writeRaster(cur_cover, "forest.current_Crax_fasciolata.tif")
writeRaster(fut_cover_IDC, "forest.future.IDCImperfect3_Crax_fasciolata.tif")
writeRaster(fut_cover_NoFC, "forest.future.NoFC_Crax_fasciolata.tif")
writeRaster(fut_cover_FC, "forest.future.FC_Crax_fasciolata.tif")


# Cerrado
# Current
Crax_cur_area <- cur_cover
plot(Crax_cur_area)
r = raster(nrow=536, ncol=446, xmn=-60.125, xmx=-41.54167, ymn=-24.66667, ymx=-2.333333) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #476,771.2 KM2


# Moderate

Crax_IDC_area <- fut_cover_IDC
plot(Crax_IDC_area)
occurrence_area = x * Crax_IDC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #330,908.7 KM2

# Pessimistic
Crax_NoFC_area <- fut_cover_NoFC
plot(Crax_NoFC_area)
occurrence_area = x * Crax_NoFC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #337,623.8 KM2

# Optimistic
Crax_FC_area <- fut_cover_FC
plot(Crax_FC_area)
occurrence_area = x * Crax_FC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #363,853.5 KM2


# 4) Amazon --------------------------------------

# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")


# Current ----------------------------------------------------------------

adeq_clim_cur <- raster("CUR.cont_Crax_fasciolata.tif") 
plot(adeq_clim_cur)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil")
cur_cover <- raster("Cover2000_nativa.tif")
plot(cur_cover)

setMinMax(cur_cover) 
cur_cover <- crop(cur_cover, extent(adeq_clim_cur))
setMinMax(cur_cover)
cur_cover <- mask (cur_cover, adeq_clim_cur)/320
plot(cur_cover)

raster(cur_cover)
raster(adeq_clim_cur)


# Future (Moderate) ----------------------------------------------------
fut_cover_IDC <- raster("./CoverIDC2050_nativa.tif")
fut_cover_IDC <- crop(fut_cover_IDC, extent(adeq_clim_cur))
fut_cover_IDC <- mask (fut_cover_IDC, adeq_clim_cur)/320
plot(fut_cover_IDC)

# Future (Pessimistic) ----------------------------------------------------
fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
fut_cover_NoFC <- crop(fut_cover_NoFC, extent(adeq_clim_cur))
fut_cover_NoFC <- mask (fut_cover_NoFC, adeq_clim_cur)/320
plot(fut_cover_NoFC)

# Future (FC) --------------------------------------------------------
fut_cover_FC <- raster("./CoverFC2050_nativa.tif")
fut_cover_FC <- crop(fut_cover_FC, extent(adeq_clim_cur))
fut_cover_FC <- mask (fut_cover_FC, adeq_clim_cur)/320
plot(fut_cover_FC)


# Moderate -------------------------------------------------------------
valid.pixel <- !is.na(cur_cover[])
valid.pixel2 <- !is.na(fut_cover_IDC[])
forest.change.IDC <- (sum(fut_cover_IDC[valid.pixel2])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.IDC #-11.60243

# Pessimistic  -------------------------------------------------------------
valid.pixel3 <- !is.na(fut_cover_NoFC[])
forest.change.NoFC <- (sum(fut_cover_NoFC[valid.pixel3])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.NoFC #-17.22636

# Optimistic ---------------------------------------------------------
valid.pixel4 <- !is.na(fut_cover_FC[])
forest.change.FC <- (sum(fut_cover_FC[valid.pixel4])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.FC #-7.334877


# Saving maps ---------------------------------------------------------

getwd()
setwd("./Maps")
dir.create("Amazonia")
setwd("Amazonia")

writeRaster(cur_cover, "forest.current_Crax_fasciolata.tif")
writeRaster(fut_cover_IDC, "forest.future.IDCImperfect3_Crax_fasciolata.tif")
writeRaster(fut_cover_NoFC, "forest.future.NoFC_Crax_fasciolata.tif")
writeRaster(fut_cover_FC, "forest.future.FC_Crax_fasciolata.tif")

# Current
Crax_cur_area <- cur_cover
plot(Crax_cur_area)
r = raster(nrow=479, ncol=733, xmn=-73.54167, xmx=-43, ymn=-16.29167, ymx=3.666667) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #2,025,713 KM2


# Moderate

Crax_IDC_area <- fut_cover_IDC
plot(Crax_IDC_area)
occurrence_area = x * Crax_IDC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #1,791,698 KM2


# Pessimistic
Crax_NoFC_area <- fut_cover_NoFC
plot(Crax_NoFC_area)
occurrence_area = x * Crax_NoFC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #1,678,408 KM2

# Optimistic
Crax_FC_area <- fut_cover_FC
plot(Crax_FC_area)
occurrence_area = x * Crax_FC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #1,877,922 KM2

###############################################################

# 5) Caatinga --------------------------------------

# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")


# Current ----------------------------------------------------------------

adeq_clim_cur <- raster("CUR.cont_Crax_fasciolata.tif") 
plot(adeq_clim_cur)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil")
cur_cover <- raster("Cover2000_nativa.tif")
plot(cur_cover)

setMinMax(cur_cover) cur_cover <- crop(cur_cover, extent(adeq_clim_cur))
cur_cover <- mask (cur_cover, adeq_clim_cur)/320
cur_cover 
plot(cur_cover)

raster(cur_cover)
raster(adeq_clim_cur)


# Future (Moderate) ----------------------------------------------------
fut_cover_IDC <- raster("./CoverIDC2050_nativa.tif")
fut_cover_IDC <- crop(fut_cover_IDC, extent(adeq_clim_cur))
fut_cover_IDC <- mask(fut_cover_IDC, adeq_clim_cur)/320
plot(fut_cover_IDC) # max ~71%

# Future (Pessimistic) ----------------------------------------------------
fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
fut_cover_NoFC <- crop(fut_cover_NoFC, extent(adeq_clim_cur))
fut_cover_NoFC <- mask (fut_cover_NoFC, adeq_clim_cur)/320
plot(fut_cover_NoFC) # max ~71%

# Future (FC) --------------------------------------------------------
fut_cover_FC <- raster("./CoverFC2050_nativa.tif")
fut_cover_FC <- crop(fut_cover_FC, extent(adeq_clim_cur))
fut_cover_FC <- mask (fut_cover_FC, adeq_clim_cur)/320
plot(fut_cover_FC) # max ~71%


# Moderate -------------------------------------------------------------
valid.pixel <- !is.na(cur_cover[])
which(valid.pixel)
valid.pixel2 <- !is.na(fut_cover_IDC[])
which(valid.pixel2)
forest.change.IDC <- (sum(fut_cover_IDC[valid.pixel2])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.IDC #-12.06

# Pessimistic -------------------------------------------------------------
valid.pixel3 <- !is.na(fut_cover_NoFC[])
forest.change.NoFC <- (sum(fut_cover_NoFC[valid.pixel3])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.NoFC #-8.81


# Optimistic ---------------------------------------------------------
valid.pixel4 <- !is.na(fut_cover_FC[])
forest.change.FC <- (sum(fut_cover_FC[valid.pixel4])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.FC #-16.14


# Saving maps ---------------------------------------------------------

getwd()
setwd("./Maps")
dir.create("Caatinga")
setwd("Caatinga")

writeRaster(cur_cover, "forest.current_Crax_fasciolata.tif")
writeRaster(fut_cover_IDC, "forest.future.IDCImperfect3_Crax_fasciolata.tif")
writeRaster(fut_cover_NoFC, "forest.future.NoFC_Crax_fasciolata.tif")
writeRaster(fut_cover_FC, "forest.future.FC_Crax_fasciolata.tif")

# Current
Crax_cur_area <- cur_cover
plot(Crax_cur_area)
r = raster(nrow=319, ncol=135, xmn=-44.5, xmx=-38.875, ymn=-16.08333, ymx=-2.791667) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #122,872.2 KM2

# Moderate

Crax_IDC_area <- fut_cover_IDC
plot(Crax_IDC_area)
occurrence_area = x * Crax_IDC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #108,050.1 KM2

# Pessimistic
Crax_NoFC_area <- fut_cover_NoFC
plot(Crax_NoFC_area)
occurrence_area = x * Crax_NoFC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #112,036.2 KM2

# Optimistic
Crax_FC_area <- fut_cover_FC
plot(Crax_FC_area)
occurrence_area = x * Crax_FC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #103,030.3 KM2

###############################################################

# 6) Pampa --------------------------------------

# Setting directory ----------------------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")


# Current ----------------------------------------------------------------

adeq_clim_cur <- raster("CUR.cont_Crax_fasciolata.tif") 
plot(adeq_clim_cur)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil")
cur_cover <- raster("Cover2000_nativa.tif")
plot(cur_cover)

setMinMax(cur_cover) 
cur_cover <- crop(cur_cover, extent(adeq_clim_cur))
cur_cover <- mask (cur_cover, adeq_clim_cur)/320
cur_cover 
plot(cur_cover)

raster(cur_cover)
raster(adeq_clim_cur)


# Future (Moderate) ----------------------------------------------------
fut_cover_IDC <- raster("./CoverIDC2050_nativa.tif")
fut_cover_IDC <- crop(fut_cover_IDC, extent(adeq_clim_cur))
fut_cover_IDC <- mask(fut_cover_IDC, adeq_clim_cur)/320
plot(fut_cover_IDC) # max ~71%

# Future (Pessimistic) ----------------------------------------------------
fut_cover_NoFC <- raster("./CoverNoFC2050_nativa.tif")
fut_cover_NoFC <- crop(fut_cover_NoFC, extent(adeq_clim_cur))
fut_cover_NoFC <- mask (fut_cover_NoFC, adeq_clim_cur)/320
plot(fut_cover_NoFC) # max ~71%

# Future (FC) --------------------------------------------------------
fut_cover_FC <- raster("./CoverFC2050_nativa.tif")
fut_cover_FC <- crop(fut_cover_FC, extent(adeq_clim_cur))
fut_cover_FC <- mask (fut_cover_FC, adeq_clim_cur)/320
plot(fut_cover_FC) # max ~71%


# Moderate -------------------------------------------------------------
valid.pixel <- !is.na(cur_cover[])
which(valid.pixel)
valid.pixel2 <- !is.na(fut_cover_IDC[])
which(valid.pixel2)
forest.change.IDC <- (sum(fut_cover_IDC[valid.pixel2])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.IDC #-6.7

# Pessimistic -------------------------------------------------------------
valid.pixel3 <- !is.na(fut_cover_NoFC[])
forest.change.NoFC <- (sum(fut_cover_NoFC[valid.pixel3])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.NoFC #-8.81


# Optimistic ---------------------------------------------------------
valid.pixel4 <- !is.na(fut_cover_FC[])
forest.change.FC <- (sum(fut_cover_FC[valid.pixel4])-sum(cur_cover[valid.pixel]))/sum(cur_cover[valid.pixel])*100
forest.change.FC #-16.14


# Saving maps ---------------------------------------------------------

getwd()
setwd("./Maps")
dir.create("Pampa")
setwd("Pampa")

writeRaster(cur_cover, "forest.current_Crax_fasciolata.tif")
writeRaster(fut_cover_IDC, "forest.future.IDCImperfect3_Crax_fasciolata.tif")
writeRaster(fut_cover_NoFC, "forest.future.NoFC_Crax_fasciolata.tif")
writeRaster(fut_cover_FC, "forest.future.FC_Crax_fasciolata.tif")

# Current
Crax_cur_area <- cur_cover
plot(Crax_cur_area)
r = raster(nrow=136, ncol=192, xmn=-57.66667, xmx=-49.66667, ymn=-33.75, ymx=-28.08333) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #4,517.506 KM2

# Moderate

Crax_IDC_area <- fut_cover_IDC
plot(Crax_IDC_area)
occurrence_area = x * Crax_IDC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #4213.023 KM2

# Pessimistic
Crax_NoFC_area <- fut_cover_NoFC
plot(Crax_NoFC_area)
occurrence_area = x * Crax_NoFC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #4213.023 KM2

# Optimistic
Crax_FC_area <- fut_cover_FC
plot(Crax_FC_area)
occurrence_area = x * Crax_FC_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #4213.124 KM2

###############################################################