# -------------------------------------------------------------
# Environmental suitability for Brazil and Brazilian domains
# 14 jan 2021
# Ana Claudia de Almeida
# -------------------------------------------------------------
#

# FC: Forest Code (Optimistic scenario)
# IDCImperfect3: 50% of deforestation control (Moderate scenario)
# NoFC: No Forest Code / Business as usual (Pessimistic scenario)
# 370: moderate climatic scenario
# 585: pessimistic climatic scenario



# Loading packages --------------------------------------
library(raster)
library(rgdal)


# Setting directory --------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil/Maps")


# Loading habitat rasters --------------------------------------
Cover_2000 <- raster("./forest.current_Crax_fasciolata.tif")
FC_2050 <- raster("./forest.future.FC_Crax_fasciolata.tif")
IDC3_2050 <- raster ("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
NoFC_2050 <- raster ("./forest.future.NoFC_Crax_fasciolata.tif")

# plot(Cover_2000)
# plot(FC_2050)
# plot(IDC3_2050)
# plot(NoFC_2050)


# Loading climatic rasters --------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

suit_clim_cur <- raster ("./CUR.cont_Crax_fasciolata.tif")
suit_clim_fut_370 <- raster ("./Future_370_2050.cont_Crax_fasciolata.tif")
suit_clim_fut_585 <- raster ("./Future_585_2050.cont_Crax_fasciolata.tif")

plot(suit_clim_cur)


# Multiplying habitat maps by climatic maps --------------------------------------
# Current --------------------------------------
suit_env_cur <- suit_clim_cur * Cover_2000

# Future --------------------------------------
# IDC X 370
suit_env_IDC3_370 <- suit_clim_fut_370 * IDC3_2050

# FC X 370
suit_env_FC_370 <- suit_clim_fut_370 * FC_2050

# NoFC X 370
suit_env_NoFC_370 <- suit_clim_fut_370 * NoFC_2050


# IDC X 585
suit_env_IDC3_585 <- suit_clim_fut_585 * IDC3_2050

# FC X 585
suit_env_FC_585 <- suit_clim_fut_585 * FC_2050

# NoFC X 585
suit_env_NoFC_585 <- suit_clim_fut_585 * NoFC_2050

plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(suit_env_FC_370)
plot(suit_env_NoFC_370)
plot(suit_env_IDC3_585)
plot(suit_env_FC_585)
plot(suit_env_NoFC_585)


# % changes --------------------------------------
# Current X Future 370 
#IDC
valid.pixels <- !is.na(suit_env_cur[])
valid.pixels1 <- !is.na(suit_env_IDC3_370[])
change.suit.env.IDC3.370 <- (sum(suit_env_IDC3_370[valid.pixels1])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.370 #3.060401

#FC
valid.pixels2 <- !is.na(suit_env_FC_370[])
change.suit.env.FC.370 <- (sum(suit_env_FC_370[valid.pixels2])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.370 #8.099331

#NoFC
valid.pixels3 <- !is.na(suit_env_NoFC_370[])
change.suit.env.NoFC.370 <- (sum(suit_env_NoFC_370[valid.pixels3])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.370 #-1.761603


# Current X Future 585 --------------------------------------
#IDC3
valid.pixels4 <- !is.na(suit_env_IDC3_585[])
change.suit.env.IDC3.585 <- (sum(suit_env_IDC3_585[valid.pixels4])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.585 #5.168571

#FC
valid.pixels5 <- !is.na(suit_env_FC_585[])
change.suit.env.FC.585 <- (sum(suit_env_FC_585[valid.pixels5])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.585 #10.2497

#NoFC
valid.pixels6 <- !is.na(suit_env_NoFC_585[])
change.suit.env.NoFC.585 <- (sum(suit_env_NoFC_585[valid.pixels6])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.585 #0.09860408


# Saving maps --------------------------------------
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

dir.create("Adequabilidade_ambiental")
target_dir = paste( './Adequabilidade_ambiental', '/', sep="" )

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', '...', '\n')
writeRaster(suit_env_cur, file = paste(target_dir, '/suit.current_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_370, file = paste(target_dir, '/suit.IDC3_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_370, file = paste(target_dir, '/suit.FC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_370, file = paste(target_dir, '/suit.NoFC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_585, file = paste(target_dir, '/suit.IDC3_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_585, file = paste(target_dir, '/suit.FC_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_585, file = paste(target_dir, '/suit.NoFC_585_Crax_fasciolata', '.tif', sep=""))

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

#Current
suit_env_cur[suit_env_cur == 0] <- NA
plot(suit_env_cur)
r = raster(nrow=898, ncol=832, xmn=-73.54167, xmx=-38.875, ymn=-33.75, ymx=3.666667) 
x = raster::area(r)
occurrence_area = x * suit_env_cur
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #567,851.2 KM2

# IDC X 370
suit_env_IDC3_370[suit_env_IDC3_370 == 0] <- NA
plot(suit_env_IDC3_370)
occurrence_area = x * suit_env_IDC3_370
occurrence_area_calc.IDC3_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_370 #587,306.1

# FC X 370
suit_env_FC_370[suit_env_FC_370 == 0] <- NA
plot(suit_env_FC_370)
occurrence_area <- x * suit_env_FC_370
occurrence_area_calc.FC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_370 #616,163.2

# NoFC X 370
suit_env_NoFC_370[suit_env_NoFC_370 == 0] <- NA
plot(suit_env_NoFC_370)
occurrence_area = x * suit_env_NoFC_370
occurrence_area_calc.NoFC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_370 #559,588.4

# IDC X 585
suit_env_IDC3_585[suit_env_IDC3_585 == 0] <- NA
plot(suit_env_IDC3_585)
occurrence_area = x * suit_env_IDC3_585
occurrence_area_calc.IDC3_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_585 #559,567.8

# FC X 585
suit_env_FC_585[suit_env_FC_585 == 0] <- NA
plot(suit_env_FC_585)
occurrence_area = x * suit_env_FC_585
occurrence_area_calc.FC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_585 #628,666.6

# NoFC X 585
suit_env_NoFC_585[suit_env_NoFC_585 == 0] <- NA
plot(suit_env_NoFC_585)
occurrence_area = x * suit_env_NoFC_585
occurrence_area_calc.NoFC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_585 #570,432.6


# change

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

changeIDC3_370 <- suit_env_IDC3_370 - suit_env_cur
plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(changeIDC3_370)

changeFC_370 <- suit_env_FC_370 - suit_env_cur
plot(suit_env_cur)
plot(suit_env_FC_370)
plot(changeFC_370)

changeNoFC_370 <- suit_env_NoFC_370 - suit_env_cur
plot(suit_env_cur)
plot(suit_env_NoFC_370)
plot(changeNoFC_370)

writeRaster(changeIDC, "changeIDC.tif")
writeRaster(changeFC, "changeFC.tif")
writeRaster(changeNoFC, "changeNoFC.tif")

###############################################################


# BRAZILIAN DOMAINS --------------------------------------

# 1) Atlantic Forest

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil/Maps/AF")


Cover_2000 <- raster("./forest.current_Crax_fasciolata.tif")
FC_2050 <- raster("./forest.future.FC_Crax_fasciolata.tif")
IDC3_2050 <- raster ("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
NoFC_2050 <- raster ("./forest.future.NoFC_Crax_fasciolata.tif")

# plot(Cover_2000)
# plot(FC_2050)
# plot(IDC3_2050)
# plot(NoFC_2050)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

suit_clim_cur <- raster ("./CUR.cont_Crax_fasciolata.tif")
suit_clim_fut_370 <- raster ("./Future_370_2050.cont_Crax_fasciolata.tif")
suit_clim_fut_585 <- raster ("./Future_585_2050.cont_Crax_fasciolata.tif")

# plot(suit_clim_cur)

# Current --------------------------------------
suit_env_cur <- suit_clim_cur * Cover_2000

# Future 370 --------------------------------------
# IDC3
suit_env_IDC3_370 <- suit_clim_fut_370 * IDC3_2050

# FC
suit_env_FC_370 <- suit_clim_fut_370 * FC_2050

# NoFC
suit_env_NoFC_370 <- suit_clim_fut_370 * NoFC_2050

# Future 585 --------------------------------------
# IDC3
suit_env_IDC3_585 <- suit_clim_fut_585 * IDC3_2050

# FC
suit_env_FC_585 <- suit_clim_fut_585 * FC_2050

# NoFC
suit_env_NoFC_585 <- suit_clim_fut_585 * NoFC_2050

plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(suit_env_FC_370)
plot(suit_env_NoFC_370)
plot(suit_env_IDC3_585)
plot(suit_env_FC_585)
plot(suit_env_NoFC_585)


# Current X Future 370 --------------------------------------
#IDC3
valid.pixels <- !is.na(suit_env_cur[])
valid.pixels1 <- !is.na(suit_env_IDC3_370[])
change.suit.env.IDC3.370 <- (sum(suit_env_IDC3_370[valid.pixels1])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.370 # 28.8693

#FC
valid.pixels2 <- !is.na(suit_env_FC_370[])
change.suit.env.FC.370 <- (sum(suit_env_FC_370[valid.pixels2])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.370 #28.92643

#NoFC
valid.pixels3 <- !is.na(suit_env_NoFC_370[])
change.suit.env.NoFC.370 <- (sum(suit_env_NoFC_370[valid.pixels3])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.370 #28.88585

# Current X Future 585 --------------------------------------
#IDC3
valid.pixels4 <- !is.na(suit_env_IDC3_585[])
change.suit.env.IDC3.585 <- (sum(suit_env_IDC3_585[valid.pixels4])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.585 #27.92906

#FC
valid.pixels5 <- !is.na(suit_env_FC_585[])
change.suit.env.FC.585 <- (sum(suit_env_FC_585[valid.pixels5])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.585 #27.98638

#NoFC
valid.pixels6 <- !is.na(suit_env_NoFC_585[])
change.suit.env.NoFC.585 <- (sum(suit_env_NoFC_585[valid.pixels6])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.585 #27.94301

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")
dir.create("AF")

target_dir = paste( './AF', '/', sep="" )

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', '...', '\n')
writeRaster(suit_env_cur, file = paste(target_dir, '/suit.current_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_370, file = paste(target_dir, '/suit.IDC3_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_370, file = paste(target_dir, '/suit.FC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_370, file = paste(target_dir, '/suit.NoFC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_585, file = paste(target_dir, '/suit.IDC3_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_585, file = paste(target_dir, '/suit.FC_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_585, file = paste(target_dir, '/suit.NoFC_585_Crax_fasciolata', '.tif', sep=""))

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental/AF")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

#Current
suit_env_cur[suit_env_cur == 0] <- NA
plot(suit_env_cur)
r = raster(nrow=596, ncol=403, xmn=-55.66667, xmx=-38.875, ymn=-29.95833, ymx=-5.125) 
x = raster::area(r)
occurrence_area = x * suit_env_cur
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #40,775.02 KM2

# IDC X 370
suit_env_IDC3_370[suit_env_IDC3_370 == 0] <- NA
plot(suit_env_IDC3_370)
occurrence_area = x * suit_env_IDC3_370
occurrence_area_calc.IDC3_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_370 #52,532.89

# FC X 370
suit_env_FC_370[suit_env_FC_370 == 0] <- NA
plot(suit_env_FC_370)
occurrence_area = x * suit_env_FC_370
occurrence_area_calc.FC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_370 #52,556.53

# NoFC X 370
suit_env_NoFC_370[suit_env_NoFC_370 == 0] <- NA
plot(suit_env_NoFC_370)
occurrence_area = x * suit_env_NoFC_370
occurrence_area_calc.NoFC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_370 #52,539.89


# IDC X 585
suit_env_IDC3_585[suit_env_IDC3_585 == 0] <- NA
plot(suit_env_IDC3_585)
occurrence_area = x * suit_env_IDC3_585
occurrence_area_calc.IDC3_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_585 #52,154.06

# FC X 585
suit_env_FC_585[suit_env_FC_585 == 0] <- NA
plot(suit_env_FC_585)
occurrence_area = x * suit_env_FC_585
occurrence_area_calc.FC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_585 #52,177.78

# NoFC X 585
suit_env_NoFC_585[suit_env_NoFC_585 == 0] <- NA
plot(suit_env_NoFC_585)
occurrence_area = x * suit_env_NoFC_585
occurrence_area_calc.NoFC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_585 #52,159.95


# 2) Pantanal

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil/Maps/Pantanal")

Cover_2000 <- raster("./forest.current_Crax_fasciolata.tif")
FC_2050 <- raster("./forest.future.FC_Crax_fasciolata.tif")
IDC3_2050 <- raster ("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
NoFC_2050 <- raster ("./forest.future.NoFC_Crax_fasciolata.tif")

# plot(Cover_2000)
# plot(FC_2050)
# plot(IDC3_2050)
# plot(NoFC_2050)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

suit_clim_cur <- raster ("./CUR.cont_Crax_fasciolata.tif")
suit_clim_fut_370 <- raster ("./Future_370_2050.cont_Crax_fasciolata.tif")
suit_clim_fut_585 <- raster ("./Future_585_2050.cont_Crax_fasciolata.tif")

plot(suit_clim_cur)

# Current --------------------------------------
suit_env_cur <- suit_clim_cur * Cover_2000

# Future 370 --------------------------------------
# IDC3
suit_env_IDC3_370 <- suit_clim_fut_370 * IDC3_2050

# FC
suit_env_FC_370 <- suit_clim_fut_370 * FC_2050

# NoFC
suit_env_NoFC_370 <- suit_clim_fut_370 * NoFC_2050

# Future 585 --------------------------------------
# IDC3
suit_env_IDC3_585 <- suit_clim_fut_585 * IDC3_2050

# FC
suit_env_FC_585 <- suit_clim_fut_585 * FC_2050

# NoFC
suit_env_NoFC_585 <- suit_clim_fut_585 * NoFC_2050

plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(suit_env_FC_370)
plot(suit_env_NoFC_370)
plot(suit_env_IDC3_585)
plot(suit_env_FC_585)
plot(suit_env_NoFC_585)


# Current X Future 370 --------------------------------------
#IDC3
valid.pixels <- !is.na(suit_env_cur[])
valid.pixels1 <- !is.na(suit_env_IDC3_370[])
change.suit.env.IDC3.370 <- (sum(suit_env_IDC3_370[valid.pixels1])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.370 # -58.35761

#FC
valid.pixels2 <- !is.na(suit_env_FC_370[])
change.suit.env.FC.370 <- (sum(suit_env_FC_370[valid.pixels2])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.370 #-63.26154

#NoFC
valid.pixels3 <- !is.na(suit_env_NoFC_370[])
change.suit.env.NoFC.370 <- (sum(suit_env_NoFC_370[valid.pixels3])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.370 #-56.30367                                    


# Current X Future 585 --------------------------------------
#IDC3
valid.pixels4 <- !is.na(suit_env_IDC3_585[])
change.suit.env.IDC3.585 <- (sum(suit_env_IDC3_585[valid.pixels4])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.585 #-59.697

#FC
valid.pixels5 <- !is.na(suit_env_FC_585[])
change.suit.env.FC.585 <- (sum(suit_env_FC_585[valid.pixels5])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.585 #-64.54664

#NoFC
valid.pixels6 <- !is.na(suit_env_NoFC_585[])
change.suit.env.NoFC.585 <- (sum(suit_env_NoFC_585[valid.pixels6])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.585 #-57.68249


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")
dir.create("Pantanal")

target_dir = paste( './Pantanal', '/', sep="" )

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', '...', '\n')
writeRaster(suit_env_cur, file = paste(target_dir, '/suit.current_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_370, file = paste(target_dir, '/suit.IDC3_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_370, file = paste(target_dir, '/suit.FC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_370, file = paste(target_dir, '/suit.NoFC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_585, file = paste(target_dir, '/suit.IDC3_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_585, file = paste(target_dir, '/suit.FC_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_585, file = paste(target_dir, '/suit.NoFC_585_Crax_fasciolata', '.tif', sep=""))


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental/Pantanal")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

#Current
suit_env_cur[suit_env_cur == 0] <- NA
plot(suit_env_cur)
r = raster(nrow=159, ncol=102, xmn=-59.16667, xmx=-54.91667, ymn=-22.16667, ymx=-15.54167) 
x = raster::area(r)
occurrence_area = x * suit_env_cur
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #32,614.01 KM2

# IDC X 370
suit_env_IDC3_370[suit_env_IDC3_370 == 0] <- NA
plot(suit_env_IDC3_370)
occurrence_area = x * suit_env_IDC3_370
occurrence_area_calc.IDC3_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_370 #13,554.22

# FC X 370
suit_env_FC_370[suit_env_FC_370 == 0] <- NA
plot(suit_env_FC_370)
occurrence_area = x * suit_env_FC_370
occurrence_area_calc.FC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_370 #11,965.66

# NoFC X 370
suit_env_NoFC_370[suit_env_NoFC_370 == 0] <- NA
plot(suit_env_NoFC_370)
occurrence_area = x * suit_env_NoFC_370
occurrence_area_calc.NoFC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_370 #14,213.06

# IDC X 585
suit_env_IDC3_585[suit_env_IDC3_585 == 0] <- NA
plot(suit_env_IDC3_585)
occurrence_area = x * suit_env_IDC3_585
occurrence_area_calc.IDC3_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_585 #13,118.37

# FC X 585
suit_env_FC_585[suit_env_FC_585 == 0] <- NA
plot(suit_env_FC_585)
occurrence_area = x * suit_env_FC_585
occurrence_area_calc.FC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_585 #11,546.7

# NoFC X 585
suit_env_NoFC_585[suit_env_NoFC_585 == 0] <- NA
plot(suit_env_NoFC_585)
occurrence_area = x * suit_env_NoFC_585
occurrence_area_calc.NoFC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_585 #13,764.66



# 3) Cerrado

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil/Maps/Cerrado")


Cover_2000 <- raster("./forest.current_Crax_fasciolata.tif")
FC_2050 <- raster("./forest.future.FC_Crax_fasciolata.tif")
IDC3_2050 <- raster ("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
NoFC_2050 <- raster ("./forest.future.NoFC_Crax_fasciolata.tif")

# plot(Cover_2000)
# plot(FC_2050)
# plot(IDC3_2050)
# plot(NoFC_2050)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

suit_clim_cur <- raster ("./CUR.cont_Crax_fasciolata.tif")
suit_clim_fut_370 <- raster ("./Future_370_2050.cont_Crax_fasciolata.tif")
suit_clim_fut_585 <- raster ("./Future_585_2050.cont_Crax_fasciolata.tif")

#plot(suit_clim_cur)

# Current --------------------------------------
suit_env_cur <- suit_clim_cur * Cover_2000

# Future 370 --------------------------------------
# IDC3
suit_env_IDC3_370 <- suit_clim_fut_370 * IDC3_2050

# FC
suit_env_FC_370 <- suit_clim_fut_370 * FC_2050

# NoFC
suit_env_NoFC_370 <- suit_clim_fut_370 * NoFC_2050

# Future 585 --------------------------------------
# IDC3
suit_env_IDC3_585 <- suit_clim_fut_585 * IDC3_2050

# FC
suit_env_FC_585 <- suit_clim_fut_585 * FC_2050

# NoFC
suit_env_NoFC_585 <- suit_clim_fut_585 * NoFC_2050

plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(suit_env_FC_370)
plot(suit_env_NoFC_370)
plot(suit_env_IDC3_585)
plot(suit_env_FC_585)
plot(suit_env_NoFC_585)


# Current X Future 370 --------------------------------------
#IDC3
valid.pixels <- !is.na(suit_env_cur[])
valid.pixels1 <- !is.na(suit_env_IDC3_370[])
change.suit.env.IDC3.370 <- (sum(suit_env_IDC3_370[valid.pixels1])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.370 # -35.4965

#FC
valid.pixels2 <- !is.na(suit_env_FC_370[])
change.suit.env.FC.370 <- (sum(suit_env_FC_370[valid.pixels2])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.370 #-28.82112

#NoFC
valid.pixels3 <- !is.na(suit_env_NoFC_370[])
change.suit.env.NoFC.370 <- (sum(suit_env_NoFC_370[valid.pixels3])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.370 #-34.69185


# Current X Future 585 --------------------------------------
#IDC3
valid.pixels4 <- !is.na(suit_env_IDC3_585[])
change.suit.env.IDC3.585 <- (sum(suit_env_IDC3_585[valid.pixels4])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.585 #-35.45712

#FC
valid.pixels5 <- !is.na(suit_env_FC_585[])
change.suit.env.FC.585 <- (sum(suit_env_FC_585[valid.pixels5])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.585 #-28.77973

#NoFC
valid.pixels6 <- !is.na(suit_env_NoFC_585[])
change.suit.env.NoFC.585 <- (sum(suit_env_NoFC_585[valid.pixels6])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.585 #-34.67464


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")
dir.create("Cerrado")

target_dir = paste( './Cerrado', '/', sep="" )

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', '...', '\n')
writeRaster(suit_env_cur, file = paste(target_dir, '/suit.current_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_370, file = paste(target_dir, '/suit.IDC3_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_370, file = paste(target_dir, '/suit.FC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_370, file = paste(target_dir, '/suit.NoFC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_585, file = paste(target_dir, '/suit.IDC3_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_585, file = paste(target_dir, '/suit.FC_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_585, file = paste(target_dir, '/suit.NoFC_585_Crax_fasciolata', '.tif', sep=""))


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental/Cerrado")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

#Current
suit_env_cur[suit_env_cur == 0] <- NA
plot(suit_env_cur)
r = raster(nrow=536, ncol=446, xmn=-60.125, xmx=-41.54167, ymn=-24.66667, ymx=-2.333333) 
x = raster::area(r)
occurrence_area = x * suit_env_cur
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #129,547.5 KM2

# IDC X 370
suit_env_IDC3_370[suit_env_IDC3_370 == 0] <- NA
plot(suit_env_IDC3_370)
occurrence_area = x * suit_env_IDC3_370
occurrence_area_calc.IDC3_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_370 #83,576.88

# FC X 370
suit_env_FC_370[suit_env_FC_370 == 0] <- NA
plot(suit_env_FC_370)
occurrence_area = x * suit_env_FC_370
occurrence_area_calc.FC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_370 #92,250.29

# NoFC X 370
suit_env_NoFC_370[suit_env_NoFC_370 == 0] <- NA
plot(suit_env_NoFC_370)
occurrence_area = x * suit_env_NoFC_370
occurrence_area_calc.NoFC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_370 #84,612.9

# IDC X 585
suit_env_IDC3_585[suit_env_IDC3_585 == 0] <- NA
plot(suit_env_IDC3_585)
occurrence_area = x * suit_env_IDC3_585
occurrence_area_calc.IDC3_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_585 #83,642.84

# FC X 585
suit_env_FC_585[suit_env_FC_585 == 0] <- NA
plot(suit_env_FC_585)
occurrence_area = x * suit_env_FC_585
occurrence_area_calc.FC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_585 #92,322.59

# NoFC X 585
suit_env_NoFC_585[suit_env_NoFC_585 == 0] <- NA
plot(suit_env_NoFC_585)
occurrence_area = x * suit_env_NoFC_585
occurrence_area_calc.NoFC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_585 #84,649.73


# 4) Amazon

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil/Maps/Amazonia")


Cover_2000 <- raster("./forest.current_Crax_fasciolata.tif")
FC_2050 <- raster("./forest.future.FC_Crax_fasciolata.tif")
IDC3_2050 <- raster ("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
NoFC_2050 <- raster ("./forest.future.NoFC_Crax_fasciolata.tif")

# plot(Cover_2000)
# plot(FC_2050)
# plot(IDC3_2050)
# plot(NoFC_2050)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

suit_clim_cur <- raster ("./CUR.cont_Crax_fasciolata.tif")
suit_clim_fut_370 <- raster ("./Future_370_2050.cont_Crax_fasciolata.tif")
suit_clim_fut_585 <- raster ("./Future_585_2050.cont_Crax_fasciolata.tif")

# plot(suit_clim_cur)

# Current --------------------------------------
suit_env_cur <- suit_clim_cur * Cover_2000

# Future 370 --------------------------------------
# IDC3
suit_env_IDC3_370 <- suit_clim_fut_370 * IDC3_2050

# FC
suit_env_FC_370 <- suit_clim_fut_370 * FC_2050

# NoFC
suit_env_NoFC_370 <- suit_clim_fut_370 * NoFC_2050

# Future 585 --------------------------------------
# IDC3
suit_env_IDC3_585 <- suit_clim_fut_585 * IDC3_2050

# FC
suit_env_FC_585 <- suit_clim_fut_585 * FC_2050

# NoFC
suit_env_NoFC_585 <- suit_clim_fut_585 * NoFC_2050

plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(suit_env_FC_370)
plot(suit_env_NoFC_370)
plot(suit_env_IDC3_585)
plot(suit_env_FC_585)
plot(suit_env_NoFC_585)


# Current X Future 370 --------------------------------------
#IDC3
valid.pixels <- !is.na(suit_env_cur[])
valid.pixels1 <- !is.na(suit_env_IDC3_370[])
change.suit.env.IDC3.370 <- (sum(suit_env_IDC3_370[valid.pixels1])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.370 # 20.28718

#FC
valid.pixels2 <- !is.na(suit_env_FC_370[])
change.suit.env.FC.370 <- (sum(suit_env_FC_370[valid.pixels2])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.370 #26.58239

#NoFC
valid.pixels3 <- !is.na(suit_env_NoFC_370[])
change.suit.env.NoFC.370 <- (sum(suit_env_NoFC_370[valid.pixels3])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.370 #11.82317


# Current X Future 585 --------------------------------------
#IDC3
valid.pixels4 <- !is.na(suit_env_IDC3_585[])
change.suit.env.IDC3.585 <- (sum(suit_env_IDC3_585[valid.pixels4])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.585 #24.00484

#FC
valid.pixels5 <- !is.na(suit_env_FC_585[])
change.suit.env.FC.585 <- (sum(suit_env_FC_585[valid.pixels5])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.585 # 30.36273

#NoFC
valid.pixels6 <- !is.na(suit_env_NoFC_585[])
change.suit.env.NoFC.585 <- (sum(suit_env_NoFC_585[valid.pixels6])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.585 #15.14683

####Para salvar os mapas na pasta####
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")
dir.create("Amazonia")

target_dir = paste( './Amazonia', '/', sep="" )

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', '...', '\n')
writeRaster(suit_env_cur, file = paste(target_dir, '/suit.current_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_370, file = paste(target_dir, '/suit.IDC3_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_370, file = paste(target_dir, '/suit.FC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_370, file = paste(target_dir, '/suit.NoFC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_585, file = paste(target_dir, '/suit.IDC3_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_585, file = paste(target_dir, '/suit.FC_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_585, file = paste(target_dir, '/suit.NoFC_585_Crax_fasciolata', '.tif', sep=""))


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental/Amazonia")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

#Current
suit_env_cur[suit_env_cur == 0] <- NA
plot(suit_env_cur)
r = raster(nrow=479, ncol=733, xmn=-73.54167, xmx=-43, ymn=-16.29167, ymx=3.666667) 
x = raster::area(r)
occurrence_area = x * suit_env_cur
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #354,039.6 KM2

# IDC X 370
suit_env_IDC3_370[suit_env_IDC3_370 == 0] <- NA
plot(suit_env_IDC3_370)
occurrence_area = x * suit_env_IDC3_370
occurrence_area_calc.IDC3_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_370 #426,752.1

# FC X 370
suit_env_FC_370[suit_env_FC_370 == 0] <- NA
plot(suit_env_FC_370)
occurrence_area = x * suit_env_FC_370
occurrence_area_calc.FC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_370 #449,018.5

# NoFC X 370
suit_env_NoFC_370[suit_env_NoFC_370 == 0] <- NA
plot(suit_env_NoFC_370)
occurrence_area = x * suit_env_NoFC_370
occurrence_area_calc.NoFC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_370 #396,874.8

# IDC X 585
suit_env_IDC3_585[suit_env_IDC3_585 == 0] <- NA
plot(suit_env_IDC3_585)
occurrence_area = x * suit_env_IDC3_585
occurrence_area_calc.IDC3_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_585 #439,963.1

# FC X 585
suit_env_FC_585[suit_env_FC_585 == 0] <- NA
plot(suit_env_FC_585)
occurrence_area = x * suit_env_FC_585
occurrence_area_calc.FC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_585 #462,448.1

# NoFC X 585
suit_env_NoFC_585[suit_env_NoFC_585 == 0] <- NA
plot(suit_env_NoFC_585)
occurrence_area = x * suit_env_NoFC_585
occurrence_area_calc.NoFC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_585 #408,698.5

###############################################################

# 5) Caatinga

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil/Maps/Caatinga")

Cover_2000 <- raster("./forest.current_Crax_fasciolata.tif")
FC_2050 <- raster("./forest.future.FC_Crax_fasciolata.tif")
IDC3_2050 <- raster ("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
NoFC_2050 <- raster ("./forest.future.NoFC_Crax_fasciolata.tif")

# plot(Cover_2000)
# plot(FC_2050)
# plot(IDC3_2050)
# plot(NoFC_2050)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

suit_clim_cur <- raster ("./CUR.cont_Crax_fasciolata.tif")
suit_clim_fut_370 <- raster ("./Future_370_2050.cont_Crax_fasciolata.tif")
suit_clim_fut_585 <- raster ("./Future_585_2050.cont_Crax_fasciolata.tif")

# plot(suit_clim_cur)

# Current --------------------------------------
suit_env_cur <- suit_clim_cur * Cover_2000

# Future 370 --------------------------------------
# IDC3
suit_env_IDC3_370 <- suit_clim_fut_370 * IDC3_2050

# FC
suit_env_FC_370 <- suit_clim_fut_370 * FC_2050

# NoFC
suit_env_NoFC_370 <- suit_clim_fut_370 * NoFC_2050

# Future 585 --------------------------------------
# IDC3
suit_env_IDC3_585 <- suit_clim_fut_585 * IDC3_2050

# FC
suit_env_FC_585 <- suit_clim_fut_585 * FC_2050

# NoFC
suit_env_NoFC_585 <- suit_clim_fut_585 * NoFC_2050

plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(suit_env_FC_370)
plot(suit_env_NoFC_370)
plot(suit_env_IDC3_585)
plot(suit_env_FC_585)
plot(suit_env_NoFC_585)


# Current X Future 370 --------------------------------------
#IDC3
valid.pixels <- !is.na(suit_env_cur[])
valid.pixels1 <- !is.na(suit_env_IDC3_370[])
change.suit.env.IDC3.370 <- (sum(suit_env_IDC3_370[valid.pixels1])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.370 # -1.89

#FC
valid.pixels2 <- !is.na(suit_env_FC_370[])
change.suit.env.FC.370 <- (sum(suit_env_FC_370[valid.pixels2])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.370 #-6.85

#NoFC
valid.pixels3 <- !is.na(suit_env_NoFC_370[])
change.suit.env.NoFC.370 <- (sum(suit_env_NoFC_370[valid.pixels3])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.370 #2.48


# Current X Future 585 --------------------------------------
#IDC3
valid.pixels4 <- !is.na(suit_env_IDC3_585[])
change.suit.env.IDC3.585 <- (sum(suit_env_IDC3_585[valid.pixels4])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.585 #-2.38

#FC
valid.pixels5 <- !is.na(suit_env_FC_585[])
change.suit.env.FC.585 <- (sum(suit_env_FC_585[valid.pixels5])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.585 # -7.34

#NoFC
valid.pixels6 <- !is.na(suit_env_NoFC_585[])
change.suit.env.NoFC.585 <- (sum(suit_env_NoFC_585[valid.pixels6])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.585 #2.11


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")
dir.create("Caatinga")

target_dir = paste( './Caatinga', '/', sep="" )

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', '...', '\n')
writeRaster(suit_env_cur, file = paste(target_dir, '/suit.current_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_370, file = paste(target_dir, '/suit.IDC3_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_370, file = paste(target_dir, '/suit.FC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_370, file = paste(target_dir, '/suit.NoFC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_585, file = paste(target_dir, '/suit.IDC3_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_585, file = paste(target_dir, '/suit.FC_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_585, file = paste(target_dir, '/suit.NoFC_585_Crax_fasciolata', '.tif', sep=""))



setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental/Caatinga")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

#Current
suit_env_cur[suit_env_cur == 0] <- NA
plot(suit_env_cur)
r = raster(nrow=319, ncol=135, xmn=-44.5, xmx=-38.875, ymn=-16.08333, ymx=-2.791667) 
x = raster::area(r)
occurrence_area = x * suit_env_cur
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #10459.1 KM2

# IDC X 370
suit_env_IDC3_370[suit_env_IDC3_370 == 0] <- NA
plot(suit_env_IDC3_370)
occurrence_area = x * suit_env_IDC3_370
occurrence_area_calc.IDC3_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_370 #10257.98

# FC X 370
suit_env_FC_370[suit_env_FC_370 == 0] <- NA
plot(suit_env_FC_370)
occurrence_area = x * suit_env_FC_370
occurrence_area_calc.FC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_370 #9739.947

# NoFC X 370
suit_env_NoFC_370[suit_env_NoFC_370 == 0] <- NA
plot(suit_env_NoFC_370)
occurrence_area = x * suit_env_NoFC_370
occurrence_area_calc.NoFC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_370 #10716.33

# IDC X 585
suit_env_IDC3_585[suit_env_IDC3_585 == 0] <- NA
plot(suit_env_IDC3_585)
occurrence_area = x * suit_env_IDC3_585
occurrence_area_calc.IDC3_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_585 #10208.48

# FC X 585
suit_env_FC_585[suit_env_FC_585 == 0] <- NA
plot(suit_env_FC_585)
occurrence_area = x * suit_env_FC_585
occurrence_area_calc.FC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_585 #9689.96

# NoFC X 585
suit_env_NoFC_585[suit_env_NoFC_585 == 0] <- NA
plot(suit_env_NoFC_585)
occurrence_area = x * suit_env_NoFC_585
occurrence_area_calc.NoFC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_585 #10679.37

###############################################################


# 6) Pampa

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/GLOBIOM_Brasil/Maps/Pampa")


Cover_2000 <- raster("./forest.current_Crax_fasciolata.tif")
FC_2050 <- raster("./forest.future.FC_Crax_fasciolata.tif")
IDC3_2050 <- raster ("./forest.future.IDCImperfect3_Crax_fasciolata.tif")
NoFC_2050 <- raster ("./forest.future.NoFC_Crax_fasciolata.tif")

# plot(Cover_2000)
# plot(FC_2050)
# plot(IDC3_2050)
# plot(NoFC_2050)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

suit_clim_cur <- raster ("./CUR.cont_Crax_fasciolata.tif")
suit_clim_fut_370 <- raster ("./Future_370_2050.cont_Crax_fasciolata.tif")
suit_clim_fut_585 <- raster ("./Future_585_2050.cont_Crax_fasciolata.tif")

# plot(suit_clim_cur)

# Current --------------------------------------
suit_env_cur <- suit_clim_cur * Cover_2000

# Future 370 --------------------------------------
# IDC3
suit_env_IDC3_370 <- suit_clim_fut_370 * IDC3_2050

# FC
suit_env_FC_370 <- suit_clim_fut_370 * FC_2050

# NoFC
suit_env_NoFC_370 <- suit_clim_fut_370 * NoFC_2050

# Future 585 --------------------------------------
# IDC3
suit_env_IDC3_585 <- suit_clim_fut_585 * IDC3_2050

# FC
suit_env_FC_585 <- suit_clim_fut_585 * FC_2050

# NoFC
suit_env_NoFC_585 <- suit_clim_fut_585 * NoFC_2050

plot(suit_env_cur)
plot(suit_env_IDC3_370)
plot(suit_env_FC_370)
plot(suit_env_NoFC_370)
plot(suit_env_IDC3_585)
plot(suit_env_FC_585)
plot(suit_env_NoFC_585)


# Current X Future 370 --------------------------------------
#IDC3
valid.pixels <- !is.na(suit_env_cur[])
valid.pixels1 <- !is.na(suit_env_IDC3_370[])
change.suit.env.IDC3.370 <- (sum(suit_env_IDC3_370[valid.pixels1])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.370 # 51.18

#FC
valid.pixels2 <- !is.na(suit_env_FC_370[])
change.suit.env.FC.370 <- (sum(suit_env_FC_370[valid.pixels2])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.370 #51.18

#NoFC
valid.pixels3 <- !is.na(suit_env_NoFC_370[])
change.suit.env.NoFC.370 <- (sum(suit_env_NoFC_370[valid.pixels3])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.370 #51.18


# Current X Future 585 --------------------------------------
#IDC3
valid.pixels4 <- !is.na(suit_env_IDC3_585[])
change.suit.env.IDC3.585 <- (sum(suit_env_IDC3_585[valid.pixels4])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.IDC3.585 #15.37

#FC
valid.pixels5 <- !is.na(suit_env_FC_585[])
change.suit.env.FC.585 <- (sum(suit_env_FC_585[valid.pixels5])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.FC.585 # 15.37

#NoFC
valid.pixels6 <- !is.na(suit_env_NoFC_585[])
change.suit.env.NoFC.585 <- (sum(suit_env_NoFC_585[valid.pixels6])-sum(suit_env_cur[valid.pixels]))/sum(suit_env_cur[valid.pixels])*100 # em porcentagem, quanto mudou a suit climatica
change.suit.env.NoFC.585 #15.37


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental")
dir.create("Pampa")

target_dir = paste( './Pampa', '/', sep="" )

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', '...', '\n')
writeRaster(suit_env_cur, file = paste(target_dir, '/suit.current_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_370, file = paste(target_dir, '/suit.IDC3_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_370, file = paste(target_dir, '/suit.FC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_370, file = paste(target_dir, '/suit.NoFC_370_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_IDC3_585, file = paste(target_dir, '/suit.IDC3_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_FC_585, file = paste(target_dir, '/suit.FC_585_Crax_fasciolata', '.tif', sep=""))
writeRaster(suit_env_NoFC_585, file = paste(target_dir, '/suit.NoFC_585_Crax_fasciolata', '.tif', sep=""))



setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Adequabilidade_ambiental/Pampa")

suit_env_cur <- raster("suit.current_Crax_fasciolata.tif")
suit_env_IDC3_370 <- raster("suit.IDC3_370_Crax_fasciolata.tif")
suit_env_FC_370 <- raster("suit.FC_370_Crax_fasciolata.tif")
suit_env_NoFC_370 <- raster("suit.NoFC_370_Crax_fasciolata.tif")
suit_env_IDC3_585 <- raster("suit.IDC3_585_Crax_fasciolata.tif")
suit_env_FC_585 <- raster("suit.FC_585_Crax_fasciolata.tif")
suit_env_NoFC_585 <- raster("suit.NoFC_585_Crax_fasciolata.tif")

#Current
suit_env_cur[suit_env_cur == 0] <- NA
plot(suit_env_cur)
r = raster(nrow=136, ncol=192, xmn=-57.66667, xmx=-49.66667, ymn=-33.75, ymx=-28.08333) 
x = raster::area(r)
occurrence_area = x * suit_env_cur
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #424.3653 KM2

# IDC X 370
suit_env_IDC3_370[suit_env_IDC3_370 == 0] <- NA
plot(suit_env_IDC3_370)
occurrence_area = x * suit_env_IDC3_370
occurrence_area_calc.IDC3_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_370 #641.2038

# FC X 370
suit_env_FC_370[suit_env_FC_370 == 0] <- NA
plot(suit_env_FC_370)
occurrence_area = x * suit_env_FC_370
occurrence_area_calc.FC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_370 #641.2164

# NoFC X 370
suit_env_NoFC_370[suit_env_NoFC_370 == 0] <- NA
plot(suit_env_NoFC_370)
occurrence_area = x * suit_env_NoFC_370
occurrence_area_calc.NoFC_370 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_370 #641.2038

# IDC X 585
suit_env_IDC3_585[suit_env_IDC3_585 == 0] <- NA
plot(suit_env_IDC3_585)
occurrence_area = x * suit_env_IDC3_585
occurrence_area_calc.IDC3_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.IDC3_585 #489.5315

# FC X 585
suit_env_FC_585[suit_env_FC_585 == 0] <- NA
plot(suit_env_FC_585)
occurrence_area = x * suit_env_FC_585
occurrence_area_calc.FC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.FC_585 #489.5431

# NoFC X 585
suit_env_NoFC_585[suit_env_NoFC_585 == 0] <- NA
plot(suit_env_NoFC_585)
occurrence_area = x * suit_env_NoFC_585
occurrence_area_calc.NoFC_585 <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.NoFC_585 #489.5315

###############################################################