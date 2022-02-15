# -------------------------------------------------------------
# Calculating climatic suitability (South America, Brazil and 
# Brazilian Domains)
# 16 nov 2020
# Ana Claudia de Almeida
# -------------------------------------------------------------
#

library(raster)

Species = "Crax_fasciolata"

# South America

# Comparing current to future 370 -------------------------------------

Crax_results <- matrix(nrow = 1, ncol = 9)
colnames(Crax_results) <- c("Species", "unchanged_percent_bin", "loss_percent_bin", 
                          "gain_percent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                          "mean_diff_fut_cur_percent","CUR_ens_uncertainty_mean", 
                          "Future_370_2050_ens_uncertainty_mean")

getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")


  #Binary results
  CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
  Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")
  
  CUR_bin[CUR_bin == 1] <- 2
  
  plot(CUR_bin)
  
  diff_bin <- Future_370_2050_bin - CUR_bin
  plot(diff_bin)
  diff_bin[diff_bin == 0] <- NA
  plot(diff_bin) 
  unchanged <- ncell(which(diff_bin[] == -1))
  loss <- ncell(which(diff_bin[] == -2))
  gain <- ncell(which(diff_bin[] == 1))
  total_cells <- unchanged + loss + gain
  
  unchanged_percent_bin <- (unchanged/total_cells)*100
  loss_percent_bin <- (loss/total_cells)*100
  gain_percent_bin <- (gain/total_cells)*100
  extent_ecoph <- unchanged + gain
  
  table_results <- as.data.frame(matrix(nrow=1, ncol=4))
  colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                               "gain_percent_bin", "extent_ecoph")
  table_results[1,1] <- unchanged_percent_bin
  table_results[1,2] <- loss_percent_bin
  table_results[1,3] <- gain_percent_bin
  table_results[1,4] <- extent_ecoph
  
  setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/outputs_370")
  
  
  diff_bin[diff_bin == -1] <- 0
  diff_bin[diff_bin == -2] <- -1
  plot(diff_bin)
  writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)
  
  setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")
  
  #Continuous results
  CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
  Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")
  
  testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
  #Mean of difference - Negative value means loss, positive value means gain
  chars <- capture.output(print(testt))
  mean_diff_estimate <- as.data.frame(testt$estimate)
  mean_diff_fut_cur <- mean_diff_estimate[1,]
  mean_diff_fut_cur_percent <- mean_diff_fut_cur*100
  
  diff_cont <- Future_370_2050_cont - CUR_cont  
  diff_cont_percent <- diff_cont*100
  diff_cont_txt <- capture.output(print(diff_cont))
  diff_cont_percent_txt <- capture.output(print(diff_cont_percent))
  
  all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)
  
  setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/outputs_370")
  
  
  writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))
  
  plot(diff_cont)
  writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
  writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)
  
  #Normalizing
  minimo <- minValue(diff_cont)
  maximo <- maxValue(diff_cont)
  normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
  diff_cont_norm <- ((normalizar(diff_cont))*2)-1
  diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
  writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
  writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)
  
  #Uncertainty
  
  setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")
  
  CUR_ens_sd <- raster("ens.cur.sd.w.2_Crax_fasciolata.tif")
  CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
  CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))
  
  
  Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w.2_Crax_fasciolata.tif")
  Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
  Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))
  
  
  Crax_results[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                       gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                       CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)
  

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/outputs_370")
write.csv(Crax_results, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)
r = raster(nrow=898, ncol=832, xmn=-73.54167, xmx=-38.875, ymn=-33.75, ymx=3.666667) 
x = raster::area(r) 
plot(x)
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #area


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.370 <- occurrence_area_calc.370


CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #unchanged area

plot(diff_bin_loss)

# gained area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc 

# lost area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc 

# total (unchanged+loss+gain)
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc 

# Comparing current to future 585 -------------------------------------

Crax_results <- matrix(nrow = 1, ncol = 9)
colnames(Crax_results) <- c("Species", "unchanged_percent_bin", "loss_percent_bin", 
                            "gain_percent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                            "mean_diff_fut_cur_percent","CUR_ens_uncertainty_mean", 
                            "Future_585_2050_ens_uncertainty_mean")

getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")


#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")


CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 

unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/outputs_585")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


Crax_results[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                      gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                      CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/outputs_585")
write.csv(Crax_results, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)
r = raster(nrow=898, ncol=832, xmn=-73.54167, xmx=-38.875, ymn=-33.75, ymx=3.666667) 
x = raster::area(r) 
plot(x)
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #3,283,364 KM2


#future_585
Crax_fut_bin_area = Future_585_2050_bin 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 3,055,622 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.585 <- occurrence_area_calc.585


CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA

unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain


diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #2,441,714 - unchanged

plot(diff_bin_loss)

# gained area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #613,907.3 km²

# lost area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #841,649.4 km²

# total (unchanged+loss+gain)
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #3,897,271 km²


###############################################################


# Brazil --------------------------------------

# Comparing current to future 370 -------------------------------------

Crax_results <- matrix(nrow = 1, ncol = 9)
colnames(Crax_results) <- c("Species", "unchanged_percent_bin", "loss_percent_bin", 
                          "gain_percent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                          "mean_diff_fut_cur_percent","CUR_ens_uncertainty_mean", 
                          "Future_370_2050_ens_uncertainty_mean")

getwd()

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_370_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin)
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil/outputs_370")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_370_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil/outputs_370")


writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w_Crax_fasciolata.tif")
Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))


Crax_results[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                      gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                      CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil/outputs_370")
write.csv(Crax_results, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)
r = raster(nrow=898, ncol=832, xmn=-73.54167, xmx=-38.875, ymn=-33.75, ymx=3.666667) 
x = raster::area(r) 
plot(x)
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #2,382,553 KM2


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 # 2,445,467 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.370 <- occurrence_area_calc.370


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #1,959,856 km² - unchanged

plot(diff_bin_loss)

# gained area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #485,611.1 km²

# lost area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #422,696.9 km²

# Total (unchanged+loss+gain)
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #2,868,164 km²


# Comparing current to future 585 -------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil/outputs_585")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")

CUR_ens_sd <- raster("ens.cur.sd.w.2_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w.2_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


Crax_results[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                      gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                      CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil/outputs_585")
write.csv(Crax_results, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Brasil")


#future_585
Crax_fut_bin_area <- raster("Future_585_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 2,500,483 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.585 <- occurrence_area_calc.585


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #1,969,859 km² - unchanged

plot(diff_bin_loss)

# gained area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- NA
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #530,623.9 km²

# lost area
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- NA
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- NA
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #412,693.4 km²

# Total (unchanged+loss+gain)
diff_bin_loss <- diff_bin
plot(diff_bin_loss)
diff_bin_loss[diff_bin_loss == 1] <- 1
#unique(diff_bin_loss)
diff_bin_loss[diff_bin_loss == -1] <- 1
diff_bin_loss[diff_bin_loss == 0] <- NA
diff_bin_loss[diff_bin_loss == -2] <- 1
occurrence_area = x * diff_bin_loss
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #2,913,177 km²


###############################################################


# BRAZILIAN DOMAINS --------------------------------------

# Amazon --------------------------------------

# Comparing current to future 370 -------------------------------------

Amazon <- matrix(nrow = 1, ncol = 9)
colnames(Amazon) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                        "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                        "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                        "Future_370_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_370_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin)
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia/outputs_370")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_370_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia/outputs_370")


writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w_Crax_fasciolata.tif")
Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))


Amazon[1,] <- c(Species, unchanged_percent_bin, loss_percent_bin,
                  gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                  CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia/outputs_370")
write.csv(Amazonia, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
# Crax_cur_bin #479, 733, 351107  (nrow, ncol, ncell)
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)

r = raster(nrow=479, ncol=733, xmn=-73.54167, xmx=-43, ymn=-16.29167, ymx=3.666667) 
x = raster::area(r) 
plot(x)
occurrence_area = x * Crax_cur_bin_area
#plot(occurrence_area)
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #599,648.6 KM2


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 # 639,977.6 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.370 <- occurrence_area_calc.370


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #344,309.6 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #295,668 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #255,159 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #1,518,405 km²


# Comparing current to future 585 -------------------------------------

Amazonia <- matrix(nrow = 1, ncol = 9)
colnames(Amazonia) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                        "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                        "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                        "Future_585_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia/outputs_585")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


Amazonia[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                  gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                  CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia/outputs_585")
write.csv(Amazonia, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Amazonia")

#future_585
Crax_fut_bin_area <- raster("Future_585_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 700,543.5 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.585 <- occurrence_area_calc.585


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #351,926.8 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #348,616.7 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #247,541.7 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #948,085.3 km²

###############################################################

# Pantanal --------------------------------------

# Comparing current to future 370 -------------------------------------

Pantanal <- matrix(nrow = 1, ncol = 9)
colnames(Pantanal) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                        "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                        "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                        "Future_370_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_370_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal/outputs_370")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_370_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal/outputs_370")


writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w_Crax_fasciolata.tif")
Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))


Pantanal[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                  gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                  CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal/outputs_370")
write.csv(Pantanal, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
# Crax_cur_bin #159, 102, 16218  (nrow, ncol, ncell)
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)
# Crax_cur_bin #159, 102, 16218  (nrow, ncol, ncell)
r = raster(nrow=159, ncol=102, xmn=-59.16667, xmx=-54.91667, ymn=-22.16667, ymx=-15.54167) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #150,698 KM2


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 # 144,635 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.370 <- occurrence_area_calc.370


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #144,614.7 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #20,25026 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #6,083.24 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #150,718.2 km²

# Comparing current to future 585 -------------------------------------

Pantanal <- matrix(nrow = 1, ncol = 9)
colnames(Pantanal) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                        "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                        "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                        "Future_585_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin)
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal/outputs_585")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


Pantanal[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                  gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                  CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal/outputs_585")
write.csv(Pantanal, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pantanal")

#future_585
Crax_fut_bin_area <- raster("Future_585_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 135,424.7 km²

plot(Crax_fut_bin_area)


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #135,424.7 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #0 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #15,273.3 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #150,698 km²

###############################################################


# Atlantic Forest --------------------------------------

# Comparing current to future 370 -------------------------------------

AF <- matrix(nrow = 1, ncol = 9)
colnames(AF) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                  "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                  "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                  "Future_370_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_370_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF/outputs_370")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_370_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF/outputs_370")


writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w_Crax_fasciolata.tif")
Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))


AF[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
            gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
            CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF/outputs_370")
write.csv(AF, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
# Crax_cur_bin #596, 403, 240188  (nrow, ncol, ncell)
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)

r = raster(nrow=596, ncol=403, xmn=-55.66667, xmx=-38.875, ymn=-29.95833, ymx=-5.125) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #505,800.8 KM2


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 # 628,700.9 km²


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #503,497.1 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #125,203.8 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #2,303.708 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #631,004.6 km²


# Comparing current to future 585 -------------------------------------

AF <- matrix(nrow = 1, ncol = 9)
colnames(AF) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                  "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                  "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                  "Future_585_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF/outputs_585")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


AF[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
            gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
            CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF/outputs_585")
write.csv(AF, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_AF")

#future_585
Crax_fut_bin_area <- raster("Future_585_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 628,383.2 km²


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #504,311 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #124,072.2 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #1,489.882 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #629,873 km²

###############################################################


# Cerrado --------------------------------------

# Comparing current to future 370 -------------------------------------

Cerrado <- matrix(nrow = 1, ncol = 9)
colnames(Cerrado) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                       "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                       "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                       "Future_370_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_370_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin)
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado/outputs_370")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_370_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado/outputs_370")


writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w_Crax_fasciolata.tif")
Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))


Cerrado[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                 gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                 CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado/outputs_370")
write.csv(Cerrado, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
# Crax_cur_bin #536, 446, 239056  (nrow, ncol, ncell)
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)
# Crax_cur_bin #536, 446, 239056  (nrow, ncol, ncell)
r = raster(nrow=536, ncol=446, xmn=-60.125, xmx=-41.54167, ymn=-24.66667, ymx=-2.333333) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #1,106,382 KM2


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 # 991,754.9 km²


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #948,043.5 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #43,711.38 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #158,338 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #1,150,093 km²

# Comparing current to future 585 -------------------------------------

Cerrado <- matrix(nrow = 1, ncol = 9)
colnames(Cerrado) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                       "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                       "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                       "Future_585_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin)
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado/outputs_585")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


Cerrado[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                 gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                 CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado/outputs_585")
write.csv(Cerrado, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Cerrado")


#future_585
Crax_fut_bin_area <- raster("Future_585_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 998,423 km²


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #959,182.1 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #39,240.91 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #147,199.4 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #1,145,622 km²


# Caatinga --------------------------------------

# Comparing current to future 370 -------------------------------------

Caatinga <- matrix(nrow = 1, ncol = 9)
colnames(Caatinga) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                        "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                        "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                        "Future_370_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_370_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga/outputs_370")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_370_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga/outputs_370")


writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w_Crax_fasciolata.tif")
Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))


Caatinga[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                  gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                  CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga/outputs_370")
write.csv(Caatinga, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
# Crax_cur_bin #159, 102, 16218  (nrow, ncol, ncell)
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)
# Crax_cur_bin #159, 102, 16218  (nrow, ncol, ncell)
r = raster(nrow=319, ncol=135, xmn=-44.5, xmx=-38.875, ymn=-16.08333, ymx=-2.791667) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #20,386.39 KM2


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 # 40,450.5 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.370 <- occurrence_area_calc.370


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #19,552.47 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #20,898.03 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #833.92 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #41284.42 km²

# Comparing current to future 585 -------------------------------------

Caatinga <- matrix(nrow = 1, ncol = 9)
colnames(Caatinga) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                        "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                        "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                        "Future_585_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin)
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga/outputs_585")


diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


Caatinga[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
                  gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
                  CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga/outputs_585")
write.csv(Caatinga, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Caatinga")

#future_585
Crax_fut_bin_area <- raster("Future_585_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 37,648.26 km²

plot(Crax_fut_bin_area)


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #19,156.61 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #18,491.65 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #1,229.78 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #38,878.04 km²

###############################################################


# Pampa --------------------------------------

# Comparing current to future 370 -------------------------------------

Pampa <- matrix(nrow = 1, ncol = 9)
colnames(Pampa) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                     "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                     "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                     "Future_370_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_370_2050_bin <- raster("Future_370_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_370_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa/outputs_370")

diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_370.tif", overwrite=T)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_370_2050_cont <- raster("Future_370_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_370_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_370_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa/outputs_370")


writeLines(all_results_cont, con = file(paste0("all_results_cont_370.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_370.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_370.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_370.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_370_2050_ens_sd <- raster("ens.Future_370_2050.sd.w_Crax_fasciolata.tif")
Future_370_2050_ens_sd_noNA <- Future_370_2050_ens_sd[!is.na(Future_370_2050_ens_sd)]
Future_370_2050_ens_uncertainty_mean <- mean(abs(Future_370_2050_ens_sd_noNA))


Pampa[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
               gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
               CUR_ens_uncertainty_mean, Future_370_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa/outputs_370")
write.csv(Pampa, "Crax_results_370.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

#current
Crax_cur_bin_area <- raster("CUR.bin_Crax_fasciolata.tif")
# Crax_cur_bin #159, 102, 16218  (nrow, ncol, ncell)
Crax_cur_bin_area[Crax_cur_bin_area == 0] <- NA
plot(Crax_cur_bin_area)
# Crax_cur_bin #159, 102, 16218  (nrow, ncol, ncell)
r = raster(nrow=136, ncol=192, xmn=-57.66667, xmx=-49.66667, ymn=-33.75, ymx=-28.08333) 
x = raster::area(r) 
occurrence_area = x * Crax_cur_bin_area
occurrence_area_calc.cur <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.cur #37.16 KM2


#future_370
Crax_fut_bin_area <- raster("Future_370_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.370 = x * Crax_fut_bin_area
plot(occurrence_area.370)
occurrence_area_calc.370 <- cellStats(occurrence_area.370, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.370 # 185.69 km²

plot(Crax_fut_bin_area)

table_results$occurrence_area_km2_cur <- occurrence_area_calc.cur
table_results$occurrence_area_km2_fut.370 <- occurrence_area_calc.370

# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_370_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #37.16 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #148.53 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #0 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #41284.42 km²

# Comparing current to future 585 -------------------------------------

Pampa <- matrix(nrow = 1, ncol = 9)
colnames(Pampa) <- c("Species", "unchanged_porcent_bin", "loss_porcent_bin", 
                     "gain_porcent_bin", "extent_ecoph", "mean_diff_fut_cur", 
                     "mean_diff_fut_cur_porcent","CUR_ens_uncertainty_mean", 
                     "Future_585_2050_ens_uncertainty_mean")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

#Binary results
CUR_bin <- raster("CUR.bin_Crax_fasciolata.tif")
Future_585_2050_bin <- raster("Future_585_2050.bin_Crax_fasciolata.tif")

CUR_bin[CUR_bin == 1] <- 2
plot(CUR_bin)

diff_bin <- Future_585_2050_bin - CUR_bin
plot(diff_bin)
diff_bin[diff_bin == 0] <- NA
plot(diff_bin) 
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

unchanged_percent_bin <- (unchanged/total_cells)*100
loss_percent_bin <- (loss/total_cells)*100
gain_percent_bin <- (gain/total_cells)*100
extent_ecoph <- unchanged + gain

table_results <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(table_results) <- c("unchanged_percent_bin", "loss_percent_bin",
                             "gain_percent_bin", "extent_ecoph")
table_results[1,1] <- unchanged_percent_bin
table_results[1,2] <- loss_percent_bin
table_results[1,3] <- gain_percent_bin
table_results[1,4] <- extent_ecoph

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa/outputs_585")

diff_bin[diff_bin == -1] <- 0
diff_bin[diff_bin == -2] <- -1
plot(diff_bin)
writeRaster(diff_bin, "diff_bin_585.tif", overwrite=T)
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

#Continuous results
CUR_cont <- raster("CUR.cont_Crax_fasciolata.tif")
Future_585_2050_cont <- raster("Future_585_2050.cont_Crax_fasciolata.tif")

testt <- t.test(Future_585_2050_cont[], CUR_cont[], paired=T)
#Mean of difference - Negative value means loss, positive value means gain
chars <- capture.output(print(testt))
mean_diff_estimate <- as.data.frame(testt$estimate)
mean_diff_fut_cur <- mean_diff_estimate[1,]
mean_diff_fut_cur_percent <- mean_diff_fut_cur*100

diff_cont <- Future_585_2050_cont - CUR_cont  
diff_cont_percent <- diff_cont*100
diff_cont_txt <- capture.output(print(diff_cont))
diff_cont_percent_txt <- capture.output(print(diff_cont_percent))

all_results_cont <- c(chars,diff_cont_txt, diff_cont_percent_txt)

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa/outputs_585")


writeLines(all_results_cont, con = file(paste0("all_results_cont_585.txt")))

plot(diff_cont)
writeRaster(diff_cont, "diff_cont_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent, "diff_cont_percent_585.tif", overwrite=TRUE)

#Normalizing
minimo <- minValue(diff_cont)
maximo <- maxValue(diff_cont)
normalizar <- function(x) {(x-minimo)/(maximo-minimo)}
diff_cont_norm <- ((normalizar(diff_cont))*2)-1
diff_cont_percent_norm <- ((normalizar(diff_cont_percent))*2)-1
writeRaster(diff_cont_norm, "diff_cont_norm_585.tif", overwrite=TRUE)
writeRaster(diff_cont_percent_norm, "diff_cont_percent_norm_585.tif",overwrite=TRUE)

#Uncertainty

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

CUR_ens_sd <- raster("ens.cur.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA <- CUR_ens_sd[!is.na(CUR_ens_sd)]
CUR_ens_uncertainty_mean <- mean(abs(CUR_ens_sd_noNA))


Future_585_2050_ens_sd <- raster("ens.Future_585_2050.sd.w_Crax_fasciolata.tif")
Future_585_2050_ens_sd_noNA <- Future_585_2050_ens_sd[!is.na(Future_585_2050_ens_sd)]
Future_585_2050_ens_uncertainty_mean <- mean(abs(Future_585_2050_ens_sd_noNA))


Pampa[1,] <- c(Species,unchanged_percent_bin, loss_percent_bin,
               gain_percent_bin, extent_ecoph, mean_diff_fut_cur, mean_diff_fut_cur_percent,
               CUR_ens_uncertainty_mean, Future_585_2050_ens_uncertainty_mean)


setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa/outputs_585")
write.csv(Pampa, "Crax_results_585.csv", row.names = F)


# AREA --------------------------------------------------------------------

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_5km/Recorte_Pampa")

#future_585
Crax_fut_bin_area <- raster("Future_585_2050.bin_Crax_fasciolata.tif") 
Crax_fut_bin_area[Crax_fut_bin_area == 0] <- NA
plot(Crax_fut_bin_area)
occurrence_area.585 = x * Crax_fut_bin_area
plot(occurrence_area.585)
occurrence_area_calc.585 <- cellStats(occurrence_area.585, stat='sum',
                                      na.rm=TRUE, asSample=TRUE)
occurrence_area_calc.585 # 278.46 km²

plot(Crax_fut_bin_area)


# Non-dispersal scenario

CUR_bin[CUR_bin == 1] <- 2
diff_bin <- Future_585_2050_bin - CUR_bin
diff_bin[diff_bin == 0] <- NA
unchanged <- ncell(which(diff_bin[] == -1))
loss <- ncell(which(diff_bin[] == -2))
gain <- ncell(which(diff_bin[] == 1))
total_cells <- unchanged + loss + gain

diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #19,156.61 km² - unchanged

plot(diff_bin_perda)

#gained area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- NA
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #241.3 km²

# lost area
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- NA
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- NA
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #0 km²

# Total (unchanged+loss+gain)
diff_bin_perda <- diff_bin
plot(diff_bin_perda)
diff_bin_perda[diff_bin_perda == 1] <- 1
#unique(diff_bin_perda)
diff_bin_perda[diff_bin_perda == -1] <- 1
diff_bin_perda[diff_bin_perda == 0] <- NA
diff_bin_perda[diff_bin_perda == -2] <- 1
occurrence_area = x * diff_bin_perda
occurrence_area_calc <- cellStats(occurrence_area, stat='sum', na.rm=TRUE, asSample=TRUE)
occurrence_area_calc #278.46 km²

###############################################################