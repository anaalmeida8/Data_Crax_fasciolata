# -------------------------------------------------------------
# Uncertainties by algorithm
# 24 mar 2021
# Ana Claudia de Almeida
# -------------------------------------------------------------
#

library(raster)


getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/outputs_0.7")

#Uncertainty

Crax_results <- matrix(nrow = 1, ncol = 12)
colnames(Crax_results) <- c("CUR_ens_uncertainty_mean.bc",
                            "Future_370_2050_ens_uncertainty_mean.bc",
                            "Future_585_2050_ens_uncertainty_mean.bc",
                            "CUR_ens_uncertainty_mean.rf",
                            "Future_370_2050_ens_uncertainty_mean.rf",
                            "Future_585_2050_ens_uncertainty_mean.rf",
                            "CUR_ens_uncertainty_mean.mx",
                            "Future_370_2050_ens_uncertainty_mean.mx",
                            "Future_585_2050_ens_uncertainty_mean.mx",
                            "CUR_ens_uncertainty_mean.sv",
                            "Future_370_2050_ens_uncertainty_mean.sv",
                            "Future_585_2050_ens_uncertainty_mean.sv")

#BC

CUR_ens_sd.bc <- raster("cur.bc.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA.bc <- CUR_ens_sd.bc[!is.na(CUR_ens_sd.bc)]
CUR_ens_uncertainty_mean.bc <- mean(abs(CUR_ens_sd_noNA.bc))

fut.bc.csm2.370 <- raster("CSM2_370_2050.bc.sd.w_Crax_fasciolata.tif")
fut.bc.ipsl.370 <- raster("IPSL_370_2050.bc.sd.w_Crax_fasciolata.tif")
fut.bc.miroc.370 <- raster("IPSL_370_2050.bc.sd.w_Crax_fasciolata.tif")

Future_370_2050_ens_sd.bc <- mean(fut.bc.csm2.370,fut.bc.ipsl.370,fut.bc.miroc.370)
Future_370_2050_ens_sd_noNA.bc <- Future_370_2050_ens_sd.bc[!is.na(Future_370_2050_ens_sd.bc)]
Future_370_2050_ens_uncertainty_mean.bc <- mean(abs(Future_370_2050_ens_sd_noNA.bc))


fut.bc.csm2.585 <- raster("CSM2_585_2050.bc.sd.w_Crax_fasciolata.tif")
fut.bc.ipsl.585 <- raster("IPSL_585_2050.bc.sd.w_Crax_fasciolata.tif")
fut.bc.miroc.585 <- raster("IPSL_585_2050.bc.sd.w_Crax_fasciolata.tif")

Future_585_2050_ens_sd.bc <- mean(fut.bc.csm2.585,fut.bc.ipsl.585,fut.bc.miroc.585)
Future_585_2050_ens_sd_noNA.bc <- Future_585_2050_ens_sd.bc[!is.na(Future_585_2050_ens_sd.bc)]
Future_585_2050_ens_uncertainty_mean.bc <- mean(abs(Future_585_2050_ens_sd_noNA.bc))


#RF
CUR_ens_sd.rf <- raster("cur.rf.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA.rf <- CUR_ens_sd.rf[!is.na(CUR_ens_sd.rf)]
CUR_ens_uncertainty_mean.rf <- mean(abs(CUR_ens_sd_noNA.rf))

fut.rf.csm2.370 <- raster("CSM2_370_2050.rf.sd.w_Crax_fasciolata.tif")
fut.rf.ipsl.370 <- raster("IPSL_370_2050.rf.sd.w_Crax_fasciolata.tif")
fut.rf.miroc.370 <- raster("IPSL_370_2050.rf.sd.w_Crax_fasciolata.tif")

Future_370_2050_ens_sd.rf <- mean(fut.rf.csm2.370,fut.rf.ipsl.370,fut.rf.miroc.370)
Future_370_2050_ens_sd_noNA.rf <- Future_370_2050_ens_sd.rf[!is.na(Future_370_2050_ens_sd.rf)]
Future_370_2050_ens_uncertainty_mean.rf <- mean(abs(Future_370_2050_ens_sd_noNA.rf))


fut.rf.csm2.585 <- raster("CSM2_585_2050.rf.sd.w_Crax_fasciolata.tif")
fut.rf.ipsl.585 <- raster("IPSL_585_2050.rf.sd.w_Crax_fasciolata.tif")
fut.rf.miroc.585 <- raster("IPSL_585_2050.rf.sd.w_Crax_fasciolata.tif")

Future_585_2050_ens_sd.rf <- mean(fut.rf.csm2.585,fut.rf.ipsl.585,fut.rf.miroc.585)
Future_585_2050_ens_sd_noNA.rf <- Future_585_2050_ens_sd.rf[!is.na(Future_585_2050_ens_sd.rf)]
Future_585_2050_ens_uncertainty_mean.rf <- mean(abs(Future_585_2050_ens_sd_noNA.rf))



#MX
CUR_ens_sd.mx <- raster("cur.mx.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA.mx <- CUR_ens_sd.mx[!is.na(CUR_ens_sd.mx)]
CUR_ens_uncertainty_mean.mx <- mean(abs(CUR_ens_sd_noNA.mx))

fut.mx.csm2.370 <- raster("CSM2_370_2050.mx.sd.w_Crax_fasciolata.tif")
fut.mx.ipsl.370 <- raster("IPSL_370_2050.mx.sd.w_Crax_fasciolata.tif")
fut.mx.miroc.370 <- raster("IPSL_370_2050.mx.sd.w_Crax_fasciolata.tif")

Future_370_2050_ens_sd.mx <- mean(fut.mx.csm2.370,fut.mx.ipsl.370,fut.mx.miroc.370)
Future_370_2050_ens_sd_noNA.mx <- Future_370_2050_ens_sd.mx[!is.na(Future_370_2050_ens_sd.mx)]
Future_370_2050_ens_uncertainty_mean.mx <- mean(abs(Future_370_2050_ens_sd_noNA.mx))


fut.mx.csm2.585 <- raster("CSM2_585_2050.mx.sd.w_Crax_fasciolata.tif")
fut.mx.ipsl.585 <- raster("IPSL_585_2050.mx.sd.w_Crax_fasciolata.tif")
fut.mx.miroc.585 <- raster("IPSL_585_2050.mx.sd.w_Crax_fasciolata.tif")

Future_585_2050_ens_sd.mx <- mean(fut.mx.csm2.585,fut.mx.ipsl.585,fut.mx.miroc.585)
Future_585_2050_ens_sd_noNA.mx <- Future_585_2050_ens_sd.mx[!is.na(Future_585_2050_ens_sd.mx)]
Future_585_2050_ens_uncertainty_mean.mx <- mean(abs(Future_585_2050_ens_sd_noNA.mx))



#SVM
CUR_ens_sd.sv <- raster("cur.sv.sd.w_Crax_fasciolata.tif")
CUR_ens_sd_noNA.sv <- CUR_ens_sd.sv[!is.na(CUR_ens_sd.sv)]
CUR_ens_uncertainty_mean.sv <- mean(abs(CUR_ens_sd_noNA.sv))

fut.sv.csm2.370 <- raster("CSM2_370_2050.sv.sd.w_Crax_fasciolata.tif")
fut.sv.ipsl.370 <- raster("IPSL_370_2050.sv.sd.w_Crax_fasciolata.tif")
fut.sv.miroc.370 <- raster("IPSL_370_2050.sv.sd.w_Crax_fasciolata.tif")

Future_370_2050_ens_sd.sv <- mean(fut.sv.csm2.370,fut.sv.ipsl.370,fut.sv.miroc.370)
Future_370_2050_ens_sd_noNA.sv <- Future_370_2050_ens_sd.sv[!is.na(Future_370_2050_ens_sd.sv)]
Future_370_2050_ens_uncertainty_mean.sv <- mean(abs(Future_370_2050_ens_sd_noNA.sv))


fut.sv.csm2.585 <- raster("CSM2_585_2050.sv.sd.w_Crax_fasciolata.tif")
fut.sv.ipsl.585 <- raster("IPSL_585_2050.sv.sd.w_Crax_fasciolata.tif")
fut.sv.miroc.585 <- raster("IPSL_585_2050.sv.sd.w_Crax_fasciolata.tif")

Future_585_2050_ens_sd.sv <- mean(fut.sv.csm2.585,fut.sv.ipsl.585,fut.sv.miroc.585)
Future_585_2050_ens_sd_noNA.sv <- Future_585_2050_ens_sd.sv[!is.na(Future_585_2050_ens_sd.sv)]
Future_585_2050_ens_uncertainty_mean.sv <- mean(abs(Future_585_2050_ens_sd_noNA.sv))



Crax_results[1,] <- c(CUR_ens_uncertainty_mean.bc,
                      Future_370_2050_ens_uncertainty_mean.bc,
                      Future_585_2050_ens_uncertainty_mean.bc,
                      CUR_ens_uncertainty_mean.rf,
                      Future_370_2050_ens_uncertainty_mean.rf,
                      Future_585_2050_ens_uncertainty_mean.rf,
                      CUR_ens_uncertainty_mean.mx,
                      Future_370_2050_ens_uncertainty_mean.mx,
                      Future_585_2050_ens_uncertainty_mean.mx,
                      CUR_ens_uncertainty_mean.sv,
                      Future_370_2050_ens_uncertainty_mean.sv,
                      Future_585_2050_ens_uncertainty_mean.sv)

write.csv(Crax_results, "Crax_uncertainties_models.csv", row.names = F)

###############################################################
