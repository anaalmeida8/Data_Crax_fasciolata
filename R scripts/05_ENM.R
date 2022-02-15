# -------------------------------------------------------------
# ENM Crax fasciolata
# 30 out 2020
# Ana Claudia de Almeida
# -------------------------------------------------------------
#

memory.limit(9999999999)

#install.packages("remotes")
#remotes::install_version("SDMTools","1.1-221")

#install.packages("devtools")
library("devtools")
# devtools::install_github("jjvanderwal/SDMTools")

# Required packages
library(parallel)
library(foreach)
library(doParallel)
require(raster)
library(rgeos)
library(rgdal)
library(dismo)
library(rJava)
library(kernlab)
library(randomForest)
library(maptools)
library(plyr)
library(SDMTools)
library(beepr)

indexOf <- function(v,findFor) {
  
  i=0
  for(i2 in v) {
    i = i + 1
    if (i2==findFor){
      return(i)
    }
  }
  return(0)
}


#MaxEnt .jar
#jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
#if (file.exists(jar) != T) {
#  url = "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
#  download.file(url, dest = "maxent.zip", mode = "wb")
#  unzip("maxent.zip",
#        files = "maxent.jar",
#        exdir = system.file("java", package = "dismo"))
#  unlink("maxent.zip")
#  warning("Maxent foi colocado no diretorio")
#}

getwd()
setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new/5km")


file = 'Crax_fasciolata_training_5.csv' # Name of .csv file with species occurrences under three columns: 'species', 'lon', 'lat'
model <- pa ~ bio_4 + bio_8 + bio_14 + bio_18 + bio_19 # variable names
k = 10 #number of replicas
bg.pt = 10000 #background points
t.met = 'spec_sens' # threshold selection method (specificity-sensitivity)
tss.lim = 0.7 # Minimum TSS value for ensemble

cont.maps = T # saving continuous maps by algorithm
bin.maps = T # saving binary maps by algorithm
ens.maps = T # saving ensemble maps (continuous and binary)

############ code

pres <- read.csv(file)
sp.names <- as.character(unique(pres$species))
sp.n = sp.names[1] 

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m")

predictors <- stack("./Current/5km/bio_4.tif", 
                    "./Current/5km/bio_8.tif", 
                    "./Current/5km/bio_14.tif",
                    "./Current/5km/bio_18.tif",
                    "./Current/5km/bio_19.tif")

CSM2_370 <- stack("./Future/BCC-CSM2-MR/ssp370/5km/bio_4.tif",
                 "./Future/BCC-CSM2-MR/ssp370/5km/bio_8.tif",
                 "./Future/BCC-CSM2-MR/ssp370/5km/bio_14.tif",
                 "./Future/BCC-CSM2-MR/ssp370/5km/bio_18.tif",
                 "./Future/BCC-CSM2-MR/ssp370/5km/bio_19.tif")

IPSL_370 <- stack("./Future/IPSL-CM6A-LR/ssp370/5km/bio_4.tif",
              "./Future/IPSL-CM6A-LR/ssp370/5km/bio_8.tif",
              "./Future/IPSL-CM6A-LR/ssp370/5km/bio_14.tif",
              "./Future/IPSL-CM6A-LR/ssp370/5km/bio_18.tif",
              "./Future/IPSL-CM6A-LR/ssp370/5km/bio_19.tif")

MIROC6_370 <- stack("./Future/MIROC6/ssp370/5km/bio_4.tif",
              "./Future/MIROC6/ssp370/5km/bio_8.tif",
              "./Future/MIROC6/ssp370/5km/bio_14.tif",
              "./Future/MIROC6/ssp370/5km/bio_18.tif",
              "./Future/MIROC6/ssp370/5km/bio_19.tif")

CSM2_585 <- stack("./Future/BCC-CSM2-MR/ssp585/5km/bio_4.tif",
              "./Future/BCC-CSM2-MR/ssp585/5km/bio_8.tif",
              "./Future/BCC-CSM2-MR/ssp585/5km/bio_14.tif",
              "./Future/BCC-CSM2-MR/ssp585/5km/bio_18.tif",
              "./Future/BCC-CSM2-MR/ssp585/5km/bio_19.tif")

IPSL_585 <- stack("./Future/IPSL-CM6A-LR/ssp585/5km/bio_4.tif",
              "./Future/IPSL-CM6A-LR/ssp585/5km/bio_8.tif",
              "./Future/IPSL-CM6A-LR/ssp585/5km/bio_14.tif",
              "./Future/IPSL-CM6A-LR/ssp585/5km/bio_18.tif",
              "./Future/IPSL-CM6A-LR/ssp585/5km/bio_19.tif")

MIROC6_585 <- stack("./Future/MIROC6/ssp585/5km/bio_4.tif",
              "./Future/MIROC6/ssp585/5km/bio_8.tif",
              "./Future/MIROC6/ssp585/5km/bio_14.tif",
              "./Future/MIROC6/ssp585/5km/bio_18.tif",
              "./Future/MIROC6/ssp585/5km/bio_19.tif")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata")

ini = Sys.time()
pb <- txtProgressBar(min = 1, max = length(sp.names)+1, style = 3)

lim = count(pres$species)$freq[indexOf(count(pres$species)$x, sp.n)]; # Number of occurrences to perform pseudo-absence sampling

# prepare environment for species
started_time = Sys.time()
cat( format( started_time, "%a %b %d %X %Y"), '-', 'STARTED', '\n')
cat( format( started_time, "%a %b %d %X %Y"), '-', 'Preparing train and test datasets for', sp.n, 'with ', lim, 'lines...', '\n')

target_dir = paste( "./outputs_5km", '/', sep="" )
dir.create( target_dir )

if(file.exists(paste(target_dir, '/STARTED.txt', sep="")))
  stop("You MUST DELETE output folder before continue")

write(format( started_time, "%a %b %d %X %Y"), file=paste("./outputs_5km", '/STARTED.txt', sep=""))

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata")
sp.data <- read.csv(paste('./Dados/new/5km/', "pres_pseudoabs_1000_5km", '.csv', sep=""), header=TRUE, sep=',')
sp.data2 <- read.csv(paste('./Dados/new/5km/', "pres_pseudoabs_304_5km", '.csv', sep=""), header=TRUE, sep=',')
sp.data3 <- read.csv(paste('./Dados/new/5km/', "pres_pseudoabs_10000_5km", '.csv', sep=""), header=TRUE, sep=',')


#for bioclim and maxent:
pres <- sp.data[sp.data$pa==1,2:3]
abs <- sp.data[sp.data$pa!=1,2:3] 
bg <- randomPoints(predictors, bg.pt)
colnames(bg) <- c("lon", "lat")

set.seed(10) 
foldpres <- kfold(pres, 4) # testing and training for presences
set.seed(10) 
foldabs <- kfold(abs, 4) # testing and training for absences

prestrain <- list()
prestest <- list()
abstrain <- list()
abstest <- list()
for(i in 1:k){
  foldpres <- kfold(pres, 4) 
  foldabs <- kfold(abs, 4) 
  prestrain[[i]] <- pres[foldpres != 1,] #presences associated with kfold are different of 1 (3/4 of the set) 
  prestest[[i]] <- pres[foldpres == 1,] #presences associated with kfold are equal to 1 (1/4 of the set)
  abstrain[[i]] <- abs[foldabs != 1,] #absences associated with kfold are different of 1 (3/4 of the set)
  abstest[[i]] <- abs[foldabs == 1,] #absences associated with kfold are equal to 1 (1/4 of the set)
} 


# for SVM:  
train <- list()
pa_train <- list()
predtrain <- list()
testpres <- list()
testabs <- list()

for(i in 1:k){
  train[[i]] <- rbind(prestrain[[i]], abstrain[[i]])
  pa_train[[i]] <- c(rep(1, nrow(prestrain[[i]])), rep(0, nrow(abstrain[[i]])))
  predtrain[[i]] <- extract(predictors, train[[i]])
  predtrain[[i]] <- data.frame(cbind(pa=pa_train[[i]], predtrain[[i]]))
}


#for Random Forest
pres2 <- sp.data2[sp.data2$pa==1,2:3]
abs2 <- sp.data2[sp.data2$pa!=1,2:3] 
set.seed(10) 
foldpres2 <- kfold(pres2, 4)
set.seed(10) 
foldabs2 <- kfold(abs2, 4)

prestrain2 <- list()
prestest2 <- list()
abstrain2 <- list()
abstest2 <- list()
for(i in 1:k){
  foldpres2 <- kfold(pres2, 4) 
  foldabs2 <- kfold(abs2, 4) 
  prestrain2[[i]] <- pres2[foldpres2 != 1,] 
  prestest2[[i]] <- pres2[foldpres2 == 1,] 
  abstrain2[[i]] <- abs2[foldabs2 != 1,] 
  abstest2[[i]] <- abs2[foldabs2 == 1,] 
}

train2 <- list()
pa_train2 <- list()
predtrain2 <- list()
testpres2 <- list()
testabs2 <- list()

for(i in 1:k){
  train2[[i]] <- rbind(prestrain2[[i]], abstrain2[[i]])
  pa_train2[[i]] <- c(rep(1, nrow(prestrain2[[i]])), rep(0, nrow(abstrain2[[i]])))
  predtrain2[[i]] <- extract(predictors, train2[[i]])
  predtrain2[[i]] <- data.frame(cbind(pa=pa_train2[[i]], predtrain2[[i]]))
}


# for GLM:
pres3 <- sp.data3[sp.data3$pa==1,2:3]
abs3 <- sp.data3[sp.data3$pa!=1,2:3] 
set.seed(10) 
foldpres3 <- kfold(pres3, 4)
set.seed(10) 
foldabs3 <- kfold(abs3, 4)

prestrain3 <- list()
prestest3 <- list()
abstrain3 <- list()
abstest3 <- list()
for(i in 1:k){
  foldpres3 <- kfold(pres3, 4) 
  foldabs3 <- kfold(abs3, 4) 
  prestrain3[[i]] <- pres3[foldpres3 != 1,] 
  prestest3[[i]] <- pres3[foldpres3 == 1,] 
  abstrain3[[i]] <- abs3[foldabs3 != 1,] 
  abstest3[[i]] <- abs3[foldabs3 == 1,] 
} 

train3 <- list()
pa_train3 <- list()
predtrain3 <- list()
testpres3 <- list()
testabs3 <- list()

for(i in 1:k){
  train3[[i]] <- rbind(prestrain3[[i]], abstrain3[[i]])
  pa_train3[[i]] <- c(rep(1, nrow(prestrain3[[i]])), rep(0, nrow(abstrain3[[i]])))
  predtrain3[[i]] <- extract(predictors, train3[[i]])
  predtrain3[[i]] <- data.frame(cbind(pa=pa_train3[[i]], predtrain3[[i]]))
}




cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Bioclim model for', sp.n, '...', '\n')
bc <- list()
evbc <- list()
bcTSS <- list()
bcAUC <- list()
bckappa <- list()
bcthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Bioclim (', i, ') model for', sp.n, '...', '\n')
  bc[[i]] <- bioclim(predictors, prestrain[[i]])
  evbc[[i]] <- evaluate(prestest[[i]], abstest[[i]], bc[[i]], predictors)
  bcTSS[[i]] <- max(evbc[[i]]@TPR + evbc[[i]]@TNR)-1
  bcAUC[[i]] <- evbc[[i]]@auc
  bckappa[[i]] <- max(evbc[[i]]@kappa) 
  bcthres[[i]] <- threshold(evbc[[i]], t.met)
}

#tirei glm porque os valores de TSS foram baixos
cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running GLM (logistic regression) for', sp.n, '...', '\n')
gm <- list()
evgm <- list()
gmTSS <- list()
gmAUC <- list()
gmkappa <- list()
gmthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running GLM (logistic regression) (', i, ') for', sp.n, '...', '\n')
  gm[[i]] <- glm(model, family=binomial(link="logit"), data=predtrain3[[i]]) 
  evgm[[i]] <- evaluate(prestest3[[i]], abstest3[[i]], gm[[i]], predictors) 
  gmTSS[[i]] <- max(evgm[[i]]@TPR + evgm[[i]]@TNR)-1
  gmAUC[[i]] <- evgm[[i]]@auc
  gmkappa[[i]] <- max(evgm[[i]]@kappa)
  gmthres[[i]] <- threshold(evgm[[i]], t.met)
}

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Random Forest model for', sp.n, '...', '\n')
rf <- list()
evrf <- list()
rfTSS <- list()
rfAUC <- list()
rfkappa <- list()
rfthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Random Forest (', i, ') model for', sp.n, '...', '\n')
  rf[[i]] <- randomForest(model, data=predtrain2[[i]], na.action=na.omit) 
  evrf[[i]] <- evaluate(prestest2[[i]], abstest2[[i]], rf[[i]], predictors)
  rfTSS[[i]] <- max(evrf[[i]]@TPR + evrf[[i]]@TNR)-1
  rfAUC[[i]] <- evrf[[i]]@auc
  rfkappa[[i]] <- max(evrf[[i]]@kappa)
  rfthres[[i]] <- threshold(evrf[[i]], t.met)
}

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Maxent model for', sp.n, '...', '\n')
mx <- list()
evmx <- list()
mxTSS <- list()
mxAUC <- list()
mxkappa <- list()
mxthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Maxent (', i, ') model for', sp.n, '...', '\n')
  mx[[i]] <- maxent(predictors, prestrain[[i]], a=bg) 
  evmx[[i]] <- evaluate(prestest[[i]], abstest[[i]], mx[[i]], predictors)
  mxTSS[[i]] <- max(evmx[[i]]@TPR + evmx[[i]]@TNR)-1
  mxAUC[[i]] <- evmx[[i]]@auc
  mxkappa[[i]] <- max(evmx[[i]]@kappa)
  mxthres[[i]] <- threshold(evmx[[i]], t.met)
}

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running SVM model for', sp.n, '...', '\n')
sv <- list()
evsv <- list()
svTSS <- list()
svAUC <- list()
svkappa <- list()
svthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running SVM (', i, ') model for', sp.n, '...', '\n')
  sv[[i]] <- ksvm(model, data=predtrain[[i]]) 
  evsv[[i]] <- evaluate(prestest[[i]], abstest[[i]], sv[[i]], predictors)
  svTSS[[i]] <- max(evsv[[i]]@TPR + evsv[[i]]@TNR)-1
  svAUC[[i]] <- evsv[[i]]@auc
  svkappa[[i]] <- max(evsv[[i]]@kappa)
  svthres[[i]] <- threshold(evsv[[i]], t.met)
}


cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Generating Validation table for models of', sp.n, '...', '\n')
bcTSSval <- unlist(bcTSS)
gmTSSval <- unlist(gmTSS)
rfTSSval <- unlist(rfTSS)
mxTSSval <- unlist(mxTSS)
svTSSval <- unlist(svTSS)
bcAUCval <- unlist(bcAUC)
gmAUCval <- unlist(gmAUC)
rfAUCval <- unlist(rfAUC)
mxAUCval <- unlist(mxAUC)
svAUCval <- unlist(svAUC)
bckappaval <- unlist(bckappa)
gmkappaval <- unlist(gmkappa)
rfkappaval <- unlist(rfkappa)
mxkappaval <- unlist(mxkappa)
svkappaval <- unlist(svkappa)
mod.names <- c(rep ('bc', k), rep('gm', k), rep('rf', k), rep('mx', k), rep('sv', k))
mod.sp <- c(rep(sp.n, k*5)) 
TSS <- c(bcTSSval, gmTSSval, rfTSSval, mxTSSval, svTSSval)
AUC <- c(bcAUCval, gmAUCval, rfAUCval, mxAUCval, svAUCval)
kappa <- c(bckappaval, gmkappaval, rfkappaval, mxkappaval, svkappaval)
Valid <- data.frame(mod.sp, mod.names, TSS, AUC, kappa, stringsAsFactors=FALSE)
write.csv(Valid, file = paste( target_dir, '/Valid_', sp.n, '.csv', sep=""))



# BIOCLIM --------------------------------------

# Drop bad models, project under current and future conditions
cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Bioclim model of', sp.n, '...', '\n')
cur.bc <- list()
cur.bc.bin <- list()
CSM2_370.bc <- list()
CSM2_370.bc.bin <- list()
IPSL_370.bc <- list()
IPSL_370.bc.bin <- list()
MIROC6_370.bc <- list()
MIROC6_370.bc.bin <- list()
CSM2_585.bc <- list()
CSM2_585.bc.bin <- list()
IPSL_585.bc <- list()
IPSL_585.bc.bin <- list()
MIROC6_585.bc <- list()
MIROC6_585.bc.bin <- list()


for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Bioclim (', i, ') model of', sp.n, '...', '\n')
  if(bcTSS[[i]] >= tss.lim){
    cur.bc[[i]] <- predict(predictors, bc[[i]])
    cur.bc.bin[[i]] <- cur.bc[[i]] > bcthres[[i]]
    CSM2_370.bc[[i]] <- predict(CSM2_370, bc[[i]])
    CSM2_370.bc.bin[[i]] <- CSM2_370.bc[[i]] > bcthres[[i]]
    IPSL_370.bc[[i]] <- predict(IPSL_370, bc[[i]])
    IPSL_370.bc.bin[[i]] <- IPSL_370.bc[[i]] > bcthres[[i]]
    MIROC6_370.bc[[i]] <- predict(MIROC6_370, bc[[i]])
    MIROC6_370.bc.bin[[i]] <- MIROC6_370.bc[[i]] > bcthres[[i]]
    CSM2_585.bc[[i]] <- predict(CSM2_585, bc[[i]])
    CSM2_585.bc.bin[[i]] <- CSM2_585.bc[[i]] > bcthres[[i]]
    IPSL_585.bc[[i]] <- predict(IPSL_585, bc[[i]])
    IPSL_585.bc.bin[[i]] <- IPSL_585.bc[[i]] > bcthres[[i]]
    MIROC6_585.bc[[i]] <- predict(MIROC6_585, bc[[i]])
    MIROC6_585.bc.bin[[i]] <- MIROC6_585.bc[[i]] > bcthres[[i]]

  } else {
    cur.bc[[i]] <- NULL
    cur.bc.bin[[i]] <- NULL
    CSM2_370.bc[[i]] <- NULL
    CSM2_370.bc.bin[[i]] <- NULL
    IPSL_370.bc[[i]] <- NULL
    IPSL_370.bc.bin[[i]] <- NULL
    MIROC6_370.bc[[i]] <- NULL
    MIROC6_370.bc.bin[[i]] <- NULL
    CSM2_585.bc[[i]] <- NULL
    CSM2_585.bc.bin[[i]] <- NULL
    IPSL_585.bc[[i]] <- NULL
    IPSL_585.bc.bin[[i]] <- NULL
    MIROC6_585.bc[[i]] <- NULL
    MIROC6_585.bc.bin[[i]] <- NULL
  }
}


##########
cur.bc <- Filter(Negate(is.null), cur.bc) 
cur.bc.bin <- Filter(Negate(is.null), cur.bc.bin) 

#Ensemble of binary models 
cur.bc.ens <- Reduce('+', cur.bc.bin) 
tval <- unique(cur.bc.ens) 
tval <- tval[tval != 0] 
tval <- median(tval) 
cur.bc.ens.bin <- cur.bc.ens >= tval 


##########
CSM2_370.bc <- Filter(Negate(is.null), CSM2_370.bc) 
CSM2_370.bc.bin <- Filter(Negate(is.null), CSM2_370.bc.bin) 

CSM2_370.bc.ens <- Reduce('+', CSM2_370.bc.bin)
tval <- unique(CSM2_370.bc.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_370.bc.ens.bin <- CSM2_370.bc.ens >= tval


##########
IPSL_370.bc <- Filter(Negate(is.null), IPSL_370.bc) 
IPSL_370.bc.bin <- Filter(Negate(is.null), IPSL_370.bc.bin) 

IPSL_370.bc.ens <- Reduce('+', IPSL_370.bc.bin)
tval <- unique(IPSL_370.bc.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_370.bc.ens.bin <- IPSL_370.bc.ens >= tval


##########
MIROC6_370.bc <- Filter(Negate(is.null), MIROC6_370.bc) 
MIROC6_370.bc.bin <- Filter(Negate(is.null), MIROC6_370.bc.bin) 

MIROC6_370.bc.ens <- Reduce('+', MIROC6_370.bc.bin)
tval <- unique(MIROC6_370.bc.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_370.bc.ens.bin <- MIROC6_370.bc.ens >= tval


##########
CSM2_585.bc <- Filter(Negate(is.null), CSM2_585.bc) 
CSM2_585.bc.bin <- Filter(Negate(is.null), CSM2_585.bc.bin) 

CSM2_585.bc.ens <- Reduce('+', CSM2_585.bc.bin)
tval <- unique(CSM2_585.bc.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_585.bc.ens.bin <- CSM2_585.bc.ens >= tval


##########
IPSL_585.bc <- Filter(Negate(is.null), IPSL_585.bc) 
IPSL_585.bc.bin <- Filter(Negate(is.null), IPSL_585.bc.bin) 

IPSL_585.bc.ens <- Reduce('+', IPSL_585.bc.bin)
tval <- unique(IPSL_585.bc.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_585.bc.ens.bin <- IPSL_585.bc.ens >= tval


##########
MIROC6_585.bc <- Filter(Negate(is.null), MIROC6_585.bc) 
MIROC6_585.bc.bin <- Filter(Negate(is.null), MIROC6_585.bc.bin) 

MIROC6_585.bc.ens <- Reduce('+', MIROC6_585.bc.bin)
tval <- unique(MIROC6_585.bc.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_585.bc.ens.bin <- MIROC6_585.bc.ens >= tval

##########

# Taking out the zero values and normalizing the rasters

for(z in 1:length(cur.bc)){
  adeq = cur.bc[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  cur.bc[[z]] <- calc(adeq, adeq_norm)
}



for(z in 1:length(CSM2_370.bc)){
  adeq = CSM2_370.bc[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  CSM2_370.bc[[z]] <- calc(adeq, adeq_norm)
}



for(z in 1:length(IPSL_370.bc)){
  adeq = IPSL_370.bc[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  IPSL_370.bc[[z]] <- calc(adeq, adeq_norm)
}



for(z in 1:length(MIROC6_370.bc)){
  adeq = MIROC6_370.bc[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  MIROC6_370.bc[[z]] <- calc(adeq, adeq_norm)
}



for(z in 1:length(CSM2_585.bc)){
  adeq = CSM2_585.bc[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  CSM2_585.bc[[z]] <- calc(adeq, adeq_norm)
}



for(z in 1:length(IPSL_585.bc)){
  adeq = IPSL_585.bc[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  IPSL_585.bc[[z]] <- calc(adeq, adeq_norm)
}



for(z in 1:length(MIROC6_585.bc)){
  adeq = MIROC6_585.bc[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  MIROC6_585.bc[[z]] <- calc(adeq, adeq_norm)
}

#Ensemble of continuous models

x <- stack(cur.bc)
cur.bc.cont <- calc(x, fun = mean)

x <- stack(CSM2_370.bc)
CSM2_370.bc.cont <- calc(x, fun = mean)

x <- stack(IPSL_370.bc)
IPSL_370.bc.cont <- calc(x, fun = mean)

x <- stack(MIROC6_370.bc)
MIROC6_370.bc.cont <- calc(x, fun = mean)

x <- stack(CSM2_585.bc)
CSM2_585.bc.cont <- calc(x, fun = mean)

x <- stack(IPSL_585.bc)
IPSL_585.bc.cont <- calc(x, fun = mean)

x <- stack(MIROC6_585.bc)
MIROC6_585.bc.cont <- calc(x, fun = mean)



# GLM --------------------------------------
# All TSS values <0.7

# cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting GLMs of', sp.n, '...', '\n')
# cur.gm <- list()
# cur.gm.bin <- list()
# CSM2_370.gm <- list()
# CSM2_370.gm.bin <- list()
# IPSL_370.gm <- list()
# IPSL_370.gm.bin <- list()
# MIROC6_370.gm <- list()
# MIROC6_370.gm.bin <- list()
# CSM2_585.gm <- list()
# CSM2_585.gm.bin <- list()
# IPSL_585.gm <- list()
# IPSL_585.gm.bin <- list()
# MIROC6_585.gm <- list()
# MIROC6_585.gm.bin <- list()
# 
# 
# for(i in 1:k){
#   cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting GLMs (', i, ') of', sp.n, '...', '\n')
#   if(gmTSS[[i]] >= tss.lim){
#     cur.gm[[i]] <- predict(predictors, gm[[i]])
#     cur.gm.bin[[i]] <- cur.gm[[i]] > gmthres[[i]]
#     CSM2_370.gm[[i]] <- predict(CSM2_370, gm[[i]])
#     CSM2_370.gm.bin[[i]] <- CSM2_370.gm[[i]] > gmthres[[i]]
#     IPSL_370.gm[[i]] <- predict(IPSL_370, gm[[i]])
#     IPSL_370.gm.bin[[i]] <- IPSL_370.gm[[i]] > gmthres[[i]]
#     MIROC6_370.gm[[i]] <- predict(MIROC6_370, gm[[i]])
#     MIROC6_370.gm.bin[[i]] <- MIROC6_370.gm[[i]] > gmthres[[i]]
#     CSM2_585.gm[[i]] <- predict(CSM2_585, gm[[i]])
#     CSM2_585.gm.bin[[i]] <- CSM2_585.gm[[i]] > gmthres[[i]]
#     IPSL_585.gm[[i]] <- predict(IPSL_585, gm[[i]])
#     IPSL_585.gm.bin[[i]] <- IPSL_585.gm[[i]] > gmthres[[i]]
#     MIROC6_585.gm[[i]] <- predict(MIROC6_585, gm[[i]])
#     MIROC6_585.gm.bin[[i]] <- MIROC6_585.gm[[i]] > gmthres[[i]]
#     
#   } else {
#     cur.gm[[i]] <- NULL
#     cur.gm.bin[[i]] <- NULL
#     CSM2_370.gm[[i]] <- NULL
#     CSM2_370.gm.bin[[i]] <- NULL
#     IPSL_370.gm[[i]] <- NULL
#     IPSL_370.gm.bin[[i]] <- NULL
#     MIROC6_370.gm[[i]] <- NULL
#     MIROC6_370.gm.bin[[i]] <- NULL
#     CSM2_585.gm[[i]] <- NULL
#     CSM2_585.gm.bin[[i]] <- NULL
#     IPSL_585.gm[[i]] <- NULL
#     IPSL_585.gm.bin[[i]] <- NULL
#     MIROC6_585.gm[[i]] <- NULL
#     MIROC6_585.gm.bin[[i]] <- NULL
#     
#   }
#   
# }
# 
# ########## 
# cur.gm <- Filter(Negate(is.null), cur.gm) 
# cur.gm.bin <- Filter(Negate(is.null), cur.gm.bin) 
# 
# cur.gm.ens <- Reduce('+', cur.gm.bin)
# tval <- unique(cur.gm.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# cur.gm.ens.bin <- cur.gm.ens >= tval
# 
# ########## 
# CSM2_370.gm <- Filter(Negate(is.null), CSM2_370.gm) 
# CSM2_370.gm.bin <- Filter(Negate(is.null), CSM2_370.gm.bin) 
# 
# CSM2_370.gm.ens <- Reduce('+', CSM2_370.gm.bin)
# tval <- unique(CSM2_370.gm.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# CSM2_370.gm.ens.bin <- CSM2_370.gm.ens >= tval
# 
# ########## 
# IPSL_370.gm <- Filter(Negate(is.null), IPSL_370.gm) 
# IPSL_370.gm.bin <- Filter(Negate(is.null), IPSL_370.gm.bin) 
# 
# IPSL_370.gm.ens <- Reduce('+', IPSL_370.gm.bin)
# tval <- unique(IPSL_370.gm.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# IPSL_370.gm.ens.bin <- IPSL_370.gm.ens >= tval
# 
# ########## 
# MIROC6_370.gm <- Filter(Negate(is.null), MIROC6_370.gm) 
# MIROC6_370.gm.bin <- Filter(Negate(is.null), MIROC6_370.gm.bin) 
# 
# MIROC6_370.gm.ens <- Reduce('+', MIROC6_370.gm.bin)
# tval <- unique(MIROC6_370.gm.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# MIROC6_370.gm.ens.bin <- MIROC6_370.gm.ens >= tval
# 
# ########## 
# CSM2_585.gm <- Filter(Negate(is.null), CSM2_585.gm) 
# CSM2_585.gm.bin <- Filter(Negate(is.null), CSM2_585.gm.bin) 
# 
# CSM2_585.gm.ens <- Reduce('+', CSM2_585.gm.bin)
# tval <- unique(CSM2_585.gm.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# CSM2_585.gm.ens.bin <- CSM2_585.gm.ens >= tval
# 
# ########## 
# IPSL_585.gm <- Filter(Negate(is.null), IPSL_585.gm) 
# IPSL_585.gm.bin <- Filter(Negate(is.null), IPSL_585.gm.bin) 
# 
# IPSL_585.gm.ens <- Reduce('+', IPSL_585.gm.bin)
# tval <- unique(IPSL_585.gm.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# IPSL_585.gm.ens.bin <- IPSL_585.gm.ens >= tval
# 
# ########## 
# MIROC6_585.gm <- Filter(Negate(is.null), MIROC6_585.gm) 
# MIROC6_585.gm.bin <- Filter(Negate(is.null), MIROC6_585.gm.bin) 
# 
# MIROC6_585.gm.ens <- Reduce('+', MIROC6_585.gm.bin)
# tval <- unique(MIROC6_585.gm.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# MIROC6_585.gm.ens.bin <- MIROC6_585.gm.ens >= tval
# 
# ########## 
# 
# for(z in 1:length(cur.gm)){
#   adeq = cur.gm[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   cur.gm[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# for(z in 1:length(CSM2_370.gm)){
#   adeq = CSM2_370.gm[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   CSM2_370.gm[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# for(z in 1:length(IPSL_370.gm)){
#   adeq = IPSL_370.gm[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   IPSL_370.gm[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# for(z in 1:length(MIROC6_370.gm)){
#   adeq = MIROC6_370.gm[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   MIROC6_370.gm[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# for(z in 1:length(CSM2_585.gm)){
#   adeq = CSM2_585.gm[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   CSM2_585.gm[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# for(z in 1:length(IPSL_585.gm)){
#   adeq = IPSL_585.gm[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   IPSL_585.gm[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# for(z in 1:length(MIROC6_585.gm)){
#   adeq = MIROC6_585.gm[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   MIROC6_585.gm[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# x <- stack(cur.gm)
# cur.gm.cont <- calc(x, fun = mean)
# 
# x <- stack(CSM2_370.gm)
# CSM2_370.gm.cont <- calc(x, fun = mean)
# 
# x <- stack(IPSL_370.gm)
# IPSL_370.gm.cont <- calc(x, fun = mean)
# 
# x <- stack(MIROC6_370.gm)
# MIROC6_370.gm.cont <- calc(x, fun = mean)
# 
# x <- stack(CSM2_585.gm)
# CSM2_585.gm.cont <- calc(x, fun = mean)
# 
# x <- stack(IPSL_585.gm)
# IPSL_585.gm.cont <- calc(x, fun = mean)
# 
# x <- stack(MIROC6_585.gm)
# MIROC6_585.gm.cont <- calc(x, fun = mean)



# RANDOM FOREST --------------------------------------

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Random Forest models of', sp.n, '...', '\n')
cur.rf <- list()
cur.rf.bin <- list()
CSM2_370.rf <- list()
CSM2_370.rf.bin <- list()
IPSL_370.rf <- list()
IPSL_370.rf.bin <- list()
MIROC6_370.rf <- list()
MIROC6_370.rf.bin <- list()
CSM2_585.rf <- list()
CSM2_585.rf.bin <- list()
IPSL_585.rf <- list()
IPSL_585.rf.bin <- list()
MIROC6_585.rf <- list()
MIROC6_585.rf.bin <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Random Forest (', i, ') models of', sp.n, '...', '\n')
  if(rfTSS[[i]] >= tss.lim){
    cur.rf[[i]] <- predict(predictors, rf[[i]])
    cur.rf.bin[[i]] <- cur.rf[[i]] > rfthres[[i]]
    
    CSM2_370.rf[[i]] <- predict(CSM2_370, rf[[i]])
    CSM2_370.rf.bin[[i]] <- CSM2_370.rf[[i]] > rfthres[[i]]
    IPSL_370.rf[[i]] <- predict(IPSL_370, rf[[i]])
    IPSL_370.rf.bin[[i]] <- IPSL_370.rf[[i]] > rfthres[[i]]
    MIROC6_370.rf[[i]] <- predict(MIROC6_370, rf[[i]])
    MIROC6_370.rf.bin[[i]] <- MIROC6_370.rf[[i]] > rfthres[[i]]
    CSM2_585.rf[[i]] <- predict(CSM2_585, rf[[i]])
    CSM2_585.rf.bin[[i]] <- CSM2_585.rf[[i]] > rfthres[[i]]
    IPSL_585.rf[[i]] <- predict(IPSL_585, rf[[i]])
    IPSL_585.rf.bin[[i]] <- IPSL_585.rf[[i]] > rfthres[[i]]
    MIROC6_585.rf[[i]] <- predict(MIROC6_585, rf[[i]])
    MIROC6_585.rf.bin[[i]] <- MIROC6_585.rf[[i]] > rfthres[[i]]
    
  } else {
    
    cur.rf[[i]] <- NULL
    cur.rf.bin[[i]] <- NULL
    CSM2_370.rf[[i]] <- NULL
    CSM2_370.rf.bin[[i]] <- NULL
    IPSL_370.rf[[i]] <- NULL
    IPSL_370.rf.bin[[i]] <- NULL
    MIROC6_370.rf[[i]] <- NULL
    MIROC6_370.rf.bin[[i]] <- NULL
    CSM2_585.rf[[i]] <- NULL
    CSM2_585.rf.bin[[i]] <- NULL
    IPSL_585.rf[[i]] <- NULL
    IPSL_585.rf.bin[[i]] <- NULL
    MIROC6_585.rf[[i]] <- NULL
    MIROC6_585.rf.bin[[i]] <- NULL
    
  }
}

########## 
cur.rf <- Filter(Negate(is.null), cur.rf) 
cur.rf.bin <- Filter(Negate(is.null), cur.rf.bin) 

cur.rf.ens <- Reduce('+', cur.rf.bin)
tval <- unique(cur.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
cur.rf.ens.bin <- cur.rf.ens >= tval


########## 
CSM2_370.rf <- Filter(Negate(is.null), CSM2_370.rf) 
CSM2_370.rf.bin <- Filter(Negate(is.null), CSM2_370.rf.bin) 

CSM2_370.rf.ens <- Reduce('+', CSM2_370.rf.bin)
tval <- unique(CSM2_370.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_370.rf.ens.bin <- CSM2_370.rf.ens >= tval


########## 
IPSL_370.rf <- Filter(Negate(is.null), IPSL_370.rf) 
IPSL_370.rf.bin <- Filter(Negate(is.null), IPSL_370.rf.bin) 

IPSL_370.rf.ens <- Reduce('+', IPSL_370.rf.bin)
tval <- unique(IPSL_370.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_370.rf.ens.bin <- IPSL_370.rf.ens >= tval


########## 
MIROC6_370.rf <- Filter(Negate(is.null), MIROC6_370.rf) 
MIROC6_370.rf.bin <- Filter(Negate(is.null), MIROC6_370.rf.bin) 

MIROC6_370.rf.ens <- Reduce('+', MIROC6_370.rf.bin)
tval <- unique(MIROC6_370.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_370.rf.ens.bin <- MIROC6_370.rf.ens >= tval


########## 
CSM2_585.rf <- Filter(Negate(is.null), CSM2_585.rf) 
CSM2_585.rf.bin <- Filter(Negate(is.null), CSM2_585.rf.bin) 

CSM2_585.rf.ens <- Reduce('+', CSM2_585.rf.bin)
tval <- unique(CSM2_585.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_585.rf.ens.bin <- CSM2_585.rf.ens >= tval


########## 
IPSL_585.rf <- Filter(Negate(is.null), IPSL_585.rf) 
IPSL_585.rf.bin <- Filter(Negate(is.null), IPSL_585.rf.bin) 

IPSL_585.rf.ens <- Reduce('+', IPSL_585.rf.bin)
tval <- unique(IPSL_585.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_585.rf.ens.bin <- IPSL_585.rf.ens >= tval


########## 
MIROC6_585.rf <- Filter(Negate(is.null), MIROC6_585.rf) 
MIROC6_585.rf.bin <- Filter(Negate(is.null), MIROC6_585.rf.bin) 

MIROC6_585.rf.ens <- Reduce('+', MIROC6_585.rf.bin)
tval <- unique(MIROC6_585.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_585.rf.ens.bin <- MIROC6_585.rf.ens >= tval

########## 

for(z in 1:length(cur.rf)){
  adeq = cur.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    cur.rf[[z]] <- calc(adeq, adeq_norm)
  }
  
}


for(z in 1:length(CSM2_370.rf)){
  adeq = CSM2_370.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    CSM2_370.rf[[z]] <- calc(adeq, adeq_norm)
  }
}


for(z in 1:length(IPSL_370.rf)){
  adeq = IPSL_370.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    IPSL_370.rf[[z]] <- calc(adeq, adeq_norm)
  }
}


for(z in 1:length(MIROC6_370.rf)){
  adeq = MIROC6_370.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    MIROC6_370.rf[[z]] <- calc(adeq, adeq_norm)
  }
}


for(z in 1:length(CSM2_585.rf)){
  adeq = CSM2_585.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    CSM2_585.rf[[z]] <- calc(adeq, adeq_norm)
  }
}


for(z in 1:length(IPSL_585.rf)){
  adeq = IPSL_585.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    IPSL_585.rf[[z]] <- calc(adeq, adeq_norm)
  }
}


for(z in 1:length(MIROC6_585.rf)){
  adeq = MIROC6_585.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    MIROC6_585.rf[[z]] <- calc(adeq, adeq_norm)
  }
}


x <- stack(cur.rf)
cur.rf.cont <- calc(x, fun = mean)

x <- stack(CSM2_370.rf)
CSM2_370.rf.cont <- calc(x, fun = mean)

x <- stack(IPSL_370.rf)
IPSL_370.rf.cont <- calc(x, fun = mean)

x <- stack(MIROC6_370.rf)
MIROC6_370.rf.cont <- calc(x, fun = mean)

x <- stack(CSM2_585.rf)
CSM2_585.rf.cont <- calc(x, fun = mean)

x <- stack(IPSL_585.rf)
IPSL_585.rf.cont <- calc(x, fun = mean)

x <- stack(MIROC6_585.rf)
MIROC6_585.rf.cont <- calc(x, fun = mean)




# MAXENT --------------------------------------

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Maxent models of', sp.n, '...', '\n')
cur.mx <- list()
cur.mx.bin <- list()
CSM2_370.mx <- list()
CSM2_370.mx.bin <- list()
IPSL_370.mx <- list()
IPSL_370.mx.bin <- list()
MIROC6_370.mx <- list()
MIROC6_370.mx.bin <- list()
CSM2_585.mx <- list()
CSM2_585.mx.bin <- list()
IPSL_585.mx <- list()
IPSL_585.mx.bin <- list()
MIROC6_585.mx <- list()
MIROC6_585.mx.bin <- list()


for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Maxent (', i, ') models of', sp.n, '...', '\n')
  if(mxTSS[[i]] >= tss.lim){
    cur.mx[[i]] <- predict(predictors, mx[[i]])
    cur.mx.bin[[i]] <- cur.mx[[i]] > mxthres[[i]]
    
    CSM2_370.mx[[i]] <- predict(CSM2_370, mx[[i]])
    CSM2_370.mx.bin[[i]] <- CSM2_370.mx[[i]] > mxthres[[i]]
    IPSL_370.mx[[i]] <- predict(IPSL_370, mx[[i]])
    IPSL_370.mx.bin[[i]] <- IPSL_370.mx[[i]] > mxthres[[i]]
    MIROC6_370.mx[[i]] <- predict(MIROC6_370, mx[[i]])
    MIROC6_370.mx.bin[[i]] <- MIROC6_370.mx[[i]] > mxthres[[i]]
    CSM2_585.mx[[i]] <- predict(CSM2_585, mx[[i]])
    CSM2_585.mx.bin[[i]] <- CSM2_585.mx[[i]] > mxthres[[i]]
    IPSL_585.mx[[i]] <- predict(IPSL_585, mx[[i]])
    IPSL_585.mx.bin[[i]] <- IPSL_585.mx[[i]] > mxthres[[i]]
    MIROC6_585.mx[[i]] <- predict(MIROC6_585, mx[[i]])
    MIROC6_585.mx.bin[[i]] <- MIROC6_585.mx[[i]] > mxthres[[i]]
    
    
  } else {
    cur.mx[[i]] <- NULL
    cur.mx.bin[[i]] <- NULL
    CSM2_370.mx[[i]] <- NULL
    CSM2_370.mx.bin[[i]] <- NULL
    IPSL_370.mx[[i]] <- NULL
    IPSL_370.mx.bin[[i]] <- NULL
    MIROC6_370.mx[[i]] <- NULL
    MIROC6_370.mx.bin[[i]] <- NULL
    CSM2_585.mx[[i]] <- NULL
    CSM2_585.mx.bin[[i]] <- NULL
    IPSL_585.mx[[i]] <- NULL
    IPSL_585.mx.bin[[i]] <- NULL
    MIROC6_585.mx[[i]] <- NULL
    MIROC6_585.mx.bin[[i]] <- NULL
    
  }
}


########## 
cur.mx <- Filter(Negate(is.null), cur.mx) 
cur.mx.bin <- Filter(Negate(is.null), cur.mx.bin) 

cur.mx.ens <- Reduce('+', cur.mx.bin)
tval <- unique(cur.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
cur.mx.ens.bin <- cur.mx.ens >= tval


########## 
CSM2_370.mx <- Filter(Negate(is.null), CSM2_370.mx) 
CSM2_370.mx.bin <- Filter(Negate(is.null), CSM2_370.mx.bin) 

CSM2_370.mx.ens <- Reduce('+', CSM2_370.mx.bin)
tval <- unique(CSM2_370.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_370.mx.ens.bin <- CSM2_370.mx.ens >= tval


########## 
IPSL_370.mx <- Filter(Negate(is.null), IPSL_370.mx) 
IPSL_370.mx.bin <- Filter(Negate(is.null), IPSL_370.mx.bin) 

IPSL_370.mx.ens <- Reduce('+', IPSL_370.mx.bin)
tval <- unique(IPSL_370.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_370.mx.ens.bin <- IPSL_370.mx.ens >= tval


########## 
MIROC6_370.mx <- Filter(Negate(is.null), MIROC6_370.mx) 
MIROC6_370.mx.bin <- Filter(Negate(is.null), MIROC6_370.mx.bin) 

MIROC6_370.mx.ens <- Reduce('+', MIROC6_370.mx.bin)
tval <- unique(MIROC6_370.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_370.mx.ens.bin <- MIROC6_370.mx.ens >= tval


########## 
CSM2_585.mx <- Filter(Negate(is.null), CSM2_585.mx) 
CSM2_585.mx.bin <- Filter(Negate(is.null), CSM2_585.mx.bin) 

CSM2_585.mx.ens <- Reduce('+', CSM2_585.mx.bin)
tval <- unique(CSM2_585.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_585.mx.ens.bin <- CSM2_585.mx.ens >= tval


########## 
IPSL_585.mx <- Filter(Negate(is.null), IPSL_585.mx) 
IPSL_585.mx.bin <- Filter(Negate(is.null), IPSL_585.mx.bin) 

IPSL_585.mx.ens <- Reduce('+', IPSL_585.mx.bin)
tval <- unique(IPSL_585.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_585.mx.ens.bin <- IPSL_585.mx.ens >= tval


########## 
MIROC6_585.mx <- Filter(Negate(is.null), MIROC6_585.mx) 
MIROC6_585.mx.bin <- Filter(Negate(is.null), MIROC6_585.mx.bin) 

MIROC6_585.mx.ens <- Reduce('+', MIROC6_585.mx.bin)
tval <- unique(MIROC6_585.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_585.mx.ens.bin <- MIROC6_585.mx.ens >= tval


##########


for(z in 1:length(cur.mx)){
  adeq = cur.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    cur.mx[[z]] <- calc(adeq, adeq_norm)
  }
}

for(z in 1:length(CSM2_370.mx)){
  adeq = CSM2_370.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    CSM2_370.mx[[z]] <- calc(adeq, adeq_norm)
  }
}

for(z in 1:length(IPSL_370.mx)){
  adeq = IPSL_370.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    IPSL_370.mx[[z]] <- calc(adeq, adeq_norm)
  }
}

for(z in 1:length(MIROC6_370.mx)){
  adeq = MIROC6_370.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    MIROC6_370.mx[[z]] <- calc(adeq, adeq_norm)
  }
}


for(z in 1:length(CSM2_585.mx)){
  adeq = CSM2_585.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    CSM2_585.mx[[z]] <- calc(adeq, adeq_norm)
  }
}

for(z in 1:length(IPSL_585.mx)){
  adeq = IPSL_585.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    IPSL_585.mx[[z]] <- calc(adeq, adeq_norm)
  }
}

for(z in 1:length(MIROC6_585.mx)){
  adeq = MIROC6_585.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    MIROC6_585.mx[[z]] <- calc(adeq, adeq_norm)
  }
}


x <- stack(cur.mx)
cur.mx.cont <- calc(x, fun = mean)

x <- stack(CSM2_370.mx)
CSM2_370.mx.cont <- calc(x, fun = mean)

x <- stack(IPSL_370.mx)
IPSL_370.mx.cont <- calc(x, fun = mean)

x <- stack(MIROC6_370.mx)
MIROC6_370.mx.cont <- calc(x, fun = mean)

x <- stack(CSM2_585.mx)
CSM2_585.mx.cont <- calc(x, fun = mean)

x <- stack(IPSL_585.mx)
IPSL_585.mx.cont <- calc(x, fun = mean)

x <- stack(MIROC6_585.mx)
MIROC6_585.mx.cont <- calc(x, fun = mean)



# SVM --------------------------------------

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting SVM models of', sp.n, '...', '\n')
cur.sv <- list()
cur.sv.bin <- list()
CSM2_370.sv <- list()
CSM2_370.sv.bin <- list()
IPSL_370.sv <- list()
IPSL_370.sv.bin <- list()
MIROC6_370.sv <- list()
MIROC6_370.sv.bin <- list()
CSM2_585.sv <- list()
CSM2_585.sv.bin <- list()
IPSL_585.sv <- list()
IPSL_585.sv.bin <- list()
MIROC6_585.sv <- list()
MIROC6_585.sv.bin <- list()


for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting SVM (', i, ') models of', sp.n, '...', '\n')
  if(svTSS[[i]] >= tss.lim){
    cur.sv[[i]] <- predict(predictors, sv[[i]])
    cur.sv.bin[[i]] <- cur.sv[[i]] > svthres[[i]]
    
    CSM2_370.sv[[i]] <- predict(CSM2_370, sv[[i]])
    CSM2_370.sv.bin[[i]] <- CSM2_370.sv[[i]] > svthres[[i]]
    IPSL_370.sv[[i]] <- predict(IPSL_370, sv[[i]])
    IPSL_370.sv.bin[[i]] <- IPSL_370.sv[[i]] > svthres[[i]]
    MIROC6_370.sv[[i]] <- predict(MIROC6_370, sv[[i]])
    MIROC6_370.sv.bin[[i]] <- MIROC6_370.sv[[i]] > svthres[[i]]
    CSM2_585.sv[[i]] <- predict(CSM2_585, sv[[i]])
    CSM2_585.sv.bin[[i]] <- CSM2_585.sv[[i]] > svthres[[i]]
    IPSL_585.sv[[i]] <- predict(IPSL_585, sv[[i]])
    IPSL_585.sv.bin[[i]] <- IPSL_585.sv[[i]] > svthres[[i]]
    MIROC6_585.sv[[i]] <- predict(MIROC6_585, sv[[i]])
    MIROC6_585.sv.bin[[i]] <- MIROC6_585.sv[[i]] > svthres[[i]]
    
  } else {
    cur.sv[[i]] <- NULL
    cur.sv.bin[[i]] <- NULL
    CSM2_370.sv[[i]] <- NULL
    CSM2_370.sv.bin[[i]] <- NULL
    IPSL_370.sv[[i]] <- NULL
    IPSL_370.sv.bin[[i]] <- NULL
    MIROC6_370.sv[[i]] <- NULL
    MIROC6_370.sv.bin[[i]] <- NULL
    CSM2_585.sv[[i]] <- NULL
    CSM2_585.sv.bin[[i]] <- NULL
    IPSL_585.sv[[i]] <- NULL
    IPSL_585.sv.bin[[i]] <- NULL
    MIROC6_585.sv[[i]] <- NULL
    MIROC6_585.sv.bin[[i]] <- NULL
    
  }
  
}


########## 
cur.sv <- Filter(Negate(is.null), cur.sv) 
cur.sv.bin <- Filter(Negate(is.null), cur.sv.bin) 

cur.sv.ens <- Reduce('+', cur.sv.bin)
tval <- unique(cur.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
cur.sv.ens.bin <- cur.sv.ens >= tval


########## 
CSM2_370.sv <- Filter(Negate(is.null), CSM2_370.sv) 
CSM2_370.sv.bin <- Filter(Negate(is.null), CSM2_370.sv.bin) 

CSM2_370.sv.ens <- Reduce('+', CSM2_370.sv.bin)
tval <- unique(CSM2_370.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_370.sv.ens.bin <- CSM2_370.sv.ens >= tval


########## 
IPSL_370.sv <- Filter(Negate(is.null), IPSL_370.sv) 
IPSL_370.sv.bin <- Filter(Negate(is.null), IPSL_370.sv.bin) 

IPSL_370.sv.ens <- Reduce('+', IPSL_370.sv.bin)
tval <- unique(IPSL_370.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_370.sv.ens.bin <- IPSL_370.sv.ens >= tval


########## 
MIROC6_370.sv <- Filter(Negate(is.null), MIROC6_370.sv) 
MIROC6_370.sv.bin <- Filter(Negate(is.null), MIROC6_370.sv.bin) 

MIROC6_370.sv.ens <- Reduce('+', MIROC6_370.sv.bin)
tval <- unique(MIROC6_370.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_370.sv.ens.bin <- MIROC6_370.sv.ens >= tval


########## 
CSM2_585.sv <- Filter(Negate(is.null), CSM2_585.sv) 
CSM2_585.sv.bin <- Filter(Negate(is.null), CSM2_585.sv.bin) 

CSM2_585.sv.ens <- Reduce('+', CSM2_585.sv.bin)
tval <- unique(CSM2_585.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CSM2_585.sv.ens.bin <- CSM2_585.sv.ens >= tval


########## 
IPSL_585.sv <- Filter(Negate(is.null), IPSL_585.sv) 
IPSL_585.sv.bin <- Filter(Negate(is.null), IPSL_585.sv.bin) 

IPSL_585.sv.ens <- Reduce('+', IPSL_585.sv.bin)
tval <- unique(IPSL_585.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
IPSL_585.sv.ens.bin <- IPSL_585.sv.ens >= tval


########## 
MIROC6_585.sv <- Filter(Negate(is.null), MIROC6_585.sv) 
MIROC6_585.sv.bin <- Filter(Negate(is.null), MIROC6_585.sv.bin) 

MIROC6_585.sv.ens <- Reduce('+', MIROC6_585.sv.bin)
tval <- unique(MIROC6_585.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
MIROC6_585.sv.ens.bin <- MIROC6_585.sv.ens >= tval

########## 

for(z in 1:length(cur.sv)){
  adeq = cur.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  cur.sv[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(CSM2_370.sv)){
  adeq = CSM2_370.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  CSM2_370.sv[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(IPSL_370.sv)){
  adeq = IPSL_370.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  IPSL_370.sv[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(MIROC6_370.sv)){
  adeq = MIROC6_370.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  MIROC6_370.sv[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(CSM2_585.sv)){
  adeq = CSM2_585.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  CSM2_585.sv[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(IPSL_585.sv)){
  adeq = IPSL_585.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  IPSL_585.sv[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(MIROC6_585.sv)){
  adeq = MIROC6_585.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  MIROC6_585.sv[[z]] <- calc(adeq, adeq_norm)
}


x <- stack(cur.sv)
cur.sv.cont <- calc(x, fun = mean)

x <- stack(CSM2_370.sv)
CSM2_370.sv.cont <- calc(x, fun = mean)

x <- stack(IPSL_370.sv)
IPSL_370.sv.cont <- calc(x, fun = mean)

x <- stack(MIROC6_370.sv)
MIROC6_370.sv.cont <- calc(x, fun = mean)

x <- stack(CSM2_585.sv)
CSM2_585.sv.cont <- calc(x, fun = mean)

x <- stack(IPSL_585.sv)
IPSL_585.sv.cont <- calc(x, fun = mean)

x <- stack(MIROC6_585.sv)
MIROC6_585.sv.cont <- calc(x, fun = mean)



# Ensemble --------------------------------------


# Ensemble Current --------------------------------------

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting ensemble models of', sp.n, '...', '\n')
#algo.cur <- list(cur.bc.ens.bin, cur.gm.ens.bin, cur.rf.ens.bin, cur.mx.ens.bin, cur.sv.ens.bin)
algo.cur <- list(cur.bc.ens.bin, cur.rf.ens.bin, cur.mx.ens.bin, cur.sv.ens.bin)
algo.cur <- algo.cur[is.na(algo.cur) == FALSE]
ens.cur <- Reduce('+', algo.cur)
tval <- unique(ens.cur)
tval <- tval[tval != 0]
tval <- median(tval)
ens.cur.bin <- ens.cur >= tval



# continuous - TSS weighted average
# w <- c(bcTSSval, gmTSSval, rfTSSval, mxTSSval, svTSSval)
w <- c(bcTSSval, rfTSSval, mxTSSval, svTSSval)

st1 <- stack(cur.bc)
for(i in 1:length(cur.bc)){
  min_max <- range(cur.bc[[i]][], na.rm=T)
  write.table(min_max, paste("cur.bc", i, sp.n, ".txt",  sep="_"))
}


# st2 <- stack(cur.gm)
# for(i in 1:length(cur.gm)){
#   min_max <- range(cur.gm[[i]][], na.rm=T)
#   write.table(min_max, paste("cur.gm", i, sp.n, ".txt",  sep="_"))
# }


st3 <- stack(cur.rf)
for(i in 1:length(cur.rf)){
  min_max <- range(cur.rf[[i]][], na.rm=T)
  write.table(min_max, paste("cur.rf", i, sp.n, ".txt",  sep="_"))
}


st4 <- stack(cur.mx)
for(i in 1:length(cur.mx)){
  min_max <- range(cur.mx[[i]][], na.rm=T)
  write.table(min_max, paste("cur.mx", i, sp.n, ".txt",  sep="_"))
}


st5 <- stack(cur.sv)
for(i in 1:length(cur.sv)){
  min_max <- range(cur.sv[[i]][], na.rm=T)
  write.table(min_max, paste("cur.sv", i, sp.n, ".txt",  sep="_"))
}


# st <- stack(st1, st2, st3, st4, st5)
st <- stack(st1, st3, st4, st5)
w.without.null <- w[w >= tss.lim]
ens2.cur <- weighted.mean(st, w.without.null)
ens2.cur.sd.w <- sum(w.without.null * (st - ens2.cur)^2)
ens2.cur.sd.w <- sqrt(ens2.cur.sd.w)


# Uncertainty -------------------------------------------------------------

wbc <- c(bcTSSval)
wbc.without.null <- wbc[wbc >= tss.lim]
cur.bc.mean.w <- weighted.mean(st1, wbc.without.null)
cur.bc.sd.w <- sum(wbc.without.null * (st1 - cur.bc.mean.w)^2)
cur.bc.sd.w <- sqrt(cur.bc.sd.w)

# wgm <- c(gmTSSval)
# wgm.without.null <- wgm[wgm >= tss.lim]
# cur.gm.mean.w <- weighted.mean(st2, wgm.without.null)
# cur.gm.sd.w <- sum(wgm.without.null * (st2 - cur.gm.mean.w)^2)
# cur.gm.sd.w <- sqrt(cur.gm.sd.w)

wrf <- c(rfTSSval)
wrf.without.null <- wrf[wrf >= tss.lim]
cur.rf.mean.w <- weighted.mean(st3, wrf.without.null)
cur.rf.sd.w <- sum(wrf.without.null * (st3 - cur.rf.mean.w)^2)
cur.rf.sd.w <- sqrt(cur.rf.sd.w)

wmx <- c(mxTSSval)
wmx.without.null <- wmx[wmx >= tss.lim]
cur.mx.mean.w <- weighted.mean(st4, wmx.without.null)
cur.mx.sd.w <- sum(wmx.without.null * (st4 - cur.mx.mean.w)^2)
cur.mx.sd.w <- sqrt(cur.mx.sd.w)

wsv <- c(svTSSval)
wsv.without.null <- wsv[wsv >= tss.lim]
cur.sv.mean.w <- weighted.mean(st5, wsv.without.null)
cur.sv.sd.w <- sum(wsv.without.null * (st5 - cur.sv.mean.w)^2)
cur.sv.sd.w <- sqrt(cur.sv.sd.w)

cur.sd.w <- mean(cur.bc.sd.w,cur.rf.sd.w,cur.mx.sd.w,cur.sv.sd.w)

##### 

# Ensemble CSM2_370 --------------------------------------

# binary
# algo.CSM2_370 <- list(CSM2_370.bc.ens.bin, CSM2_370.gm.ens.bin, CSM2_370.rf.ens.bin, CSM2_370.mx.ens.bin, CSM2_370.sv.ens.bin)
algo.CSM2_370 <- list(CSM2_370.bc.ens.bin, CSM2_370.rf.ens.bin, CSM2_370.mx.ens.bin, CSM2_370.sv.ens.bin)
algo.CSM2_370 <- algo.CSM2_370[is.na(algo.CSM2_370) == FALSE]
ens.CSM2_370 <- Reduce('+', algo.CSM2_370)
tval <- unique(ens.CSM2_370)
tval <- tval[tval != 0]
tval <- median(tval)
ens.CSM2_370.bin <- ens.CSM2_370 >= tval


# continuous - TSS weighted average

st1 <- stack(CSM2_370.bc)
for(i in 1:length(CSM2_370.bc)){
  min_max <- range(CSM2_370.bc[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_370.bc", i, sp.n, ".txt",  sep="_"))
}


# st2 <- stack(CSM2_370.gm)
# for(i in 1:length(CSM2_370.gm)){
#   min_max <- range(CSM2_370.gm[[i]][], na.rm=T)
#   write.table(min_max, paste("CSM2_370.gm", i, sp.n, ".txt",  sep="_"))
# }


st3 <- stack(CSM2_370.rf)
for(i in 1:length(CSM2_370.rf)){
  min_max <- range(CSM2_370.rf[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_370.rf", i, sp.n, ".txt",  sep="_"))
}


st4 <- stack(CSM2_370.mx)
for(i in 1:length(CSM2_370.mx)){
  min_max <- range(CSM2_370.mx[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_370.mx", i, sp.n, ".txt",  sep="_"))
}


st5 <- stack(CSM2_370.sv)
for(i in 1:length(CSM2_370.sv)){
  min_max <- range(CSM2_370.sv[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_370.sv", i, sp.n, ".txt",  sep="_"))
}


# st <- stack(st1, st2, st3, st4, st5)
st <- stack(st1, st3, st4, st5)
w.without.null <- w[w >= tss.lim]
ens2.CSM2_370 <- weighted.mean(st, w.without.null)
ens.fut.cont <- ens2.CSM2_370
ens2.fut.sd.w.CSM2_370 <- sum(w.without.null * (st - ens.fut.cont)^2)
ens2.fut.sd.w.CSM2_370 <- sqrt(ens2.fut.sd.w.CSM2_370)


# Uncertainty future ------------------------------------------------------

wbc <- c(bcTSSval)
wbc.without.null <- wbc[wbc >= tss.lim]
CSM2_370.bc.mean.w <- weighted.mean(st1, wbc.without.null)
CSM2_370.bc.sd.w <- sum(wbc.without.null * (st1 - CSM2_370.bc.mean.w)^2)
CSM2_370.bc.sd.w <- sqrt(CSM2_370.bc.sd.w)

# wgm <- c(gmTSSval)
# wgm.without.null <- wgm[wgm >= tss.lim]
# CSM2_370.gm.mean.w <- weighted.mean(st2, wgm.without.null)
# CSM2_370.gm.sd.w <- sum(wgm.without.null * (st2 - CSM2_370.gm.mean.w)^2)
# CSM2_370.gm.sd.w <- sqrt(CSM2_370.gm.sd.w)

wrf <- c(rfTSSval)
wrf.without.null <- wrf[wrf >= tss.lim]
CSM2_370.rf.mean.w <- weighted.mean(st3, wrf.without.null)
CSM2_370.rf.sd.w <- sum(wrf.without.null * (st3 - CSM2_370.rf.mean.w)^2)
CSM2_370.rf.sd.w <- sqrt(CSM2_370.rf.sd.w)

wmx <- c(mxTSSval)
wmx.without.null <- wmx[wmx >= tss.lim]
CSM2_370.mx.mean.w <- weighted.mean(st4, wmx.without.null)
CSM2_370.mx.sd.w <- sum(wmx.without.null * (st4 - CSM2_370.mx.mean.w)^2)
CSM2_370.mx.sd.w <- sqrt(CSM2_370.mx.sd.w)

wsv <- c(svTSSval)
wsv.without.null <- wsv[wsv >= tss.lim]
CSM2_370.sv.mean.w <- weighted.mean(st5, wsv.without.null)
CSM2_370.sv.sd.w <- sum(wsv.without.null * (st5 - CSM2_370.sv.mean.w)^2)
CSM2_370.sv.sd.w <- sqrt(CSM2_370.sv.sd.w)

CSM2_370.sd.w <- mean(CSM2_370.bc.sd.w,CSM2_370.rf.sd.w,CSM2_370.mx.sd.w,
                      CSM2_370.sv.sd.w)

####

ens.fut <- ens.CSM2_370.bin 
tval <- unique(ens.fut)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.CSM2_370 <- ens.fut >= tval

#########

# Ensemble IPSL_370 --------------------------------------
# binary
# algo.IPSL_370 <- list(IPSL_370.bc.ens.bin, IPSL_370.gm.ens.bin, IPSL_370.rf.ens.bin, IPSL_370.mx.ens.bin, IPSL_370.sv.ens.bin)
algo.IPSL_370 <- list(IPSL_370.bc.ens.bin, IPSL_370.rf.ens.bin, IPSL_370.mx.ens.bin, IPSL_370.sv.ens.bin)
algo.IPSL_370 <- algo.IPSL_370[is.na(algo.IPSL_370) == FALSE]
ens.IPSL_370 <- Reduce('+', algo.IPSL_370)
tval <- unique(ens.IPSL_370)
tval <- tval[tval != 0]
tval <- median(tval)
ens.IPSL_370.bin <- ens.IPSL_370 >= tval

# continuous - TSS weighted average

st1 <- stack(IPSL_370.bc)
for(i in 1:length(IPSL_370.bc)){
  min_max <- range(IPSL_370.bc[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_370.bc", i, sp.n, ".txt",  sep="_"))
}


# st2 <- stack(IPSL_370.gm)
# for(i in 1:length(IPSL_370.gm)){
#   min_max <- range(IPSL_370.gm[[i]][], na.rm=T)
#   write.table(min_max, paste("IPSL_370.gm", i, sp.n, ".txt",  sep="_"))
# }


st3 <- stack(IPSL_370.rf)
for(i in 1:length(IPSL_370.rf)){
  min_max <- range(IPSL_370.rf[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_370.rf", i, sp.n, ".txt",  sep="_"))
}


st4 <- stack(IPSL_370.mx)
for(i in 1:length(IPSL_370.mx)){
  min_max <- range(IPSL_370.mx[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_370.mx", i, sp.n, ".txt",  sep="_"))
}


st5 <- stack(IPSL_370.sv)
for(i in 1:length(IPSL_370.sv)){
  min_max <- range(IPSL_370.sv[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_370.sv", i, sp.n, ".txt",  sep="_"))
}


# st <- stack(st1, st2, st3, st4, st5)
st <- stack(st1, st3, st4, st5)
w.without.null <- w[w >= tss.lim]
ens2.IPSL_370 <- weighted.mean(st, w.without.null)
ens.fut.cont <- ens2.IPSL_370
ens2.fut.sd.w.IPSL_370 <- sum(w.without.null * (st - ens.fut.cont)^2)
ens2.fut.sd.w.IPSL_370 <- sqrt(ens2.fut.sd.w.IPSL_370)


# Uncertainty future ------------------------------------------------------

wbc <- c(bcTSSval)
wbc.without.null <- wbc[wbc >= tss.lim]
IPSL_370.bc.mean.w <- weighted.mean(st1, wbc.without.null)
IPSL_370.bc.sd.w <- sum(wbc.without.null * (st1 - IPSL_370.bc.mean.w)^2)
IPSL_370.bc.sd.w <- sqrt(IPSL_370.bc.sd.w)

# wgm <- c(gmTSSval)
# wgm.without.null <- wgm[wgm >= tss.lim]
# IPSL_370.gm.mean.w <- weighted.mean(st2, wgm.without.null)
# IPSL_370.gm.sd.w <- sum(wgm.without.null * (st2 - IPSL_370.gm.mean.w)^2)
# IPSL_370.gm.sd.w <- sqrt(IPSL_370.gm.sd.w)

wrf <- c(rfTSSval)
wrf.without.null <- wrf[wrf >= tss.lim]
IPSL_370.rf.mean.w <- weighted.mean(st3, wrf.without.null)
IPSL_370.rf.sd.w <- sum(wrf.without.null * (st3 - IPSL_370.rf.mean.w)^2)
IPSL_370.rf.sd.w <- sqrt(IPSL_370.rf.sd.w)

wmx <- c(mxTSSval)
wmx.without.null <- wmx[wmx >= tss.lim]
IPSL_370.mx.mean.w <- weighted.mean(st4, wmx.without.null)
IPSL_370.mx.sd.w <- sum(wmx.without.null * (st4 - IPSL_370.mx.mean.w)^2)
IPSL_370.mx.sd.w <- sqrt(IPSL_370.mx.sd.w)

wsv <- c(svTSSval)
wsv.without.null <- wsv[wsv >= tss.lim]
IPSL_370.sv.mean.w <- weighted.mean(st5, wsv.without.null)
IPSL_370.sv.sd.w <- sum(wsv.without.null * (st5 - IPSL_370.sv.mean.w)^2)
IPSL_370.sv.sd.w <- sqrt(IPSL_370.sv.sd.w)

IPSL_370.sd.w <- mean(IPSL_370.bc.sd.w,IPSL_370.rf.sd.w,IPSL_370.mx.sd.w,
                        IPSL_370.sv.sd.w)

####

ens.fut <- ens.IPSL_370.bin 
tval <- unique(ens.fut)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.IPSL_370 <- ens.fut >= tval 

#########

# Ensemble MIROC6_370 --------------------------------------
# binary
# algo.MIROC6_370 <- list(MIROC6_370.bc.ens.bin, MIROC6_370.gm.ens.bin, MIROC6_370.rf.ens.bin, MIROC6_370.mx.ens.bin, MIROC6_370.sv.ens.bin)
algo.MIROC6_370 <- list(MIROC6_370.bc.ens.bin, MIROC6_370.rf.ens.bin, MIROC6_370.mx.ens.bin, MIROC6_370.sv.ens.bin)
algo.MIROC6_370 <- algo.MIROC6_370[is.na(algo.MIROC6_370) == FALSE]
ens.MIROC6_370 <- Reduce('+', algo.MIROC6_370)
tval <- unique(ens.MIROC6_370)
tval <- tval[tval != 0]
tval <- median(tval)
ens.MIROC6_370.bin <- ens.MIROC6_370 >= tval

# continuous - TSS weighted average

st1 <- stack(MIROC6_370.bc)
for(i in 1:length(MIROC6_370.bc)){
  min_max <- range(MIROC6_370.bc[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_370.bc", i, sp.n, ".txt",  sep="_"))
}


# st2 <- stack(MIROC6_370.gm)
# for(i in 1:length(MIROC6_370.gm)){
#   min_max <- range(MIROC6_370.gm[[i]][], na.rm=T)
#   write.table(min_max, paste("MIROC6_370.gm", i, sp.n, ".txt",  sep="_"))
# }


st3 <- stack(MIROC6_370.rf)
for(i in 1:length(MIROC6_370.rf)){
  min_max <- range(MIROC6_370.rf[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_370.rf", i, sp.n, ".txt",  sep="_"))
}


st4 <- stack(MIROC6_370.mx)
for(i in 1:length(MIROC6_370.mx)){
  min_max <- range(MIROC6_370.mx[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_370.mx", i, sp.n, ".txt",  sep="_"))
}


st5 <- stack(MIROC6_370.sv)
for(i in 1:length(MIROC6_370.sv)){
  min_max <- range(MIROC6_370.sv[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_370.sv", i, sp.n, ".txt",  sep="_"))
}


# st <- stack(st1, st2, st3, st4, st5)
st <- stack(st1, st3, st4, st5)
w.without.null <- w[w >= tss.lim]
ens2.MIROC6_370 <- weighted.mean(st, w.without.null)
ens.fut.cont <- ens2.MIROC6_370
ens2.fut.sd.w.MIROC6_370 <- sum(w.without.null * (st - ens.fut.cont)^2)
ens2.fut.sd.w.MIROC6_370 <- sqrt(ens2.fut.sd.w.MIROC6_370)


# Uncertainty future ------------------------------------------------------

wbc <- c(bcTSSval)
wbc.without.null <- wbc[wbc >= tss.lim]
MIROC6_370.bc.mean.w <- weighted.mean(st1, wbc.without.null)
MIROC6_370.bc.sd.w <- sum(wbc.without.null * (st1 - MIROC6_370.bc.mean.w)^2)
MIROC6_370.bc.sd.w <- sqrt(MIROC6_370.bc.sd.w)

# wgm <- c(gmTSSval)
# wgm.without.null <- wgm[wgm >= tss.lim]
# MIROC6_370.gm.mean.w <- weighted.mean(st2, wgm.without.null)
# MIROC6_370.gm.sd.w <- sum(wgm.without.null * (st2 - MIROC6_370.gm.mean.w)^2)
# MIROC6_370.gm.sd.w <- sqrt(MIROC6_370.gm.sd.w)

wrf <- c(rfTSSval)
wrf.without.null <- wrf[wrf >= tss.lim]
MIROC6_370.rf.mean.w <- weighted.mean(st3, wrf.without.null)
MIROC6_370.rf.sd.w <- sum(wrf.without.null * (st3 - MIROC6_370.rf.mean.w)^2)
MIROC6_370.rf.sd.w <- sqrt(MIROC6_370.rf.sd.w)

wmx <- c(mxTSSval)
wmx.without.null <- wmx[wmx >= tss.lim]
MIROC6_370.mx.mean.w <- weighted.mean(st4, wmx.without.null)
MIROC6_370.mx.sd.w <- sum(wmx.without.null * (st4 - MIROC6_370.mx.mean.w)^2)
MIROC6_370.mx.sd.w <- sqrt(MIROC6_370.mx.sd.w)

wsv <- c(svTSSval)
wsv.without.null <- wsv[wsv >= tss.lim]
MIROC6_370.sv.mean.w <- weighted.mean(st5, wsv.without.null)
MIROC6_370.sv.sd.w <- sum(wsv.without.null * (st5 - MIROC6_370.sv.mean.w)^2)
MIROC6_370.sv.sd.w <- sqrt(MIROC6_370.sv.sd.w)

MIROC6_370.sd.w <- mean(MIROC6_370.bc.sd.w,MIROC6_370.rf.sd.w,MIROC6_370.mx.sd.w,
                        MIROC6_370.sv.sd.w)

####

ens.fut <- ens.MIROC6_370.bin 
tval <- unique(ens.fut)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.MIROC6_370 <- ens.fut >= tval 

##### 

# Ensemble CSM2_585 --------------------------------------

# binary
# algo.CSM2_585 <- list(CSM2_585.bc.ens.bin, CSM2_585.gm.ens.bin, CSM2_585.rf.ens.bin, CSM2_585.mx.ens.bin, CSM2_585.sv.ens.bin)
algo.CSM2_585 <- list(CSM2_585.bc.ens.bin, CSM2_585.rf.ens.bin, CSM2_585.mx.ens.bin, CSM2_585.sv.ens.bin)
algo.CSM2_585 <- algo.CSM2_585[is.na(algo.CSM2_585) == FALSE]
ens.CSM2_585 <- Reduce('+', algo.CSM2_585)
tval <- unique(ens.CSM2_585)
tval <- tval[tval != 0]
tval <- median(tval)
ens.CSM2_585.bin <- ens.CSM2_585 >= tval

# continuous - TSS weighted average

st1 <- stack(CSM2_585.bc)
for(i in 1:length(CSM2_585.bc)){
  min_max <- range(CSM2_585.bc[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_585.bc", i, sp.n, ".txt",  sep="_"))
}


# st2 <- stack(CSM2_585.gm)
# for(i in 1:length(CSM2_585.gm)){
#   min_max <- range(CSM2_585.gm[[i]][], na.rm=T)
#   write.table(min_max, paste("CSM2_585.gm", i, sp.n, ".txt",  sep="_"))
# }


st3 <- stack(CSM2_585.rf)
for(i in 1:length(CSM2_585.rf)){
  min_max <- range(CSM2_585.rf[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_585.rf", i, sp.n, ".txt",  sep="_"))
}


st4 <- stack(CSM2_585.mx)
for(i in 1:length(CSM2_585.mx)){
  min_max <- range(CSM2_585.mx[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_585.mx", i, sp.n, ".txt",  sep="_"))
}


st5 <- stack(CSM2_585.sv)
for(i in 1:length(CSM2_585.sv)){
  min_max <- range(CSM2_585.sv[[i]][], na.rm=T)
  write.table(min_max, paste("CSM2_585.sv", i, sp.n, ".txt",  sep="_"))
}


#st <- stack(st1, st2, st3, st4, st5)
st <- stack(st1, st3, st4, st5)
w.without.null <- w[w >= tss.lim]
ens2.CSM2_585 <- weighted.mean(st, w.without.null)
ens.fut.cont <- ens2.CSM2_585
ens2.fut.sd.w.CSM2_585 <- sum(w.without.null * (st - ens.fut.cont)^2)
ens2.fut.sd.w.CSM2_585 <- sqrt(ens2.fut.sd.w.CSM2_585)


# Uncertainty future ------------------------------------------------------

wbc <- c(bcTSSval)
wbc.without.null <- wbc[wbc >= tss.lim]
CSM2_585.bc.mean.w <- weighted.mean(st1, wbc.without.null)
CSM2_585.bc.sd.w <- sum(wbc.without.null * (st1 - CSM2_585.bc.mean.w)^2)
CSM2_585.bc.sd.w <- sqrt(CSM2_585.bc.sd.w)

# wgm <- c(gmTSSval)
# wgm.without.null <- wgm[wgm >= tss.lim]
# CSM2_585.gm.mean.w <- weighted.mean(st2, wgm.without.null)
# CSM2_585.gm.sd.w <- sum(wgm.without.null * (st2 - CSM2_585.gm.mean.w)^2)
# CSM2_585.gm.sd.w <- sqrt(CSM2_585.gm.sd.w)

wrf <- c(rfTSSval)
wrf.without.null <- wrf[wrf >= tss.lim]
CSM2_585.rf.mean.w <- weighted.mean(st3, wrf.without.null)
CSM2_585.rf.sd.w <- sum(wrf.without.null * (st3 - CSM2_585.rf.mean.w)^2)
CSM2_585.rf.sd.w <- sqrt(CSM2_585.rf.sd.w)

wmx <- c(mxTSSval)
wmx.without.null <- wmx[wmx >= tss.lim]
CSM2_585.mx.mean.w <- weighted.mean(st4, wmx.without.null)
CSM2_585.mx.sd.w <- sum(wmx.without.null * (st4 - CSM2_585.mx.mean.w)^2)
CSM2_585.mx.sd.w <- sqrt(CSM2_585.mx.sd.w)

wsv <- c(svTSSval)
wsv.without.null <- wsv[wsv >= tss.lim]
CSM2_585.sv.mean.w <- weighted.mean(st5, wsv.without.null)
CSM2_585.sv.sd.w <- sum(wsv.without.null * (st5 - CSM2_585.sv.mean.w)^2)
CSM2_585.sv.sd.w <- sqrt(CSM2_585.sv.sd.w)

CSM2_585.sd.w <- mean(CSM2_585.bc.sd.w,CSM2_585.rf.sd.w,CSM2_585.mx.sd.w,
                      CSM2_585.sv.sd.w)

####

ens.fut <- ens.CSM2_585.bin 
tval <- unique(ens.fut)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.CSM2_585 <- ens.fut >= tval #arquivo final

#########

# Ensemble IPSL_585 --------------------------------------
# binary
# algo.IPSL_585 <- list(IPSL_585.bc.ens.bin, IPSL_585.gm.ens.bin, IPSL_585.rf.ens.bin, IPSL_585.mx.ens.bin, IPSL_585.sv.ens.bin)
algo.IPSL_585 <- list(IPSL_585.bc.ens.bin, IPSL_585.rf.ens.bin, IPSL_585.mx.ens.bin, IPSL_585.sv.ens.bin)
algo.IPSL_585 <- algo.IPSL_585[is.na(algo.IPSL_585) == FALSE]
ens.IPSL_585 <- Reduce('+', algo.IPSL_585)
tval <- unique(ens.IPSL_585)
tval <- tval[tval != 0]
tval <- median(tval)
ens.IPSL_585.bin <- ens.IPSL_585 >= tval

# continuous - TSS weighted average

st1 <- stack(IPSL_585.bc)
for(i in 1:length(IPSL_585.bc)){
  min_max <- range(IPSL_585.bc[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_585.bc", i, sp.n, ".txt",  sep="_"))
}


# st2 <- stack(IPSL_585.gm)
# for(i in 1:length(IPSL_585.gm)){
#   min_max <- range(IPSL_585.gm[[i]][], na.rm=T)
#   write.table(min_max, paste("IPSL_585.gm", i, sp.n, ".txt",  sep="_"))
# }


st3 <- stack(IPSL_585.rf)
for(i in 1:length(IPSL_585.rf)){
  min_max <- range(IPSL_585.rf[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_585.rf", i, sp.n, ".txt",  sep="_"))
}


st4 <- stack(IPSL_585.mx)
for(i in 1:length(IPSL_585.mx)){
  min_max <- range(IPSL_585.mx[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_585.mx", i, sp.n, ".txt",  sep="_"))
}


st5 <- stack(IPSL_585.sv)
for(i in 1:length(IPSL_585.sv)){
  min_max <- range(IPSL_585.sv[[i]][], na.rm=T)
  write.table(min_max, paste("IPSL_585.sv", i, sp.n, ".txt",  sep="_"))
}


# st <- stack(st1, st2, st3, st4, st5)
st <- stack(st1, st3, st4, st5)
w.without.null <- w[w >= tss.lim]
ens2.IPSL_585 <- weighted.mean(st, w.without.null)
ens.fut.cont <- ens2.IPSL_585
ens2.fut.sd.w.IPSL_585 <- sum(w.without.null * (st - ens.fut.cont)^2)
ens2.fut.sd.w.IPSL_585 <- sqrt(ens2.fut.sd.w.IPSL_585)


# Uncertainty future ------------------------------------------------------

wbc <- c(bcTSSval)
wbc.without.null <- wbc[wbc >= tss.lim]
IPSL_585.bc.mean.w <- weighted.mean(st1, wbc.without.null)
IPSL_585.bc.sd.w <- sum(wbc.without.null * (st1 - IPSL_585.bc.mean.w)^2)
IPSL_585.bc.sd.w <- sqrt(IPSL_585.bc.sd.w)

# wgm <- c(gmTSSval)
# wgm.without.null <- wgm[wgm >= tss.lim]
# IPSL_585.gm.mean.w <- weighted.mean(st2, wgm.without.null)
# IPSL_585.gm.sd.w <- sum(wgm.without.null * (st2 - IPSL_585.gm.mean.w)^2)
# IPSL_585.gm.sd.w <- sqrt(IPSL_585.gm.sd.w)

wrf <- c(rfTSSval)
wrf.without.null <- wrf[wrf >= tss.lim]
IPSL_585.rf.mean.w <- weighted.mean(st3, wrf.without.null)
IPSL_585.rf.sd.w <- sum(wrf.without.null * (st3 - IPSL_585.rf.mean.w)^2)
IPSL_585.rf.sd.w <- sqrt(IPSL_585.rf.sd.w)

wmx <- c(mxTSSval)
wmx.without.null <- wmx[wmx >= tss.lim]
IPSL_585.mx.mean.w <- weighted.mean(st4, wmx.without.null)
IPSL_585.mx.sd.w <- sum(wmx.without.null * (st4 - IPSL_585.mx.mean.w)^2)
IPSL_585.mx.sd.w <- sqrt(IPSL_585.mx.sd.w)

wsv <- c(svTSSval)
wsv.without.null <- wsv[wsv >= tss.lim]
IPSL_585.sv.mean.w <- weighted.mean(st5, wsv.without.null)
IPSL_585.sv.sd.w <- sum(wsv.without.null * (st5 - IPSL_585.sv.mean.w)^2)
IPSL_585.sv.sd.w <- sqrt(IPSL_585.sv.sd.w)

IPSL_585.sd.w <- mean(IPSL_585.bc.sd.w,IPSL_585.rf.sd.w,IPSL_585.mx.sd.w,
                      IPSL_585.sv.sd.w)

####

ens.fut <- ens.IPSL_585.bin 
tval <- unique(ens.fut)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.IPSL_585 <- ens.fut >= tval

#########

# Ensemble MIROC6_585 --------------------------------------
# binary
# algo.MIROC6_585 <- list(MIROC6_585.bc.ens.bin, MIROC6_585.gm.ens.bin, MIROC6_585.rf.ens.bin, MIROC6_585.mx.ens.bin, MIROC6_585.sv.ens.bin)
algo.MIROC6_585 <- list(MIROC6_585.bc.ens.bin, MIROC6_585.rf.ens.bin, MIROC6_585.mx.ens.bin, MIROC6_585.sv.ens.bin)
algo.MIROC6_585 <- algo.MIROC6_585[is.na(algo.MIROC6_585) == FALSE]
ens.MIROC6_585 <- Reduce('+', algo.MIROC6_585)
tval <- unique(ens.MIROC6_585)
tval <- tval[tval != 0]
tval <- median(tval)
ens.MIROC6_585.bin <- ens.MIROC6_585 >= tval

# continuous - TSS weighted average

st1 <- stack(MIROC6_585.bc)
for(i in 1:length(MIROC6_585.bc)){
  min_max <- range(MIROC6_585.bc[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_585.bc", i, sp.n, ".txt",  sep="_"))
}


# st2 <- stack(MIROC6_585.gm)
# for(i in 1:length(MIROC6_585.gm)){
#   min_max <- range(MIROC6_585.gm[[i]][], na.rm=T)
#   write.table(min_max, paste("MIROC6_585.gm", i, sp.n, ".txt",  sep="_"))
# }


st3 <- stack(MIROC6_585.rf)
for(i in 1:length(MIROC6_585.rf)){
  min_max <- range(MIROC6_585.rf[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_585.rf", i, sp.n, ".txt",  sep="_"))
}


st4 <- stack(MIROC6_585.mx)
for(i in 1:length(MIROC6_585.mx)){
  min_max <- range(MIROC6_585.mx[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_585.mx", i, sp.n, ".txt",  sep="_"))
}


st5 <- stack(MIROC6_585.sv)
for(i in 1:length(MIROC6_585.sv)){
  min_max <- range(MIROC6_585.sv[[i]][], na.rm=T)
  write.table(min_max, paste("MIROC6_585.sv", i, sp.n, ".txt",  sep="_"))
}


# st <- stack(st1, st2, st3, st4, st5)
st <- stack(st1, st3, st4, st5)
w.without.null <- w[w >= tss.lim]
ens2.MIROC6_585 <- weighted.mean(st, w.without.null)
ens.fut.cont <- ens2.MIROC6_585
ens2.fut.sd.w.MIROC6_585 <- sum(w.without.null * (st - ens.fut.cont)^2)
ens2.fut.sd.w.MIROC6_585 <- sqrt(ens2.fut.sd.w.MIROC6_585)


# Uncertainty future ------------------------------------------------------

wbc <- c(bcTSSval)
wbc.without.null <- wbc[wbc >= tss.lim]
MIROC6_585.bc.mean.w <- weighted.mean(st1, wbc.without.null)
MIROC6_585.bc.sd.w <- sum(wbc.without.null * (st1 - MIROC6_585.bc.mean.w)^2)
MIROC6_585.bc.sd.w <- sqrt(MIROC6_585.bc.sd.w)

# wgm <- c(gmTSSval)
# wgm.without.null <- wgm[wgm >= tss.lim]
# MIROC6_585.gm.mean.w <- weighted.mean(st2, wgm.without.null)
# MIROC6_585.gm.sd.w <- sum(wgm.without.null * (st2 - MIROC6_585.gm.mean.w)^2)
# MIROC6_585.gm.sd.w <- sqrt(MIROC6_585.gm.sd.w)

wrf <- c(rfTSSval)
wrf.without.null <- wrf[wrf >= tss.lim]
MIROC6_585.rf.mean.w <- weighted.mean(st3, wrf.without.null)
MIROC6_585.rf.sd.w <- sum(wrf.without.null * (st3 - MIROC6_585.rf.mean.w)^2)
MIROC6_585.rf.sd.w <- sqrt(MIROC6_585.rf.sd.w)

wmx <- c(mxTSSval)
wmx.without.null <- wmx[wmx >= tss.lim]
MIROC6_585.mx.mean.w <- weighted.mean(st4, wmx.without.null)
MIROC6_585.mx.sd.w <- sum(wmx.without.null * (st4 - MIROC6_585.mx.mean.w)^2)
MIROC6_585.mx.sd.w <- sqrt(MIROC6_585.mx.sd.w)

wsv <- c(svTSSval)
wsv.without.null <- wsv[wsv >= tss.lim]
MIROC6_585.sv.mean.w <- weighted.mean(st5, wsv.without.null)
MIROC6_585.sv.sd.w <- sum(wsv.without.null * (st5 - MIROC6_585.sv.mean.w)^2)
MIROC6_585.sv.sd.w <- sqrt(MIROC6_585.sv.sd.w)

MIROC6_585.sd.w <- mean(MIROC6_585.bc.sd.w,MIROC6_585.rf.sd.w,MIROC6_585.mx.sd.w,
                        MIROC6_585.sv.sd.w)

####

ens.fut <- ens.MIROC6_585.bin 
tval <- unique(ens.fut)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.MIROC6_585 <- ens.fut >= tval


# Ensemble all GCMs --------------------------------------

ens.fut.370 <- ens.fut.bin.CSM2_370 + ens.fut.bin.IPSL_370 + ens.fut.bin.MIROC6_370
tval <- unique(ens.fut.370)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.370 <- ens.fut.370 >= tval

ens.fut.585 <- ens.fut.bin.CSM2_585 + ens.fut.bin.IPSL_585 + ens.fut.bin.MIROC6_585
tval <- unique(ens.fut.585)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin.585 <- ens.fut.585 >= tval

ens.fut.cont.370 <- mean(ens2.CSM2_370, ens2.IPSL_370, ens2.MIROC6_370)
ens.fut.cont.585 <- mean(ens2.CSM2_585, ens2.IPSL_585, ens2.MIROC6_585)

#uncertainty ensemble
ens.cur.sd.w.2 <- cur.sd.w
ens.Future_370_2050.sd.w.2 <- mean(CSM2_370.sd.w,IPSL_370.sd.w,MIROC6_370.sd.w)
ens.Future_585_2050.sd.w.2 <- mean(CSM2_585.sd.w,IPSL_585.sd.w,MIROC6_585.sd.w)

# Writing rasters --------------------------------------

#if(ens.maps == T){
cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', sp.n, '...', '\n')
writeRaster(ens2.cur, file = paste(target_dir, '/CUR.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.CSM2_370, file = paste(target_dir, '/CSM2_370_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.IPSL_370, file = paste(target_dir, '/IPSL_370_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.MIROC6_370, file = paste(target_dir, '/MIROC6_370_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.CSM2_585, file = paste(target_dir, '/CSM2_585_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.IPSL_585, file = paste(target_dir, '/IPSL_585_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.MIROC6_585, file = paste(target_dir, '/MIROC6_585_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.fut.cont.370, file = paste(target_dir, '/Future_370_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.fut.cont.585, file = paste(target_dir, '/Future_585_2050.cont_', sp.n, '.tif', sep=""),overwrite=TRUE)


writeRaster(ens.cur.bin, file = paste(target_dir, '/CUR.bin_', sp.n, '.tif', sep=""),overwrite=TRUE) #binarios
writeRaster(ens.CSM2_370.bin, file = paste(target_dir, '/CSM2_370_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.IPSL_370.bin, file = paste(target_dir, '/IPSL_370_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.MIROC6_370.bin, file = paste(target_dir, '/MIROC6_370_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.CSM2_585.bin, file = paste(target_dir, '/CSM2_585_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.IPSL_585.bin, file = paste(target_dir, '/IPSL_585_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.MIROC6_585.bin, file = paste(target_dir, '/MIROC6_585_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.fut.bin.370, file = paste(target_dir, '/Future_370_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.fut.bin.585, file = paste(target_dir, '/Future_585_2050.bin_', sp.n, '.tif', sep=""),overwrite=TRUE)


#weighted.mean and weighted.sd 
writeRaster(ens.cur.sd.w.2, file = paste(target_dir, '/ens.cur.sd.w.2_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.Future_370_2050.sd.w.2,file = paste(target_dir, '/ens.Future_370_2050.sd.w.2_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens.Future_585_2050.sd.w.2, file = paste(target_dir, '/ens.Future_585_2050.sd.w.2_', sp.n, '.tif', sep=""),overwrite=TRUE)

writeRaster(ens2.fut.sd.w.CSM2_370, file = paste(target_dir, '/ens.CSM2_370_2050.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.fut.sd.w.IPSL_370, file = paste(target_dir, '/ens.IPSL_370_2050.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.fut.sd.w.MIROC6_370, file = paste(target_dir, '/ens.MIROC6_370_2050.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.fut.sd.w.CSM2_585, file = paste(target_dir, '/ens.CSM2_585_2050.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.fut.sd.w.IPSL_585, file = paste(target_dir, '/ens.IPSL_585_2050.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(ens2.fut.sd.w.MIROC6_585, file = paste(target_dir, '/ens.MIROC6_585_2050.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)

# CURRENT

writeRaster(cur.bc.mean.w, file = paste(target_dir, '/cur.bc.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(cur.bc.sd.w, file = paste(target_dir, '/cur.bc.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)

writeRaster(cur.rf.mean.w, file = paste(target_dir, '/cur.rf.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(cur.rf.sd.w, file = paste(target_dir, '/cur.rf.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)

writeRaster(cur.mx.mean.w, file = paste(target_dir, '/cur.mx.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(cur.mx.sd.w, file = paste(target_dir, '/cur.mx.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)

writeRaster(cur.sv.mean.w, file = paste(target_dir, '/cur.sv.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(cur.sv.sd.w, file = paste(target_dir, '/cur.sv.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)


# CSM2_370

writeRaster(CSM2_370.bc.mean.w, file = paste(target_dir, '/CSM2_370_2050.bc.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_370.bc.sd.w, file = paste(target_dir, '/CSM2_370_2050.bc.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_370.rf.mean.w, file = paste(target_dir, '/CSM2_370_2050.rf.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_370.rf.sd.w, file = paste(target_dir, '/CSM2_370_2050.rf.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_370.mx.mean.w, file = paste(target_dir, '/CSM2_370_2050.mx.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE) 
writeRaster(CSM2_370.mx.sd.w, file = paste(target_dir, '/CSM2_370_2050.mx.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_370.sv.mean.w, file = paste(target_dir, '/CSM2_370_2050.sv.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_370.sv.sd.w, file = paste(target_dir, '/CSM2_370_2050.sv.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)


# IPSL_370

writeRaster(IPSL_370.bc.mean.w, file = paste(target_dir, '/IPSL_370_2050.bc.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_370.bc.sd.w, file = paste(target_dir, '/IPSL_370_2050.bc.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_370.rf.mean.w, file = paste(target_dir, '/IPSL_370_2050.rf.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_370.rf.sd.w, file = paste(target_dir, '/IPSL_370_2050.rf.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_370.mx.mean.w, file = paste(target_dir, '/IPSL_370_2050.mx.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE) 
writeRaster(IPSL_370.mx.sd.w, file = paste(target_dir, '/IPSL_370_2050.mx.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_370.sv.mean.w, file = paste(target_dir, '/IPSL_370_2050.sv.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_370.sv.sd.w, file = paste(target_dir, '/IPSL_370_2050.sv.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)

# MIROC6_370

writeRaster(MIROC6_370.bc.mean.w, file = paste(target_dir, '/MIROC6_370_2050.bc.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_370.bc.sd.w, file = paste(target_dir, '/MIROC6_370_2050.bc.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_370.rf.mean.w, file = paste(target_dir, '/MIROC6_370_2050.rf.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_370.rf.sd.w, file = paste(target_dir, '/MIROC6_370_2050.rf.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_370.mx.mean.w, file = paste(target_dir, '/MIROC6_370_2050.mx.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE) 
writeRaster(MIROC6_370.mx.sd.w, file = paste(target_dir, '/MIROC6_370_2050.mx.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_370.sv.mean.w, file = paste(target_dir, '/MIROC6_370_2050.sv.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_370.sv.sd.w, file = paste(target_dir, '/MIROC6_370_2050.sv.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)

# CSM2_585

writeRaster(CSM2_585.bc.mean.w, file = paste(target_dir, '/CSM2_585_2050.bc.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_585.bc.sd.w, file = paste(target_dir, '/CSM2_585_2050.bc.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_585.rf.mean.w, file = paste(target_dir, '/CSM2_585_2050.rf.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_585.rf.sd.w, file = paste(target_dir, '/CSM2_585_2050.rf.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_585.mx.mean.w, file = paste(target_dir, '/CSM2_585_2050.mx.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE) 
writeRaster(CSM2_585.mx.sd.w, file = paste(target_dir, '/CSM2_585_2050.mx.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_585.sv.mean.w, file = paste(target_dir, '/CSM2_585_2050.sv.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(CSM2_585.sv.sd.w, file = paste(target_dir, '/CSM2_585_2050.sv.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)


# IPSL_585

writeRaster(IPSL_585.bc.mean.w, file = paste(target_dir, '/IPSL_585_2050.bc.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_585.bc.sd.w, file = paste(target_dir, '/IPSL_585_2050.bc.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_585.rf.mean.w, file = paste(target_dir, '/IPSL_585_2050.rf.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_585.rf.sd.w, file = paste(target_dir, '/IPSL_585_2050.rf.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_585.mx.mean.w, file = paste(target_dir, '/IPSL_585_2050.mx.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE) 
writeRaster(IPSL_585.mx.sd.w, file = paste(target_dir, '/IPSL_585_2050.mx.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_585.sv.mean.w, file = paste(target_dir, '/IPSL_585_2050.sv.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(IPSL_585.sv.sd.w, file = paste(target_dir, '/IPSL_585_2050.sv.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)

# MIROC6_585

writeRaster(MIROC6_585.bc.mean.w, file = paste(target_dir, '/MIROC6_585_2050.bc.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_585.bc.sd.w, file = paste(target_dir, '/MIROC6_585_2050.bc.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_585.rf.mean.w, file = paste(target_dir, '/MIROC6_585_2050.rf.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_585.rf.sd.w, file = paste(target_dir, '/MIROC6_585_2050.rf.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_585.mx.mean.w, file = paste(target_dir, '/MIROC6_585_2050.mx.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE) 
writeRaster(MIROC6_585.mx.sd.w, file = paste(target_dir, '/MIROC6_585_2050.mx.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_585.sv.mean.w, file = paste(target_dir, '/MIROC6_585_2050.sv.mean.w_', sp.n, '.tif', sep=""),overwrite=TRUE)
writeRaster(MIROC6_585.sv.sd.w, file = paste(target_dir, '/MIROC6_585_2050.sv.sd.w_', sp.n, '.tif', sep=""),overwrite=TRUE)


cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Finished train and test datasets for', sp.n, 'with ', lim, 'lines...', '\n')

save.image("./outputs_5km/Crax_fasciolata.rData")

finished_time = Sys.time()

cat( format( finished_time, "%a %b %d %X %Y"), '-', 'FINISHED', '\n')
write(format( finished_time, "%a %b %d %X %Y"), file=paste('./outputs_5km/', '/FINISHED.txt', sep="")) 

beepr::beep(8)

###############################################################
