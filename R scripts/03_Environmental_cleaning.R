# -----------------------------------------
# Environmental cleaning (Varella et al., 2014)
# 30 set 2020
# ACA
# -----------------------------------------
#

# memory
rm(list = ls())

library(raster)
library(maps)
library(mapdata)
library(biomod2)
library(doParallel)
library(maptools)
library(dismo)


# loading selected biovars

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Worldclim/2.5m/variaveis_cortadas/new")

bio4 <- raster("./bio_4.tif")
bio8 <- raster("./bio_8.tif")
bio14 <- raster("./bio_14.tif")
bio18 <- raster("./bio_18.tif")
bio19 <- raster("./bio_19.tif")

environment <- stack(bio4,bio8,bio14,bio18,bio19)
names(environment) <- c("bio4","bio8","bio14","bio18","bio19")

setwd("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new")
Species <- read.csv("Crax_fasciolata_thin1-5.csv")

Species <- Species[,2:3]
head(Species)
dim(Species)

# Removing autocorrelation and creating training table ---------------------


# From https://github.com/SaraVarela/envSample
# Varela, S., Anderson, R.P., Garcia-Valdes, R., Fernandez-Gonzalez, F., 2014. Environmental filters reduce the effects of sampling bias
# and improve predictions of ecological niche models. Ecography 37, 1084-1091.

library(rgdal)
library(roxygen2)
library(sqldf)
library(testthat)

envSample<- function (coord, filters, res, do.plot=TRUE){
  
  n<- length (filters)
  pot_points<- list ()
  for (i in 1:n){
    k<- filters [[i]] [!is.na(filters[[i]])]
    ext1<- range (k)
    ext1 [1]<- ext1[1]- 1
    x<- seq(ext1[1],ext1[2], by=res[[i]])
    pot_points[[i]]<- x
  }
  pot_p<- expand.grid(pot_points)
  
  ends<- NULL
  for (i in 1:n){
    fin<- pot_p [,i] + res[[i]]
    ends<- cbind (ends, fin)
  }
  
  pot_pp<- data.frame (pot_p, ends)
  pot_pp<- data.frame (pot_pp, groupID=c(1:nrow (pot_pp)))
  rows<- length (filters[[1]])
  filter<- data.frame(matrix(unlist(filters), nrow=rows))
  real_p<- data.frame (coord, filter)
  
  names_real<- c("lon", "lat")
  names_pot_st<- NULL
  names_pot_end<- NULL
  sql1<- NULL
  for (i in 1:n){
    names_real<- c(names_real, paste ("filter", i, sep=""))
    names_pot_st<- c(names_pot_st, paste ("start_f", i, sep=""))
    names_pot_end<- c(names_pot_end, paste ("end_f", i, sep=""))
    sql1<- paste (sql1, paste ("real_p.filter", i, sep=""), sep=", ")   
  }
  
  names (real_p)<- names_real
  names (pot_pp)<- c(names_pot_st, names_pot_end, "groupID")
  
  conditions<- paste ("(real_p.filter", 1, "<= pot_pp.end_f", 1,") and (real_p.filter", 1, "> pot_pp.start_f", 1, ")", sep="")
  for (i in 2:n){
    conditions<- paste (conditions, 
                        paste ("(real_p.filter", i, "<= pot_pp.end_f", i,") and (real_p.filter", i, "> pot_pp.start_f", i, ")", sep=""), 
                        sep="and")
  }
  
  selection_NA<- sqldf(paste ("select real_p.lon, real_p.lat, pot_pp.groupID",   
                              sql1, "from pot_pp left join real_p on", conditions, sep=" "))
  
  selection<- selection_NA [complete.cases(selection_NA),]
  
  final_points<- selection[!duplicated(selection$groupID), ]
  coord_filter<- data.frame (final_points$lon, final_points$lat) 
  names (coord_filter)<- c("lon", "lat")
  
  if (do.plot==TRUE){
    par (mfrow=c(1,2), mar=c(4,4,0,0.5))
    plot (filters[[1]], filters[[2]], pch=19, 
          col="grey50", xlab="Filter 1", ylab="Filter 2")
    points (final_points$filter1, final_points$filter2, 
            pch=19, col="#88000090")
    plot (coord, pch=19, col="grey50")
    map(add=T)
    points (coord_filter, pch=19, col="#88000090")
    
  }
  coord_filter
}



class(Species)

class(environment)

# Apply environmental filter to create training data set

env.data <- extract(environment, Species) #lembrar de fechar o R e reabrir pra rodar
#env.selected -> meu stack com minhas variaveis do modelo correlativo, os com menor valor de vif
#Species -> pontos de ocorrencia
env.data <- as.data.frame(env.data)


x <- 20 #default
Species.training <- envSample(Species, filters=list(env.data$bio4, env.data$bio8, env.data$bio14,
                                                    env.data$bio18, env.data$bio19),
                              res=list(x, x, x, x, x), do.plot=TRUE)

while(nrow(Species.training) <= 300) {
  x <- x - 1  
  (Species.training <- envSample(Species, filters=list(env.data$bio4, env.data$bio8, env.data$bio14,
                                                       env.data$bio18, env.data$bio19),
                                 res=list(x, x, x, x, x), do.plot=TRUE))
}

n_training <- as.numeric(dim(Species.training)[1])

sp_training = Species.training
sp_name <- rep("Crax_fasciolata", nrow(Species.training))
sp_training <- data.frame(sp_name,Species.training)
colnames(sp_training) <- c("species","lon","lat")

getwd()
setwd("..")
write.table(sp_training, "./Crax_fasciolata_training_5.csv",row.names = F,col.names=T,sep=",")

###############################################################