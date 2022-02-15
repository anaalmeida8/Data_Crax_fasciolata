# --------------------------------------------------------------
# Geographical cleaning
# 30 set 2020
# modified: 19 abr 2021
# ACA
# -------------------------------------------------------------
#


# Loading packages --------------------------------------------------------
library(dismo)
library(raster)
library(spThin)
library(rgdal)
library(rgeos)
library(readr)
library(readxl)

# Extracting occurrence records from GBIF ----------------------
crax <- gbif(genus="crax", species="fasciolata*", geo=TRUE)
getwd()
crax.gbif <- read_delim("Doutorado/Capitulo_3/spdata/Dados/new/gbif.csv", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)
  
colnames(crax.gbif)
dim(crax.gbif)

crax2 <- crax.gbif[,c("species","infraspecificEpithet","decimalLatitude", "decimalLongitude","collectionCode","year")]
nas <- which(is.na(crax2$decimalLatitude)) 
crax3 <- crax2[- nas, ] # delete records without latitude
View(crax3)
dim(crax3)

write.csv(crax3, file = "mutum_gbif.csv")


setwd("C:/Users/cap_a/OneDrive/Documentos")

# Loading other data sheet

# sibbr
crax.sibbr <- read_csv("Doutorado/Capitulo_3/spdata/Dados/new/Crax_fasciolata_SibBr.csv")
dim(crax.sibbr)
colnames(crax.sibbr)
View(crax.sibbr)

crax2 <- crax.sibbr[,c("Species","Latitude", "Longitude","Collection Code","Year")]
nas <- which(is.na(crax2$Latitude))
crax3 <- crax2[- nas, ]
View(crax3)
dim(crax3)

setwd("Doutorado/Capitulo_3/spdata/Dados/new")
write.csv(crax3, file = "mutum_sibbr.csv")


# species link
crax.splink <- read_excel("speciesLink_all_51777.xlsx")
dim(crax.splink)
colnames(crax.splink)

crax2 <- crax.splink[,c("scientificname","subspecies","latitude", "longitude","collectioncode","yearcollected")]
nas <- which(is.na(crax2$latitude))
crax3 <- crax2[- nas, ]
View(crax3)
dim(crax3)

setwd("Doutorado/Capitulo_3/spdata/Dados/new")
write.csv(crax3, file = "mutum_splink.csv")
getwd()


###########################################################################

# Loading table ---------------------------------------------------------

sp <- read.csv("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new/crax_fasc.csv", sep = ";")
head(sp)
dim(sp)
colnames(sp)

# Flagging and cleaning errors -----------------------------------

flags_spatial <- CoordinateCleaner::clean_coordinates(
  x = sp, 
  species = "species",
  lon = "lon", 
  lat = "lat",
  tests = c("duplicates", 
            "equal", 
            "capitals",
            "centroids",
            "institutions",
            "seas")
  ) 


flags_spatial <- CoordinateCleaner::clean_coordinates(
  x = sp, 
  species = "species",
  lon = "lon", 
  lat = "lat",
  tests = c("duplicates", # duplicatas
            "equal", # coordenadas iguais
            "capitals",
            "centroids",
            "institutions",
            "seas"),
  value = "clean"
)

dim(flags_spatial)

sp <- flags_spatial

# Defining the same projection for coordinates and map
coordinates (sp) <- ~lon+lat
proj4string (sp)= CRS("+proj=longlat +datum=WGS84") # set it to UTM
sp_projetado <- spTransform(sp,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# Removing records out of species' IUCN polygon ---------------------------------

iucn <- readOGR ("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/IUCN",
                           "data_0")

proj4string(iucn)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
iucn <- spTransform(iucn,
                    CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

par(mar=c(0,0,0,0))
plot(iucn)
plot(sp_projetado, add=T, col= "salmon", pch=20)

sp1 <- as.data.frame(raster::intersect(sp_projetado, iucn))

coordinates (sp1) <- ~lon+lat
proj4string (sp1)= CRS("+proj=longlat +datum=WGS84") # set it to UTM
sp1_projetado <- spTransform(sp1,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

south_america1 <- readOGR ("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Shapefiles/South_America",
                           "South_America")

proj4string(south_america1)=CRS("+proj=longlat +datum=WGS84")
south_america <- spTransform(south_america1,
                             CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(south_america)
plot(sp1_projetado, add=T, col= "red", pch=20)

dim(sp1_projetado)

write.csv(sp1_projetado,("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new/Crax_fasciolata.csv"),row.names = F)


# spThin -------------------------------------------------

thinned_dataset_full <-
  thin(loc.data = as.data.frame(sp1_projetado),
       lat.col = "lat", long.col = "lon",
       spec.col = "species",
       thin.par = 5, reps = 1, #thin.par=10 = 10km 
       locs.thinned.list.return = TRUE,
       write.files = TRUE,
       max.files = 1,
       out.dir = "C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new",
       out.base = "Crax_fasciolata",
       write.log.file = F)


# Loading table -------------------------------------------

crax_final <- read.csv("C:/Users/cap_a/OneDrive/Documentos/Doutorado/Capitulo_3/spdata/Dados/new/Crax_fasciolata_thin1-5.csv")
dim(crax_final) # 626


# Map and cleaned occurrence records
crax_coord <- crax_final
plot(south_america)
coordinates(crax_coord) <- ~lon+lat
plot(crax_coord, add=T, col= "red", pch=20)
