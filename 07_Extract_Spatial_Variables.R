library(data.table)
library(ggplot2)
library(raster)
library(rgdal)
library(readr)
library(geojsonio)
library(rgeos)
library(kableExtra)
library(DT)
library(stringr)
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("viridis")

# Reading in Caspio Token
source("Tokens/caspioSource.R")
# Reading in caspio GET command (don't know if this works for things > 1000)
source("caspioFunctions.R")

# Reading in Jepson File
jepson <- readOGR(dsn="Data/JepsonLayer",layer="Geographic_Subdivisions_of_California_TJMII_v2_060415")
jepson <- spTransform(jepson,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Reading in old cnddb point file (can't uplaod the more recent file)
#cnddb <- fread("Data/tblCNDDBOccurrencesRare_2019-Jun-21_1830.csv")
cnddb_shape <- readOGR(dsn="Data/CNDDBLayer",layer="cnddb")

# Extracting the label points from each CNDDB polygon
points_dat <- lapply(1:length(cnddb_shape@polygons), 
                     function(x) as.data.table(t(cnddb_shape@polygons[[x]]@labpt)))
points_dat <- rbindlist(points_dat, fill=TRUE)
points_dat <- cbind( as.data.table(cnddb_shape@data),points_dat)
points_dat<- points_dat[TAXONGROUP%in%c("Dicots","Monocots","Ferns","Bryophytes","Gymnosperms","Herbaceous")]
coordinates(points_dat) <-  ~V1 + V2
proj4string(points_dat) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
points_dat <- spTransform(points_dat,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Federal Lands
federal_lands <- readOGR(dsn="Data/FederalLands",layer="Federal_Lands_California")
federal_lands <- spTransform(federal_lands,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Reading in Accessions Data
# Getting more than 1000 rows! This is for the CaPR Accessions List
tableNew <- caspio_get_view("caprViewNoLists",login1,1)
TableCombined = NULL
pagenumber=1

while (dim(tableNew)[1]>0)
{
  pagenumber = pagenumber + 1
  TableCombined <- rbind(TableCombined,tableNew)
  tableNew <- caspio_get_view("caprViewNoLists",login1,pagenumber)
}

CaPRAcc <- TableCombined


#########################
## Extracting Jepson for CNDDB File
a<-over(points_dat,jepson)
jepCnddb <- cbind(as.data.table(points_dat@data)[,.(EONDX)], as.data.table(a))

# Merging Jepson onto overal cnddb file
cnddb <- merge(points_dat, jepCnddb, by.x="EONDX",by.y="EONDX",all.x=T)

## Extracting Federal Lands for CNDDB File (although this is also present in the OWNERMGT field)
b <- over(points_dat,federal_lands)
fedCnddb <-  cbind(as.data.table(points_dat@data)[,.(EONDX)], as.data.table(b))

# Merging Federal Lands onto cndbb file
cnddb <- merge(cnddb, fedCnddb, by.x="EONDX",by.y="EONDX",all.x=T)

write.csv(cnddb,"caprPrioritizationApp/AppData/cnddbwithJepson.csv")

#########################
## Extracting Jepson for Accessions File
capr_noNA <- CaPRAcc[!is.na(decimalLongitude)]
coordinates(capr_noNA) <-  ~decimalLongitude + decimalLatitude

# Transforming Coordinate Reference Systems
proj4string(capr_noNA) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
spTransform(capr_noNA,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extracting ecoregions from jepson layer onto the CNDDB occurrences
a<-over(capr_noNA,jepson)
jepCapr <- cbind(as.data.table(capr_noNA@data)[,.(eventID)], as.data.table(a))

## Extracting Federal Lands for CNDDB File (although this is also present in the OWNERMGT field)
c <- over(capr_noNA,federal_lands)
fedCapr <-  cbind(as.data.table(capr_noNA@data)[,.(eventID)], as.data.table(c))

# Merging Jepson onto overal cnddb file
CaPRAcc <- merge(CaPRAcc, jepCapr, by.x="eventID",by.y="eventID",all.x=T)
CaPRAcc <- merge(CaPRAcc, fedCapr, by.x="eventID",by.y="eventID",all.x=T)

write.csv(CaPRAcc,"caprPrioritizationApp/AppData/CaPRwithJepson.csv")


