library(data.table)
library(DT)
library(stringr)
library(lubridate)
library(httr)
library(RCurl)
library(rjson)
library(jsonlite)

# Reading in Caspio Token
source("Tokens/caspioSource.R")
# Reading in caspio GET command (don't know if this works for things > 1000)
source("caspioFunctions.R")

# Reading in Data
sdzgOcc <- caspio_get_table_all("Occurrence")
sdzgSpp <- caspio_get_table_all("PlantNames")
caprSpp <- caspio_get_table_all("tblSpeciesJepCAPR")

# Creating a summarized table of occurrence by plantname ID
summ <- sdzgOcc[,.(Count=.N),by="PlantNameID"]

# Mering plant names onto jepIDs
sdzgSppOccOnly <- merge(sdzgSpp,summ,by="PlantNameID")

# Adding plant name IDs onto caprSpp
caprsppWithPNI <- merge(caprSpp,sdzgSppOccOnly[,.(taxonIdCAPR,PlantNameID)],by.x="JepID",by.y="taxonIdCAPR",all.x=T)


setnames(caprsppWithPNI, c("JepID","family","genus","species","infraspecific_rank","infraspecific_epithet","species_author","infraspecific_author","synonyms","common","nativity"), 
         c("taxonIdCAPR","Family","Genus","Species","InfraspecificRank","InfraspecificEpithet","SpeciesAuthor","InfraspecificAuthor","Synonomy","CommonName","Nativity"))

write.csv(caprsppWithPNI,"Data/speciesListUpdate.csv")

