# Goal - Clean up maternalLines, basisOfRecord, and have a look at EO index

################## ################## ##################
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
################## ################## ##################

################## ################## ##################
######## Readingin Data from Caspio API ##################
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

# Reading in Seed bank processing table from SDZG seed bank
seedProSDZG <- caspio_get_table("SeedProccesing",login1,1)
TableCombinedSeedProSDZG = NULL
pagenumber=1

while (dim(seedProSDZG)[1]>0)
{
  pagenumber = pagenumber + 1
  TableCombinedSeedProSDZG <- rbind(TableCombinedSeedProSDZG,seedProSDZG)
  seedProSDZG <- caspio_get_table("SeedProccesing",login1,pagenumber)
}

seedProSDZG <- TableCombinedSeedProSDZG
################## ################## ################## ##################


################## ################## ##################
## Cleaning up maternal lines

# Looking at maternal lines by institutions
CaPRAcc[Deaccession==FALSE & grepl("Seed",basisofRecord),.(Count=.N),by=c("preparations","institutionCode")][order(preparations)]

## Calculating maternal lines for SDZG accessions
summarySDZG <- seedProSDZG[,.(preparations=toString(unique(ProcessingType))),by="SeedAccessionID"]
unique(summarySDZG$preparations)
# Standardizing to CaPR database
summarySDZG[grepl("Maternal",preparations),preparations:="maternalLines"]
summarySDZG[grepl("Bulk",preparations),preparations:="bulked"]

# Merging on Seed Bank Data
setkey(CaPRAcc,tgermplasmIdentifier)
setkey(summarySDZG,SeedAccessionID)
CaPRAcc[summarySDZG,`:=`(preparations=i.preparations)]

# Assigning data deficient to RSA seed lines that are NA or blank
CaPRAcc[Deaccession==FALSE & grepl("Seed",basisofRecord) & institutionCode%in%c("RSA","SDZG","SBBG","UCB","BERR","UCSC") & (preparations=="NA"|preparations==''),preparations:="Data deficient" ]
CaPRAcc[Deaccession==FALSE & grepl("Seed",basisofRecord),.(Count=.N),by=c("preparations","institutionCode")][order(preparations)]


################## ################## ##################
## Cleaning up provenance codes
CaPRAcc[Deaccession==FALSE ,.(Count=.N),by=c("biologicalStatus","institutionCode","basisofRecord")][order(biologicalStatus)]

CaPRAcc[!is.na(decimalLatitude) &  biologicalStatus=="", biologicalStatus:="presumed wild"]
CaPRAcc[institutionCode%in%c("SBBG","UCB","UCSC","SDZG","BERR") & biologicalStatus=="",biologicalStatus:="Data deficient"]



