library(data.table)
library(ggplot2)
library(raster)
library(rgdal)


# Read in Data
cnddb <- fread('caprPrioritizationApp/AppData/cnddbwithJepson.csv')
names(cnddb) <- make.unique(names(cnddb))
setnames(cnddb, c("V1.1","V2"),c("decimalLongPolyCent","decimalLatPolyCent"))
capr <- fread('caprPrioritizationApp/AppData/CaPRwithJepson.csv',na.strings=c("",NA,"NA"))
capr <- capr[Deaccession==FALSE  & decimalLatitude>0 & grepl("Seed",basisofRecord)]
cnps <- fread('caprPrioritizationApp/AppData/tblCNPSRanks_2019-Feb-21_2233.csv')

# mergeing on cnps to capr and jepson
 capr <- merge(capr, cnps, by.x="taxonID",by.y="JepID",all.x=T)

# Merging cnddb onto capr
capr[,cnddbEOIndex:=as.integer(cnddbEOIndex)]
# Create a version of the capr dataset that is summarized by EO index
# Creating a variable that can be used as an EO index if an occurrence hasn't been assigned
capr[,EONDXplus:=200000000+eventID[1],by=c("taxonID","institutionCode","eventDate","decimalLatitude","decimalLongitude")]
capr[,EONDXanalysis:=ifelse(is.na(cnddbEOIndex),EONDXplus,cnddbEOIndex)]
caprRareSummary <- capr[(ElementCode%in%unique(cnddb$ELMCODE)) & !is.na(ElementCode),.(CollectsPerEO=.N,ScientificName=ScientificName[1]),by="EONDXanalysis"]

# Creating a dataset that is one observation for every rare plant occurrence, including those that aren't in cnddb
cnddbCaprSumm <- merge(cnddb,caprRareSummary, by.x="EONDX",by.y="EONDXanalysis",all=T)
cnddbCaprSumm <- cnddbCaprSumm[PRESENCE=="Presumed Extant"|EONDX>20000000]
cnddbCaprSumm[is.na(CollectsPerEO),CollectsPerEO:=0]
cnddbCaprSumm[,CollectedYN:=ifelse(CollectsPerEO> 0,"Yes","No")]

# Creating a way to update fields in cnddb from capr
caprEOPlus <- capr[EONDXplus>200000000]
setkey(caprEOPlus,EONDXplus)
setkey(cnddbCaprSumm,EONDX)
cnddbCaprSumm[caprEOPlus,`:=`(SNAME=i.ScientificName,decimalLongPolyCent=i.decimalLongitude,decimalLatPolyCent=i.decimalLatitude,
                              RPLANTRANK=i.CRPR,GRANK=i.GRank,FEDLIST=i.FESA,JEP_REG=i.JEP_REG,JEPCODE=i.JEPCODE,
                              ilmcaPub_1=i.ilmcaPub_1,ilmcaPub_2=i.ilmcaPub_2,ilmcaPub_4=i.ilmcaPub_4,ilmcaPub_5=i.ilmcaPub_5,
                              ilmcaPub_6=i.ilmcaPub_6,ilmcaPub_7=i.ilmcaPub_7,ilmcaPub_8=i.ilmcaPub_8,ilmcaPub_9=i.ilmcaPub_9,ilmcaPub10=i.ilmcaPub10,ilmcaPub11=i.ilmcaPub11,
                              ilmcaPub12=i.ilmcaPub12,ilmcaPub13=i.ilmcaPub13,ilmcaPub14=i.ilmcaPub14,ilmcaPub15=i.ilmcaPub15,ilmcaPub16=i.ilmcaPub16,ilmcaPub17=i.ilmcaPub17,
                              ilmcaPub18=i.ilmcaPub18,ilmcaPub19=i.ilmcaPub19,ilmcaPub20=i.ilmcaPub20)]

# Get Shapefilef for california

us<-getData('GADM', country='USA', level=1)
cali <- subset(us, NAME_1=="California")

# Read in Federal Lands Layer
federal_lands <- readOGR(dsn="Data/FederalLands",layer="Federal_Lands_California")
# Clip Federal Lands by California
#fed_lands_clip <- federal_lands - cali

federal_lands$area_sqkm <- area(federal_lands) / 1000000
federal_lands_dat = as.data.table(federal_lands@data)
federal_lands_dat[is.na(ilmcaPub20),ilmcaPub20:=ilmcaPub_1]
federal_lands_sumPub1 <- federal_lands_dat[,.(aggsumKM = sum(area_sqkm)),by="ilmcaPub_1"]
federal_lands_sumPub20 <- federal_lands_dat[,.(aggsumKM = sum(area_sqkm)),by="ilmcaPub20"]
federal_lands_sumPub19 <- federal_lands_dat[,.(aggsumKM = sum(area_sqkm)),by="ilmcaPub19"]

# Questions I want to answer

# DO I NEED A STATE PARKS LAYER? YES
# From the Federal Lands Layer, ilmcaPub_1 = "ST" is for state entities
# ilmcaPub_6 show these options for state bodies:  
#"Parks and Recreation" "Lands Commission" "Fish and Wildlife" "Wildlife Conservation Board"  "Other State Department"       "Forestry and Fire Protection"
cnddb[PRESENCE=="Presumed Extant",.(OWNERMGT,ilmcaPub_1,ilmcaPub_6,ilmcaPub19 ,ilmcaPub20)]

# DO I NEED A UC RESERVE LAYER? Yes


## What federal lands are best represented in our collection?
## What federal lands are best represented in our collection per unit land area?

# Fixing ilmcaPub20 so that it is not NA for groups without lower subdivision
capr[is.na(ilmcaPub20),ilmcaPub20:=ilmcaPub_1]
caprCountsbyBroadFedGroup <- merge(capr[,.(CollectionsCount=.N),by="ilmcaPub_1"],federal_lands_sumPub1,by="ilmcaPub_1")
caprCountsbyBroadFedGroup[,CountperKM:=CollectionsCount/aggsumKM]
caprCountsbyBroadFedGroup[!is.na(ilmcaPub_1 )][order(-CountperKM)]

caprCountsbyMedFedGroup <- merge(capr[,.(CollectionsCount=.N),by="ilmcaPub20"],federal_lands_sumPub20,by="ilmcaPub20")
caprCountsbyMedFedGroup[,CountperKM:=CollectionsCount/aggsumKM]
caprCountsbyMedFedGroup[!is.na(ilmcaPub20 ) & aggsumKM>200][order(-CountperKM)]

caprCountsbyForestFedGroup <- merge(capr[grepl("Forest",ilmcaPub19) & grepl("200",eventDate),.(CollectionsCount=.N),by="ilmcaPub19"],federal_lands_sumPub19[grepl("Forest",ilmcaPub19),],by="ilmcaPub19",all=T)
caprCountsbyForestFedGroup[,CountperKM:=CollectionsCount/aggsumKM]
caprCountsbyForestFedGroup[][order(-CollectionsCount)]


## What proprotion of presumed extant rare plant occurrences have been collected on each federal land type? 
# Use CNDDB fields for dataset, add in fields for 


## What proportion of presumed extant rare plant occurrences have been colelcted on each management unit?

# What specific management units have the most uncollected species (most bang for buck in permissions)?