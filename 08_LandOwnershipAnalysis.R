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
cnddbCaprSumm[,CollectedYN:=ifelse(CollectsPerEO> 0,1,0)]

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

# Create indicators for different landownerships, combining ownership notes from CNDDB and fedearl land ownership layer
cnddbCaprSumm[,BLMind:=0]
cnddbCaprSumm[grepl("BLM",OWNERMGT)| ilmcaPub_1=="BLM",BLMind:=1]
cnddbCaprSumm[,NPSind:=0]
cnddbCaprSumm[grepl("NPS",OWNERMGT)| ilmcaPub_1=="NPS",NPSind:=1]
cnddbCaprSumm[,USFSind:=0]
cnddbCaprSumm[grepl("USFS",OWNERMGT)| ilmcaPub_1=="USFS",USFSind:=1]
cnddbCaprSumm[,DODind:=0]
cnddbCaprSumm[grepl("DOD-",OWNERMGT)| ilmcaPub_1%in%c("DOD","ARMY","NAVY","USAF","USMC","USCG"),DODind:=1]
cnddbCaprSumm[,FWSind:=0]
cnddbCaprSumm[grepl("USFWS",OWNERMGT)| ilmcaPub_1%in%c("FWS"),FWSind:=1]
cnddbCaprSumm[,BIAind:=0]
cnddbCaprSumm[grepl("BIA",OWNERMGT)| ilmcaPub_1%in%c("BIA"),BIAind:=1]
cnddbCaprSumm[,LOCALind:=0]
cnddbCaprSumm[grepl("CITY",OWNERMGT)|grepl("COUNTY",OWNERMGT)|grepl("EBRPD",OWNERMGT)|grepl("LADWP",OWNERMGT)|grepl("MRCA",OWNERMGT)|grepl("MWD",OWNERMGT)|grepl("EBMUD",OWNERMGT)|grepl("WATER DIST",OWNERMGT)| ilmcaPub_1%in%c("LG"),LOCALind:=1]
cnddbCaprSumm[,STind:=0]
cnddbCaprSumm[grepl("DFG",OWNERMGT)|grepl("STATE",OWNERMGT)|grepl("UCNR",OWNERMGT)|grepl("DPR",OWNERMGT)|grepl("CALTRANS",OWNERMGT)|grepl("CDF-",OWNERMGT)|grepl("CSU",OWNERMGT)|grepl("DWR",OWNERMGT)|grepl("UC-",OWNERMGT)| ilmcaPub_1%in%c("ST")|grepl("MIDPENINSULA REGIONAL OSD",OWNERMGT),STind:=1]
cnddbCaprSumm[,PVTind:=0]
cnddbCaprSumm[grepl("PVT",OWNERMGT)|ilmcaPub_1=="PVT",PVTind:=1]
cnddbCaprSumm[,NFPind:=0]
cnddbCaprSumm[grepl("TNC-",OWNERMGT)|OWNERMGT=="SOLANO LAND TRUST"|OWNERMGT=="THE WILDLANDS CONSERVANCY "|grepl("TNC-",OWNERMGT)|grepl("AUDUBON",OWNERMGT)|grepl("FOUNDATION",OWNERMGT)|grepl("LAND TRUST",OWNERMGT),NFPind:=1]
cnddbCaprSumm[,OTHERInd:=ifelse((BLMind==0 & NPSind==0 & USFSind==0 & DODind==0 & FWSind==0 & BIAind==0 & LOCALind==0 & STind==0 & PVTind==0 & NFPind==0), 1,0)]

# Creating a clean version of a high order lands category from the federal lands layer
cnddbCaprSumm[,BroadestOwnership:=ilmcaPub_1]
cnddbCaprSumm[ilmcaPub_1%in%c("ARMY","DOD","NAVY","USACE","USAF","USCG","USMC"),BroadestOwnership:="DOD"]
cnddbCaprSumm[ilmcaPub20%in%c("National Forest"),BroadestOwnership:="USFS"]
cnddbCaprSumm[ilmcaPub20%in%c("National Park"),BroadestOwnership:="NPS"]
cnddbCaprSumm[ilmcaPub20%in%c("American Indian Reservation"),BroadestOwnership:="BIA"]
cnddbCaprSumm[ilmcaPub20%in%c("Marine Corps Base"),BroadestOwnership:="DOD"]
cnddbCaprSumm[is.na(BroadestOwnership)|BroadestOwnership%in%c("OTHFE","UND","USBR"),BroadestOwnership:="OTHER"]
cnddbCaprSumm[USFSind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="USFS"]
cnddbCaprSumm[NPSind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="NPS"]
cnddbCaprSumm[BLMind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="BLM"]
cnddbCaprSumm[DODind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="DOD"]
cnddbCaprSumm[BIAind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="BIA"]
cnddbCaprSumm[PVTind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="PVT"]
cnddbCaprSumm[grepl("PVT IN",OWNERMGT),BroadestOwnership:="PVT"]
cnddbCaprSumm[FWSind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="FWS"]
cnddbCaprSumm[STind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="ST"]
cnddbCaprSumm[LOCALind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="LG"]
cnddbCaprSumm[NFPind==1 & BroadestOwnership=="OTHER",BroadestOwnership:="NFP"]
cnddbCaprSumm[OWNERMGT=="UNKNOWN" & BroadestOwnership=="OTHER",BroadestOwnership:="UNKNOWN"]

# Fixing the forest columns
cnddbCaprSumm[BroadestOwnership=="USFS",forest:=ilmcaPub19]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("LOS PADRES",OWNERMGT),forest:="Los Padres National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("SAN BERNARDINO",OWNERMGT),forest:="San Bernardino National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("LASSEN",OWNERMGT),forest:="Lassen National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("STANISLAUS",OWNERMGT),forest:="Stanislaus National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("TAHOE NF",OWNERMGT),forest:="Tahoe National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("PLUMAS",OWNERMGT),forest:="Plumas National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("SEQUOIA",OWNERMGT),forest:="Sequoia National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("LAKE TAHOE BMU",OWNERMGT),forest:="Lake Tahoe Basin Management Unit"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("SHASTA-TRINITY",OWNERMGT),forest:="Shasta-Trinity National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("KLAMATH",OWNERMGT),forest:="Klamath National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("ELDORADO",OWNERMGT),forest:="Eldorado National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("CLEVELAND",OWNERMGT),forest:="Cleveland National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("ANGELES",OWNERMGT),forest:="Angeles National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("MENDOCINO",OWNERMGT),forest:="Mendocino National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("SIERRA",OWNERMGT),forest:="Sierra National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" & grepl("SIX RIVERS",OWNERMGT),forest:="Six Rivers National Forest"]
cnddbCaprSumm[is.na(forest) & BroadestOwnership=="USFS" ,forest:="Notspecified Forest"]


# Calculating the probability that an EO has been collected given land ownership status given latitude and longitude
modelLandEO <- glm(as.integer(CollectedYN)~decimalLongPolyCent+decimalLatPolyCent+(BLMind)+(NPSind)+USFSind+DODind+FWSind+BIAind+LOCALind+STind+PVTind+OTHERInd,data=cnddbCaprSumm,family="binomial")
modelLandEOAsOneVariable <- glm(as.integer(CollectedYN)~decimalLongPolyCent+decimalLatPolyCent+BroadestOwnership,data=cnddbCaprSumm,family="binomial")
summary(modelLandEOAsOneVariable)
modelLandEOAsOneVariable1B <- glm(as.integer(CollectedYN)~decimalLongPolyCent+decimalLatPolyCent+BroadestOwnership,data=cnddbCaprSumm[grepl("1B",RPLANTRANK)],family="binomial")
summary(modelLandEOAsOneVariable1B)

# Finding the percent EOs collected on each land type
landEOSumm <- cnddbCaprSumm[,.(CountEOs = .N, EOsCollected = sum(1*CollectedYN==1)),by="BroadestOwnership"]
landEOSumm[,PercentCollected:=EOsCollected/CountEOs*100]
# Finding the percent of 1B EOs collected on each land type
cnddbCaprSumm[,CollsOfSppAnywhere:=sum(1*CollectedYN==1),by="SNAME"]
cnddbCaprSumm[,SppCollAnywhereYN:=ifelse(CollsOfSppAnywhere==0,0,1)]
landEOSumm1B <- cnddbCaprSumm[grepl("1B",RPLANTRANK),.(CountEOs = .N, EOsCollected = sum(1*CollectedYN==1),CountSpecies = uniqueN(SNAME), SppCollectedOnLandOwn =uniqueN(SNAME[CollectedYN==1]),SppCollectedAnywhere=uniqueN(SNAME[SppCollAnywhereYN==1])),by="BroadestOwnership"]
landEOSumm1B[,UnCollectedSpecies:=CountSpecies-SppCollectedAnywhere]
landEOSumm1B[,SppCollectedElseWhere:=SppCollectedAnywhere-SppCollectedOnLandOwn]
landEOSumm1B[,PercentEOsCollected:=EOsCollected/CountEOs*100]
landEOSumm1B[,PercentSppCollectedOnLand:=SppCollectedOnLandOwn/CountSpecies*100]
landEOSumm1B[,PercentSppCollectedAnywhere:=SppCollectedAnywhere/CountSpecies*100]
landEOSumm1B[,EOsPerSpp:=CountEOs/CountSpecies]
landEOSumm1B[,EOsUncollected:=CountEOs-EOsCollected]
landEOSumm1B[][order(-UnCollectedSpecies)]
# Melting this dataset down for future plots
landEOSumm1BMelt <- melt(landEOSumm1B, id.vars="BroadestOwnership",measure.vars = 2:length(names(landEOSumm1B)))



# Plotting Percent of 1B EOs collected
p <- ggplot(data=landEOSumm1B[!(BroadestOwnership%in%c("UNKNOWN","OTHER"))], aes(x=reorder(BroadestOwnership,-PercentEOsCollected), y=PercentEOsCollected))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p + xlab("Government Agency") + ylab("Percent of 1B EOs Collected")
p

# Make plots that show collections of EOs and species collected as stacked
p <- ggplot(landEOSumm1BMelt[BroadestOwnership!="UNKNOWN" & variable%in%c("EOsUncollected","EOsCollected")], aes(x=reorder(BroadestOwnership,-value), y=value, fill=variable))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p + xlab("Government Agency") + ylab("# Of 1B Element Occurrences")
p


# Plotting Number of Uncollected Species
p <- ggplot(data=landEOSumm1B[BroadestOwnership!="UNKNOWN"], aes(x=reorder(BroadestOwnership,-UnCollectedSpecies), y=UnCollectedSpecies))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p + xlab("Government Agency") + ylab("# Of 1B Species In Need of Collection")
p

# Showing how species collected on each land type break down
p <- ggplot(landEOSumm1BMelt[BroadestOwnership!="UNKNOWN" & variable%in%c("UnCollectedSpecies","SppCollectedOnLandOwn","SppCollectedElseWhere")], aes(x=reorder(BroadestOwnership,-value), y=value, fill=reorder(variable,value)))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p + xlab("Government Agency") + ylab("# 1B Species Occurring on Land Type")
p


### FOREST SERVICE
# Create the same species and EO plots for individual forests
forestEO <- cnddbCaprSumm[grepl("1B",RPLANTRANK) & BroadestOwnership=="USFS",.(CountEOs = .N, EOsCollected = sum(1*CollectedYN==1),CountSpecies = uniqueN(SNAME), SppCollectedOnLandOwn =uniqueN(SNAME[CollectedYN==1]),SppCollectedAnywhere=uniqueN(SNAME[SppCollAnywhereYN==1])),by="forest"]
forestEO[,UnCollectedSpecies:=CountSpecies-SppCollectedAnywhere]
forestEO[,SppCollectedElseWhere:=SppCollectedAnywhere-SppCollectedOnLandOwn]
forestEO[,PercentEOsCollected:=EOsCollected/CountEOs*100]
forestEO[,PercentSppCollectedOnLand:=SppCollectedOnLandOwn/CountSpecies*100]
forestEO[,PercentSppCollectedAnywhere:=SppCollectedAnywhere/CountSpecies*100]
forestEO[,EOsPerSpp:=CountEOs/CountSpecies]
forestEO[,EOsUncollected:=CountEOs-EOsCollected]
# Melting
forestEOMelt <- melt(forestEO, id.vars="forest",measure.vars = 2:length(names(forestEO)))
forestEOMelt[,forest:=gsub("National Forest","",forest)]

# Plotting Percent of 1B EOs collected
p <- ggplot(data=forestEO, aes(x=reorder(forest,-PercentEOsCollected), y=PercentEOsCollected))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p + xlab("National Forest") + ylab("Percent of 1B EOs Collected")
p

# Make plots that show collections of EOs and species collected as stacked
p <- ggplot(forestEOMelt[ variable%in%c("EOsUncollected","EOsCollected")], aes(x=reorder(forest,-value), y=value, fill=variable))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p + xlab("National Forest") + ylab("# Of 1B Element Occurrences")
p

# Plotting Number of Uncollected Species
p <- ggplot(data=forestEOMelt, aes(x=reorder(forest,-UnCollectedSpecies), y=UnCollectedSpecies))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p + xlab("National Forest") + ylab("# Of 1B Species In Need of Collection")
p

# Showing how species collected on each land type break down
p <- ggplot(forestEOMelt[variable%in%c("UnCollectedSpecies","SppCollectedOnLandOwn","SppCollectedElseWhere")], aes(x=reorder(forest,-value), y=value, fill=reorder(variable,value)))+geom_bar(stat="identity",colour="black")
p <- p + theme_classic(base_size=16)
p <- p +  xlab("National Forest") + ylab("# 1B Species Occurring on NF")
p <- p + theme(axis.text.x = element_text(angle = 45,hjust=1))
p

# Get Shapefilef for california

us<-getData('GADM', country='USA', level=1)
cali <- subset(us, NAME_1=="California")

# Read in Federal Lands Layer
federal_lands <- readOGR(dsn="Data/FederalLands",layer="Federal_Lands_California")
# Clip Federal Lands by California
#fed_lands_clip <- federal_lands - cali

federal_lands$area_sqkm <- area(federal_lands) / 1000000
federal_lands_dat = as.data.table(federal_lands@data)
federal_lands_dat[,BroadestOwnership:=ilmcaPub_1]
federal_lands_dat[ilmcaPub_1%in%c("ARMY","DOD","NAVY","USACE","USAF","USCG","USMC"),BroadestOwnership:="DOD"]
federal_lands_dat[ilmcaPub20%in%c("National Forest"),BroadestOwnership:="USFS"]
federal_lands_dat[ilmcaPub20%in%c("National Park"),BroadestOwnership:="NPS"]
federal_lands_dat[ilmcaPub20%in%c("American Indian Reservation"),BroadestOwnership:="BIA"]
federal_lands_dat[ilmcaPub20%in%c("Marine Corps Base"),BroadestOwnership:="DOD"]
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