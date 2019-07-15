library(shiny)
library(ggtree)
library(dqshiny)
library(data.table)
library(diversitree)
library(shinythemes)
library(rsconnect)
library(picante)
library(caper)
library(venneuler)

# Reading in species level data (no jepson ID)
MatchDataObj<- readRDS("AppData/MatchDataObj.rds")
MatchDataObj$data[is.na(MatchDataObj$data$Counties),]$Counties <- ""
CountyCodes <- fread("AppData/tblCNPSCountyCodes_2019-Jun-21_1830.csv")

# Making phylogoetic tree stuff in the correct format
MatchDataObj$data$AnyCollection <- as.integer(MatchDataObj$data$AnyCollection)-1
MatchDataObj$data$InSeedCollection <- as.integer(MatchDataObj$data$InSeedCollection)-1
MatchDataObj$data$InLivingCollection <- as.integer(MatchDataObj$data$InLivingCollection)-1

# Reading in Jespon Species
Spp <- fread("AppData/JepSpeviesJul2019.csv")

# Reading in accessions data
cnddb <- fread("AppData/cnddbwithJepson.csv")
capr <- fread("AppData/CaPRwithJepson.csv")
capr <- capr[Deaccession==-0]
cnps <- fread("AppData/tblCNPSRanks_2019-Feb-21_2233.csv")

## Classifying accessions by quality
# Tier One: Wild provenance, known origin (lat/long or EOindex), maternallines/conservation quality
capr[biologicalStatus=="wild" & (!is.na(decimalLatitude)|!is.na(cnddbEOIndex)) & preparations%in%c("maternalLines","conservation quality"),conservationClassification:="Tier01"]
# Tier Two: wild provenance, known origin (lat/long or EOindex), bulked or data deficient
capr[is.na(conservationClassification)& biologicalStatus%in%c("wild","cultivated from wild source") & basisofRecord=="Seed" & (!is.na(decimalLatitude)|!is.na(cnddbEOIndex) & preparations%in%c("bulked","","Data deficient")),conservationClassification:="Tier02"]
# Tier Three: other seed collections
capr[is.na(conservationClassification)&  basisofRecord=="Seed",conservationClassification:="Tier03"]
# Tier Four: wild provenanced living collections (not specifically marked as conservation quality)
capr[is.na(conservationClassification)&  basisofRecord=="Living" & biologicalStatus%in%c("wild","cultivated from wild source"),conservationClassification:="Tier04"]
# Tier Five: any wild provenanced collection
capr[is.na(conservationClassification),conservationClassification:="Tier05"]

## Create Toable
caprSppTable <- capr[,  .(
                        collectionTypes=toString(sort(unique(conservationClassification))),
                        countTierOne = uniqueN(cnddbEOIndex[conservationClassification=="Tier01"]),
                        countTierFour = uniqueN(eventID[conservationClassification=="Tier04"]),
                        seedCollections=sum(1*(grepl("Seed",basisofRecord))),
                        matLinesSeedCollections = sum(1*preparations%in%c("maternalLines","conservation quality")),
                        aggregateSeedCount=sum((as.numeric(organismQuantity)),na.rm=T),
                        aggregateMaternalLineCount = sum(as.numeric(maternalLines),na.rm=T),
                        ecoRegionsCollected = toString(unique(JEP_REG[!is.na(JEP_REG)]))
                        ), by=c("taxonID")]

# Merge Table with speices
caprSppTable <- merge(caprSppTable, Spp[,.(JepID, name_minus_authors, common)], by.x='taxonID',by.y='JepID', all.x=T)

# Summarzing cnddb data at the species level
cnddb_summ <- cnddb[,.(JepsonRegionsExsiting = toString(unique(JEP_REG))),by="ElmCode"]
cnddb_summ <- merge(cnddb_summ, cnps[,.(ElementCode,CRPR, JepID)],by.x="ElmCode",by.y="ElementCode",all.x=T)

# Merging cnddb table and capr Table
caprSppTable <- merge(caprSppTable,cnddb_summ,by.x="taxonID",by.y="JepID",all.x=T)
# calculating which ecoregions are not collected 
caprSppTable[,JepsonRegionsLeft:= toString(trimws(unlist(strsplit(JepsonRegionsExsiting, ",")))[!(trimws(unlist(strsplit(JepsonRegionsExsiting, ",")))%in%trimws(unlist(strsplit(ecoRegionsCollected, ","))))]),by="taxonID"]
# Simplifying CRPR
caprSppTable[CRPR%in%c("1B.1","1B.2","1B.3"),CRPR_simple:="1B"]
caprSppTable[CRPR%in%c("2B.1","2B.2","2B.3"),CRPR_simple:="2B"]
caprSppTable[CRPR%in%c("3","3.1","3.2","3.3"),CRPR_simple:="3"]
caprSppTable[CRPR%in%c("4","4.1","4.2","4.3"),CRPR_simple:="4"]


             
# Finding the "best ever collections"
caprSppTable[countTierOne>=5 & aggregateSeedCount>3000 & ecoRegionsCollected==JepsonRegionsExsiting, SppRank:="MeetsCPCGoal"]
caprSppTable[is.na(SppRank) & countTierOne>1 , SppRank:="PrettyGood"]
caprSppTable[SppRank=="PrettyGood"]

#objSeed <- phylo.d(MatchDataObj$data, MatchDataObj$phy, names.col=nameOnPhylogeny, binvar=InSeedCollection, permut = 100, rnd.bias=NULL)
#objLiving <- phylo.d(MatchDataObj$data, MatchDataObj$phy, names.col=nameOnPhylogeny, binvar=InLivingCollection, permut = 100, rnd.bias=NULL)

# 
# VenDat <- caprSppTable[,.(Count=.N),by="collectionTypes"]
# VenDat[,collectionTypes:=gsub(", ","&",collectionTypes,fixed=T)] 
# VenVec <- VenDat$Count
# names(VenVec) <- VenDat$collectionTypes
# 
# plot(venneuler(VenVec))





# # Playing with examples
# 
# famFilter <- which(grepl("Lamiaceae",as.character(MatchDataObj$data$family)))
# #countyFilter <- which(grepl("SDG",as.character(MatchDataObj$data$Counties)))
# instFilter <- which(grepl("SDZG",as.character(MatchDataObj$data$institutions)))
# 
# MatchDataObj$data[Reduce(intersect, list(famFilter,countyFilter,instFilter)),]


