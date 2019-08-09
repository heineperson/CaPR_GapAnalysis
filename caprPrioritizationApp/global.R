library(BiocManager)
options(repos = BiocManager::repositories())
library(shiny)
library(ggtree)
library(dqshiny)
library(data.table)
library(diversitree)
library(shinythemes)
library(rsconnect)
library(picante)
library(caper)
library(eulerr)
library(Cairo)
library(phylocanvas)
library(dplyr)
library(tidytree)
library(sensiPhy)
library(PKI)


# Reading in species level data (no jepson ID)
MatchDataObj<- readRDS("AppData/MatchDataObj.rds")
MatchDataObj$data[is.na(MatchDataObj$data$Counties),]$Counties <- ""
CountyCodes <- fread("AppData/tblCNPSCountyCodes_2019-Jun-21_1830.csv")

# Making phylogoetic tree stuff in the correct format
MatchDataObj$data$AnyCollection <- as.integer(MatchDataObj$data$AnyCollection)-1
MatchDataObj$data$InSeedCollection <- as.integer(MatchDataObj$data$InSeedCollection)-1
MatchDataObj$data$InLivingCollection <- as.integer(MatchDataObj$data$InLivingCollection)-1

#lim <- MatchDataObj$data[,c(1,13,14)]
#objSeed <- phylo.d(lim, MatchDataObj$phy, names.col=nameOnPhylogeny, binvar=InSeedCollection, permut = 50, rnd.bias=NULL)
#objLiving <- phylo.d(MatchDataObj$data[,c(1,13,14)], MatchDataObj$phy, names.col=nameOnPhylogeny, binvar=InLivingCollection, permut = 1000, rnd.bias=NULL)


# Calcalculate evoluationary distinctiveness for each species
# EvDist <- evol.distinct(MatchDataObj$phy, type = c("equal.splits", "fair.proportion"),
#               scale = FALSE, use.branch.lengths = TRUE)
# MatchDataObj$data$evolDist <- EvDist
# saveRDS(MatchDataObj,"AppData/MatchDataObj.rds")
# 

# treeTibble <- as_tibble(MatchDataObj$phy)
# dataTibble <- as_tibble(MatchDataObj$data)
# dataTibble$nameOnPhylogeny = as.character(dataTibble$nameOnPhylogeny)
# y <- full_join(treeTibble, dataTibble, by  = c("label" = "nameOnPhylogeny"))
# treeObj <- as.treedata(y)
# 
# 

# Reading in Jespon Species
Spp <- fread("AppData/JepSpeviesJul2019.csv")

# Reading in accessions data
cnddb <- fread("AppData/cnddbwithJepson.csv")
capr <- fread("AppData/CaPRwithJepson.csv")
capr <- capr[Deaccession==0]
capr[,organismQuantity:=as.numeric(organismQuantity)]
capr[biologicalStatus=="unknown"& !is.na(cnddbEOIndex),biologicalStatus:="presumed wild"]
cnps <- fread("AppData/tblCNPSRanks_2019-Feb-21_2233.csv")

## Classifying accessions by quality
# Tier One: Wild provenance, known origin (lat/long or EOindex), maternallines/conservation quality
capr[,LocationKnowledge:=ifelse((decimalLatitude>0 | as.numeric(cnddbEOIndex)>0 ),"locationKnown","locationUnknown")]
capr[is.na(LocationKnowledge),LocationKnowledge:="locationUnknown"]
capr[biologicalStatus%in%c("wild","cultivated from wild source","presumed wild","presumed cultivated from wild source") & LocationKnowledge=="locationKnown" & preparations%in%c("maternalLines","conservation quality"),conservationClassification:="Conservation Collection (Maternal Lines)"]
# Tier Two: wild provenance, known origin (lat/long or EOindex), bulked or data deficient
capr[is.na(conservationClassification)& grepl("Seed",basisofRecord) & (LocationKnowledge=="locationKnown"| biologicalStatus!="Data deficient") ,conservationClassification:="Seed: Bulked And Wild"]
# Tier Three: other seed collections
capr[is.na(conservationClassification)&  grepl("Seed",basisofRecord),conservationClassification:="Seed: Data Deficient"]
# Tier Four: wild provenanced living collections (not specifically marked as conservation quality)
capr[is.na(conservationClassification)&  basisofRecord=="Living" & biologicalStatus%in%c("wild","cultivated from wild source"),conservationClassification:="Living: Wild Prov"]
# Tier Five: any other collection
capr[is.na(conservationClassification),conservationClassification:="Living: Unknown Prov"]

## Create Toable
caprSppTable <- capr[,  .(
                        collectionTypes=toString(sort(unique(conservationClassification))),
                        collectionTypesSeed=toString(sort(unique(conservationClassification[grepl("Seed",conservationClassification)|grepl("Maternal",conservationClassification)]))),
                        countTierOne = uniqueN(cnddbEOIndex[conservationClassification=="Conservation Collection (Maternal Lines)"]),
                        countTierFour = uniqueN(eventID[conservationClassification=="Living: Wild Prov"]),
                        seedCollections=sum(1*(grepl("Seed",basisofRecord))),
                        matLinesSeedCollections = sum(1*preparations%in%c("maternalLines","conservation quality")),
                        aggregateSeedCount=sum((as.numeric(organismQuantity)),na.rm=T),
                        aggregateMaternalLineCount = sum((as.numeric(maternalLines)),na.rm=T),
                        ecoRegionsCollected = toString(unique(JEP_REG[!is.na(JEP_REG)]))
                        ), by=c("taxonID")]



# Merge Table with speices
caprSppTable <- merge(caprSppTable, Spp[,.(JepID, name_minus_authors, common)], by.x='taxonID',by.y='JepID', all=T)
caprSppTable[is.na(collectionTypes),collectionTypes:="Uncollected"]
caprSppTable[is.na(collectionTypesSeed)|collectionTypesSeed=="",collectionTypesSeed:="Uncollected"]

# Summarzing cnddb data at the species level
cnddb_summ <- cnddb[,.(JepsonRegionsExsiting = toString(unique(JEP_REG))),by="ELMCODE"]
cnddb_summ <- merge(cnddb_summ, cnps[,.(ElementCode, JepID)],by.x="ELMCODE",by.y="ElementCode",all.x=T)

# Merging cnddb table and capr Table
caprSppTable <- merge(caprSppTable,cnddb_summ,by.x="taxonID",by.y="JepID",all.x=T)
caprSppTable <- merge(caprSppTable,cnps[,.(CRPR, JepID)],by.x="taxonID",by.y="JepID",all.x=T)
# calculating which ecoregions are not collected 
caprSppTable[,JepsonRegionsLeft:= toString(trimws(unlist(strsplit(JepsonRegionsExsiting, ",")))[!(trimws(unlist(strsplit(JepsonRegionsExsiting, ",")))%in%trimws(unlist(strsplit(ecoRegionsCollected, ","))))]),by="taxonID"]
# Simplifying CRPR
caprSppTable[CRPR%in%c("1B.1","1B.2","1B.3"),CRPR_simple:="1B"]
caprSppTable[CRPR%in%c("2B.1","2B.2","2B.3"),CRPR_simple:="2B"]
caprSppTable[CRPR%in%c("3","3.1","3.2","3.3"),CRPR_simple:="3"]
caprSppTable[CRPR%in%c("4","4.1","4.2","4.3"),CRPR_simple:="4"]

 caprSppTable[grepl("Maternal",collectionTypes),topCollectionTypes:="01-Maternal Lines And Wild"]
 caprSppTable[is.na(topCollectionTypes) & grepl("Seed: Bulked",collectionTypes),topCollectionTypes:="02-Seed Bulked"]
 caprSppTable[is.na(topCollectionTypes) & grepl("Seed: Data",collectionTypes),topCollectionTypes:="03-Seed Data Deficient"]
 caprSppTable[is.na(topCollectionTypes) & grepl("Living: Wild",collectionTypes),topCollectionTypes:="04-Living Wild"]
 caprSppTable[is.na(topCollectionTypes) & grepl("Living: ",collectionTypes),topCollectionTypes:="05-Living Unknown"]
 caprSppTable[is.na(topCollectionTypes) ,topCollectionTypes:="06-Not Collected"]
 

# Finding the "best ever collections"
caprSppTable[countTierOne>=5 & aggregateSeedCount>3000 & ecoRegionsCollected==JepsonRegionsExsiting, SppRank:="MeetsCPCGoal"]
caprSppTable[is.na(SppRank) & countTierOne>1 , SppRank:="PrettyGood"]
caprSppTable[SppRank=="PrettyGood"]

# # Making a second table that can be used for venn diagram
# caprSppTable2 <- capr[,  .(
#   collectionTypes=toString(sort(unique(conservationClassification[grepl("Seed",conservationClassification)|grepl("Maternal",conservationClassification)]))),
#   countTierOne = uniqueN(cnddbEOIndex[conservationClassification=="MaternalLines And Wild"]),
#   countTierFour = uniqueN(eventID[conservationClassification=="Living: Wild Prov"]),
#   seedCollections=sum(1*(grepl("Seed",basisofRecord))),
#   matLinesSeedCollections = sum(1*preparations%in%c("maternalLines","conservation quality")),
#   aggregateSeedCount=sum((as.numeric(organismQuantity)),na.rm=T),
#   aggregateMaternalLineCount = sum((as.numeric(maternalLines)),na.rm=T),
#   ecoRegionsCollected = toString(unique(JEP_REG[!is.na(JEP_REG)]))
# ), by=c("taxonID")]
# 
# caprSppTable2[collectionTypes=="",collectionTypes:="Uncollected"]
# 
# # Merge Table with speices
# caprSppTable2 <- merge(caprSppTable2, Spp[,.(JepID, name_minus_authors, common)], by.x='taxonID',by.y='JepID', all=T)
# caprSppTable2[is.na(collectionTypes),collectionTypes:="Uncollected"]
# 
# 
# # Merging cnddb table and capr Table
# caprSppTable2 <- merge(caprSppTable2,cnddb_summ,by.x="taxonID",by.y="JepID",all.x=T)
# caprSppTable2 <- merge(caprSppTable2,cnps[,.(CRPR, JepID)],by.x="taxonID",by.y="JepID",all.x=T)
# 
# 

  
#   VenDat <- caprSppTable[,.(Count=.N),by="collectionTypesSeed"]
#   VenDat[,collectionTypesSeed:=gsub(", ","&",collectionTypesSeed,fixed=T)] 
#   VenVec <- VenDat$Count
#   names(VenVec) <- VenDat$collectionTypes
# # #
#   plot(euler(VenVec))
#

# caprSppTable[grepl("Quercus",name_minus_authors),.(name_minus_authors,CRPR)]
# caprSppTable[grepl("Quercus",name_minus_authors) & !grepl("var.",name_minus_authors) & !grepl(" x ",name_minus_authors) ,.(name_minus_authors,CRPR)]
# 
# caprSppTable[grepl("Quercus",name_minus_authors) & !grepl("var.",name_minus_authors)  ,.(name_minus_authors,CRPR)]
# 
# # Playing with examples
# 
# famFilter <- which(grepl("Lamiaceae",as.character(MatchDataObj$data$family)))
# #countyFilter <- which(grepl("SDG",as.character(MatchDataObj$data$Counties)))
# instFilter <- which(grepl("SDZG",as.character(MatchDataObj$data$institutions)))
# 
# MatchDataObj$data[Reduce(intersect, list(famFilter,countyFilter,instFilter)),]


