# Matching Genera and Families to Phylomaker Super Tree

# To have as much phylogenetic resolution as possible, we want to the genera from our species list (Jepson) to match the
#synonymous genera in the Phylomaker Super tree. I did this manually for about 150 genera. I was able to reassign about 91 genera. The rest that do not have obvious synonyms will be matched at family level.

############ S Phylomaker  #############
## Read in the megaphylogeny in S Phylomaker
phylo<-read.tree("Data/PhytoPhylo.tre") # read in the megaphylogeny.
nodes <- as.data.table(read_delim("Data/nodes.csv", "\t", escape_double = FALSE,  trim_ws = TRUE))

## Making species list for CaPR species
Spp <- data[,.(Count=.N),by="Taxon Name (Jepson)"]
Spp[,genus:=word(`Taxon Name (Jepson)`,1,1),by="Taxon Name (Jepson)"]

## Finding Family's from genera from phylomaker Nodes file
genusTable = nodes[,.(family=family[1]),by="genus"]

## Merge megatree families onto genera
Spp <- merge(Spp,genusTable,by="genus",all.x=T)
#Spp[genus=="Eremanthus", family:="Asteraceae"]
setnames(Spp,"Taxon Name (Jepson)","species")
Spp <- Spp[,.(species,genus,family)]

# Which genera weren't matched in the mega tree
GenNa <- Spp[is.na(family),.(Count=.N),by="genus"]
JepFam <- data[,.(Count=.N),by=c("Family","genus")]
# Adding families to genera that weren't matched in the super tree
GenNa <- merge(GenNa,JepFam, by="genus")
GenNa[,Count.x:=NULL]
GenNa[,Count.y:=NULL]
GenNa[,Family:=firstup(tolower(Family))]
# Checking which families in the CaPR list are not in the super tree
GenNa[!(Family%in%genusTable$family)][order(Family)]

# Making a few fixes to these famileis
GenNa[Family=="Agavaceae", Family:="Asparagacaeae"]
GenNa[Family=="Chenopodiaceae", Family:="Amaranthaceae"]
GenNa[!(Family%in%genusTable$family),Family:="FernOrNonVascular"]

# Matching genera from CaPR list to genera in the super tree
GenNa[genus=="Atrichoseris", genus:="Malacothrix"]
GenNa[genus=="Aristocapsa", genus:="Chorizanthe"]
GenNa[genus=="Benitoa", genus:="Lessingia"]
GenNa[genus=="Bergerocactus", genus:="Cereus"]
GenNa[genus=="Calliscirpus", genus:="Scirpus"]
GenNa[genus=="Callitropsis", genus:="Cupressus"]
GenNa[genus=="Carlquistia", genus:="Raillardella"]
GenNa[genus=="Carsonia", genus:="Cleome"]
GenNa[genus=="Cascadia", genus:="Saxifraga"]
GenNa[genus=="Centrostegia", genus:="Chorizanthe"]
GenNa[genus=="Chloracantha", genus:="Aster"]
GenNa[genus=="Chloropyron", genus:="Cordylanthus"]
GenNa[genus=="Chylismiella", genus:="Camissonia"]
GenNa[genus=="Condea", genus:="Hyptis"]
GenNa[genus=="Crocanthemum", genus:="Halimium"]
GenNa[genus=="Cuniculotinus", genus:="Petradoria"]
GenNa[genus=="Dasyochloa", genus:="Erioneuron"]
GenNa[genus=="Dicranostegia", genus:="Cordylanthus"]
GenNa[genus=="Diplacus", genus:="Mimulus"]
GenNa[genus=="Dodecahema", genus:="Chorizanthe"]
GenNa[genus=="Eremothera", genus:="Camissonia"]
GenNa[genus=="Erythranthe", genus:="Mimulus"]
GenNa[genus=="Eucephalus", genus:="Aster"]
GenNa[genus=="Euphrosyne", genus:="Iva"]
GenNa[genus=="Extriplex", genus:="Atriplex"]
GenNa[genus=="Gamochaeta", genus:="Gnaphalium"]
GenNa[genus=="Greeneocharis", genus:="Cryptantha"]
GenNa[genus=="Grusonia", genus:="Opuntia"]
GenNa[genus=="Harmonia", genus:="Madia"]
GenNa[genus=="Heliomeris", genus:="Viguiera"]
GenNa[genus=="Hemizonella", genus:="Madia"]
GenNa[genus=="Hemizonia", genus:="Deinandra"]
GenNa[genus=="Herissantia", genus:="Abutilon"]
GenNa[genus=="Hesperocyparis", genus:="Cupressus"]
GenNa[genus=="Horkeliella", genus:="Potentilla"]
GenNa[genus=="Horsfordia", genus:="Abutilon"]
GenNa[genus=="Howellanthus", genus:="Phacelia"]
GenNa[genus=="Ionactis", genus:="Aster"]
GenNa[genus=="Ivesia", genus:="Potentilla"]
GenNa[genus=="Jensia", genus:="Madia"]
GenNa[genus=="Johnstonella", genus:="Cryptantha"]
GenNa[genus=="Kochia", genus:="Bassia"]
GenNa[genus=="Kyhosia", genus:="Madia"]
GenNa[genus=="Landoltia", genus:="Spirodela"]
GenNa[genus=="Lastarriaea", genus:="Chorizanthe"]
GenNa[genus=="Lathrocasis", genus:="Gilia"]
GenNa[genus=="Lepidospartum", genus:="Baccharis"]
GenNa[genus=="Leptosyne", genus:="Coreopsis"]
GenNa[genus=="Leucosyris", genus:="Machaeranthera"]
GenNa[genus=="Listera", genus:="Neottia"]
GenNa[genus=="Malvella", genus:="Sida"]
GenNa[genus=="Maurandella", genus:="Maurandya"]
GenNa[genus=="Micromonolepis", genus:="Monolepis"]
GenNa[genus=="Mitellastra", genus:="Mitella"]
GenNa[genus=="Mucronea", genus:="Chorizanthe"]
GenNa[genus=="Munroa", genus:="Crypsis"]
GenNa[genus=="Munzothamnus", genus:="Stephanomeria"]
GenNa[genus=="Neoholmgrenia", genus:="Camissonia"]
GenNa[genus=="Noccaea", genus:="Thlaspi"]
GenNa[genus=="Oreocarya", genus:="Cryptantha"]
GenNa[genus=="Oreostemma", genus:="Aster"]
GenNa[genus=="Osmadenia", genus:="Calycadenia"]
GenNa[genus=="Oxytheca", genus:="Eriogonum"]
GenNa[genus=="Pediomelum", genus:="Psoralea"]
GenNa[genus=="Pentachaeta", genus:="Chaetopappa"]
GenNa[genus=="Picradeniopsis", genus:="Bahia"]
GenNa[genus=="Piperia", genus:="Platanthera"]
GenNa[genus=="Pityopus", genus:="Monotropa"]
GenNa[genus=="Pleurocoronis", genus:="Hofmeisteria"]
GenNa[genus=="Podistera", genus:="Ligusticum"]
GenNa[genus=="Poteridium", genus:="Sanguisorba"]
GenNa[genus=="Pseudorontium", genus:="Antirrhinum"]
GenNa[genus=="Sabulina", genus:="Minuartia"]
GenNa[genus=="Saxifragopsis", genus:="Saxifraga"]
GenNa[genus=="Sclerocactus", genus:="Echinocactus"]
GenNa[genus=="Scopulophila", genus:="Achyronychia"]
GenNa[genus=="Scribneria", genus:="Lepturus"]
GenNa[genus=="Sericocarpus", genus:="Aster"]
GenNa[genus=="Sibara", genus:="Arabis"]
GenNa[genus=="Spermolepis", genus:="Apium"]
GenNa[genus=="Sphaeromeria", genus:="Tanacetum"]
GenNa[genus=="Stebbinsoseris", genus:="Microseris"]
GenNa[genus=="Stutzia", genus:="Atriplex"]
GenNa[genus=="Systenotheca", genus:="Chorizanthe"]
GenNa[genus=="Taraxia", genus:="Camissonia"]
GenNa[genus=="Tetraneuris", genus:="Hymenoxys"]
GenNa[genus=="Tetrapteron", genus:="Camissonia"]
GenNa[genus=="Tonestus", genus:="Haplopappus"]
GenNa[genus=="Transberingia", genus:="Halimolobos"]
GenNa[genus=="Tripterocalyx", genus:="Abronia"]
GenNa[genus=="Vahlodea", genus:="Aira"]
GenNa[genus=="Yabea", genus:="Caucalis"]

# Update familes in the mega tree list
setkey(Spp, genus)
setkey(GenNa, genus)
Spp[GenNa,`:=` (genus = i.genus,family=i.Family)]

# Limit to Vascular Plants
SppVas <- Spp[family!="FernOrNonVascular"]
#############  #############  #############  #############  #############


# Source the code from Qian & Jin 2016 paper
source("R_codes for S.PhyloMaker.R")

## Bulding Tree for all three scenarios
result_california_tree <- S.PhyloMaker(spList=as.data.frame(SppVas), tree=phylo, nodes=nodes, scenarios=c("S3")) 

