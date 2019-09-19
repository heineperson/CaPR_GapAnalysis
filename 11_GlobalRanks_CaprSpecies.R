library(data.table)

ns <- fread("Data/tblNatureServe_2019-Sep-06_2005.csv")
jep <- fread("Data/tblSpeciesJepCAPR_2019-Sep-06_2128.csv")
acc <- fread("Data/tblAccessionsCAPR_2019-Jun-21_1829.csv")
jepSyn <- fread("Data/JepsonSynonyms2017.csv")
jepSyn[,SynonymNS:=gsub("subsp.","ssp.",Synonym,fixed=T)]

# Fixing discrepancies
jep[,nameNS:=gsub("subsp.","ssp.",name_minus_authors,fixed=T)]
jep[name_minus_authors=="Stipa lemmonii var. pubescens",nameNS:="Achnatherum lemmonii var. pubescens"]
jep[grepl("Castilleja ambigua",name_minus_authors) & !grepl("mead",name_minus_authors),nameNS:=gsub("var.","ssp.",nameNS, fixed=T)]
#jep[name_minus_authors=="Agave shawii var. shawii",nameNS:="Agave shawii"]

# Creating list with natureserve ranks that can be uploaded
ns_globRareCA <- ns[grepl("CA",US_STATES_AND_S_RANKS) & NATURESERVE_ROUNDED_G_RANK%in%c("T1","T2","G1","G2"),.(SCIENTIFIC_NAME,NATURESERVE_ROUNDED_G_RANK,US_STATES_AND_S_RANKS,NATURESERVE_ID)]
ns_globRareCA_jep <- merge(ns_globRareCA,jep[,.(nameNS,JepsonName=name_minus_authors,JepID)],by.x="SCIENTIFIC_NAME",by.y="nameNS",all.x=T)

# Defining exclusion reasions
ns_globRareCA_jep[,reviewStatus:="None"]


# Trying to add on synonyms
synonymMerge <- merge(ns_globRareCA_jep[is.na(JepID)],jepSyn, by.x="SCIENTIFIC_NAME",by.y="SynonymNS")
synonymMerge[,V1:=NULL]

# Update with synonym merge
setkey(ns_globRareCA_jep, SCIENTIFIC_NAME)
setkey(synonymMerge,SCIENTIFIC_NAME)

ns_globRareCA_jep[synonymMerge,`:=`(JepID=JepID.y, JepsonName=JepAcceptedName,reviewStatus="Added from Synonym")]




# Species that should not be considered G1/G2 according to eflora
ns_globRareCA_jep[SCIENTIFIC_NAME%in%c("Abronia umbellata ssp. breviflora",
                                       "Acer glabrum var. greenei",
                                       "Achillea millefolium var. arenicola",
                                       "Achillea millefolium var. gigantea",
                                       "Achillea millefolium var. puberula",
                                       "Agave shawii",
                                       "Agoseris heterophylla var. turgida",
                                       "Allium bolanderi var. stenanthum",
                                       "Agrostis clivicola var. punta-reyesensis",
                                       "Arabis glabra var. furcatipilis",
                                       "Arabis suffrutescens var. horizontalis",
                                       "Argemone munita ssp. robusta",
                                       "Arnica chamissonis var. bernardina",
                                       "Balsamorhiza macrolepis var. macrolepis",
                                       "Calochortus elegans var. nanus",
                                       "Calochortus elegans var. oreophilus",
                                       "Calystegia occidentalis var. tomentella",
                                       "Calycadenia truncata ssp. microcephala",
                                       "Ceanothus connivens",
                                       "Cardamine californica var. cuneata",
                                       "Ceanothus jepsonii ssp. albiflorus",
                                       "Ceanothus papillosus ssp. roweanus",
                                       "Cordylanthus mollis",
                                       "Corethrogyne californica var. lyonii",
                                       "Corethrogyne californica var. obovata",
                                       "Cupressus goveniana ssp. goveniana",
                                       "Cuscuta brachycalyx var. brachycalyx",
                                       "Cuscuta brachycalyx var. apodanthera",
                                       "Cuscuta suksdorfii var. suksdorfii",
                                       "Cuscuta suksdorfii var. subpedicellata",
                                       "Diplacus lompocensis",
                                       "Downingia concolor var. tricolor",
                                       "Draba asterophora",
                                       "Draba corrugata var. corrugata",
                                       "Draba stenoloba var. ramosa",
                                       "Dudleya attenuata ssp. orcuttii",
                                       "Echinocereus engelmannii var. armatus",
                                       "Erigeron bloomeri var. pubens",
                                       "Erigeron uncialis ssp. uncialis",
                                       "Eriogonum deflexum var. rectum",
                                       "Eriogonum microthecum var. microthecum",
                                       "Eriogonum nutans var. glabratum",
                                       "Eriophyllum lanatum var. cuneatum",
                                       "Fritillaria affinis var. tristulis",
                                       "Glossopetalon pungens var. glabrum",
                                       "Hemizonia congesta ssp. leucocephala",
                                       "Hemizonia congesta ssp. vernalis",
                                       "Hemizonia corymbosa ssp. macrocephala",
                                       "Heterotheca sessiliflora var. sanjacintensis",
                                       "Heterotheca sessiliflora var. bolanderioides",
                                       "Holocarpha obconica ssp. autumnalis",
                                       "Horkelia daucifolia ssp. latior",
                                       "Ivesia argyrocoma",
                                       "Lathyrus vestitus ssp. laetiflorus",
                                       "Leptodactylon californicum ssp. glandulosum",
                                       "Lobelia dunnii var. dunnii",
                                       "Lupinus arbustus ssp. silvicola",
                                       "Lupinus adsurgens var. lilacinus",
                                       "Lupinus brevior",
                                       "Lupinus densiflorus var. austrocollium",
                                       "Lupinus densiflorus var. glareosus",
                                       "Lupinus lyallii ssp. lyallii",
                                       "Lupinus punto-reyesensis",
                                       "Lupinus sublanatus",
                                       "Lupinus subvexus var. albilanatus",
                                       "Marina orcuttii",
                                       "Mimulus biolettii",
                                       "Mimulus purpureus var. pauxillus",
                                       "Mimulus purpureus var. purpureus",
                                       "Mimulus latifolius",
                                       "Mimulus whitneyi",
                                       "Mirabilis californica var. cordifolia",
                                       "Monardella frutescens",
                                       "Monardella linoides ssp. stricta",
                                       "Monardella linoides ssp. viminea",
                                       "Notholaena californica ssp. leucophylla",
                                       "Opuntia californica var. californica",
                                       "Opuntia littoralis var. piercei",
                                       "Opuntia littoralis var. austrocalifornica",
                                       "Pedicularis densiflora ssp. aurantiaca",
                                       "Pentagramma triangularis ssp. semipallida",
                                       "Phalacroseris bolanderi var. coronata",
                                       "Philadelphus pumilus",
                                       "Physalis crassifolia var. crassifolia",
                                       "Physocarpus alternans ssp. panamintensis",
                                       "Plagiobothrys bracteatus var. aculeolatus",
                                       "Pinus muricata var. remorata",
                                       "Polyctenium fremontii var. confertum",
                                       "Physocarpus alternans ssp. annulatus",
                                       "Prunus subcordata var. rubicunda",
                                       "Rhododendron occidentale var. paludosum",
                                       "Quercus chrysolepis var. nana",
                                       "Rhododendron occidentale var. sonomense",
                                       "Ribes nevadense var. jaegeri",
                                       "Ribes nevadense var. glaucescens",
                                       "Ribes sanguineum var. deductum",
                                       "Ribes sanguineum var. melanocarpum",
                                       "Solanum xanti var. montanum",
                                       "Solanum xanti var. obispoense",
                                       "Streptanthus albidus",
                                       "Thelypodium howellii",
                                       "Vaccinium caespitosum var. paludicola",
                                       "Umbellularia californica var. fresnensis",
                                       "Viola adunca var. kirkii",
                                       "Viola beckwithii ssp. glabrata",
                                       "Viola pedunculata ssp. tenuifolia",
                                       "Lupinus polyphyllus ssp. bernardianus",
                                       "Lupinus tidestromii var. tidestromii",
                                       "Lupinus tidestromii var. layneae"), reviewStatus:="Not recognized in eFlora"]

# Get rid of duplicate of Lewisia kelloggii ssp. kelloggii (161323)
ns_globRareCA_jep[NATURESERVE_ID=="161323",reviewStatus:="Not recognized in eFlora"]

ns_globRareCA_jep[SCIENTIFIC_NAME=="Arabis hirshbergiae",`:=`(JepID="NOJEP-101", JepsonName="Boechera hirshbergiae",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Camissonia luciae",`:=`(JepID="1244", JepsonName="Camissoniopsis luciae",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Calystegia malacophylla var. berryi",`:=`(JepID="CNPSadd_10", JepsonName="Calystegia malacophylla subsp berryi",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Ceanothus papillosus ssp. papillosus",`:=`(JepID="1552", JepsonName="Ceanothus papillosus",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Corethrogyne californica var. californica",`:=`(JepID="UCSCADD-2018-2", JepsonName="Corethrogyne filaginifolia var. californica",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Cryptantha rostellata var. spithamea",`:=`(JepID="2143", JepsonName="Cryptantha spithamaea",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Cupressus abramsiana ssp. abramsiana",`:=`(JepID="3640", JepsonName="Hesperocyparis abramsiana var. abramsiana",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Dendromecon rigida ssp. rhamnoides",`:=`(JepID="NOJEP-88", JepsonName="Dendromecon harfordii var. rhamnoides",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Diplacus aurantiacus ssp. australis",`:=`(JepID="7251_Rev6", JepsonName="Diplacus australis",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Diplacus fasciculatus",`:=`(JepID="4751", JepsonName="Diplacus grandiflorus",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Dudleya cymosa ssp. costafolia",`:=`(JepID="2464", JepsonName="Dudleya cymosa subsp. costatifolia",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Eriogonum ochrocephalum var. alexandrae",`:=`(JepID="CNPSadd_45", JepsonName="Eriogonum alexanderae",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Fritillaria grayana",`:=`(JepID="CNPSadd_201", JepsonName="Fritillaria roderickii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Gilia austrooccidentalis",`:=`(JepID="3445", JepsonName="Gilia austro-occidentalis",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Heuchera alpestris",`:=`(JepID="3693", JepsonName="Heuchera parishii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Horkelia clevelandii",`:=`(JepID="3741", JepsonName="Horkelia clevelandii var. clevelandii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Ivesia purpurascens",`:=`(JepID="3771", JepsonName="Horkeliella purpurascens",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Lasthenia macrantha ssp. macrantha",`:=`(JepID="4053", JepsonName="Lasthenia californica subsp. macrantha",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Lathyrus jepsonii ssp. jepsonii",`:=`(JepID="4079", JepsonName="Lathyrus jepsonii var. jepsonii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Lupinus eximius",`:=`(JepID="CNPSadd_85", JepsonName="Lupinus arboreus var. eximius",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Lupinus latifolius ssp. dudleyi",`:=`(JepID="4476", JepsonName="Lupinus latifolius var. dudleyi",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus acutidens",`:=`(JepID="CNPSadd_46", JepsonName="Erythranthe acutidens",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus brachiatus",`:=`(JepID="4779", JepsonName="Erythranthe guttata",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus exiguus",`:=`(JepID="CNPSadd_47", JepsonName="Erythranthe exigua",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus filicaulis",`:=`(JepID="CNPSadd_48", JepsonName="Erythranthe filicaulis",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus fremontii var. vandenbergensis",`:=`(JepID="CNPSadd_40", JepsonName="Diplacus vandenbergensis",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus gracilipes",`:=`(JepID="NOJEP-72", JepsonName="Erythranthe gracilipes",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus grayi",`:=`(JepID="CNPSadd_51", JepsonName="Erythranthe grayi",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus purpureus",`:=`(JepID="NOJEP-81", JepsonName="Erythranthe purpurea",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus mohavensis",`:=`(JepID="CNPSadd_33", JepsonName="Diplacus mohavensis",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus norrisii",`:=`(JepID="CNPSadd_57", JepsonName="Erythranthe norrisii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus pictus",`:=`(JepID="CNPSadd_35", JepsonName="Diplacus pictus",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus primuloides var. linearifolius",`:=`(JepID="4804", JepsonName="Erythranthe linearifolia",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus pulchellus",`:=`(JepID="CNPSadd_36", JepsonName="Diplacus pulchellus",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus rattanii var. decurtatus",`:=`(JepID="4810", JepsonName="Diplacus rattanii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Mimulus shevockii",`:=`(JepID="NOJEP-76", JepsonName="Erythranthe shevockii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Opuntia californica var. californica",`:=`(JepID="2189", JepsonName="Cylindropuntia californica var. californica",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Orobanche californica ssp. condensa",`:=`(JepID="5193", JepsonName="Aphyllon californicum subsp. condensum",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Orobanche valida ssp. valida",`:=`(JepID="5208", JepsonName="Aphyllon validum subsp. validum",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Orobanche vallicola",`:=`(JepID="5209", JepsonName="Aphyllon vallicolum",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Petrophytum acuminatum",`:=`(JepID="5479", JepsonName="Petrophytum caespitosum subsp. acuminatum",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Physocarpus alternans ssp. annulatus",`:=`(JepID="5662", JepsonName="Physocarpus alternans",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Pinus contorta var. bolanderi",`:=`(JepID="5680", JepsonName="Pinus contorta subsp. bolanderi",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Silene nuda ssp. nuda",`:=`(JepID="6589", JepsonName="Silene nuda",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Silene parishii var. parishii",`:=`(JepID="6592", JepsonName="Silene parishii",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Smelowskia ovalis var. congesta",`:=`(JepID="6611", JepsonName="Smelowskia ovalis",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Streptanthus glandulosus var. secundus",`:=`(JepID="6782", JepsonName="Streptanthus glandulosus subsp. secundus",reviewStatus="Added Manually")]
ns_globRareCA_jep[SCIENTIFIC_NAME=="Viola lanceolata ssp. occidentalis",`:=`(JepID="7179", JepsonName="Viola primulifolia subsp. occidentalis",reviewStatus="Added Manually")]

ns_globRareCA_jep[reviewStatus!="Not recognized in eFlora",JepCount:=.N,by="JepID"]
# Adding Count of JepIDs
jepNSMatchforUpload <- ns_globRareCA_jep[reviewStatus!="Not recognized in eFlora",.(JepID,NATURESERVE_ID)]
write.csv(jepNSMatchforUpload,"Data/jepNSMatchforUpload.csv")


# Counting how many globally ranked species have been collected

spp <- fread("Data/tblSpeciesJepCAPR_2019-Sep-17_1647.csv")
cnps <- fread("Data/tblCNPSRanks_2019-Jun-21_1830.csv")
accCount <- acc[grepl("Seed",basisofRecord),.(CountAcc=.N),by=c("taxonID")]

merge1<-merge(spp[,.(name_minus_authors,nativity,family,JepID,NATURESERVE_ID)],accCount, by.y="taxonID",by.x="JepID",all.x=T)

nsMerge <- merge(merge1, ns[,.(NATURESERVE_ID,NATURESERVE_ROUNDED_G_RANK)],by="NATURESERVE_ID",all.x=T)
nsMerge[is.na(CountAcc),CountAcc:=0]

nsMerge[,GloballyRare:=ifelse(NATURESERVE_ROUNDED_G_RANK%in%c("G1","G2","T1","T2","GX","TX"),"Yes","No")]

nsMerge[,Collected:=ifelse(CountAcc>0,"Yes","No")]
nsMerge[GloballyRare=="Yes",.(Count=.N),by=c("GloballyRare","Collected")]


nsCNPSmerge = merge(nsMerge, cnps[,.(JepID,CRPR,GRankCNPS=GRank)], by="JepID",all.x=T)

nsCNPSmerge[,CNPS1b:=ifelse(grepl("1B",CRPR),"Yes","No")]

summary <- nsCNPSmerge[!(GloballyRare=="No" & CNPS1b=="No"),.(Count=.N),by=c("Collected","GloballyRare","CNPS1b")][order(Collected,GloballyRare)]


# Looking at if any species should be added to National Collection
cpc <- fread('Data/tblRareTaxonTable_2019-Sep-17_1734.csv')
nsCNPSmerge[,NATURESERVE_ID:=as.character(NATURESERVE_ID)]
cpcMerge <- merge(cpc[,.(CPCNumber,Taxon,In_National_Collection,NATURESERVE_ID)],nsCNPSmerge[,.(Collected,NATURESERVE_ID,name_minus_authors,NATURESERVE_ROUNDED_G_RANK,GloballyRare, CNPS1B)], by="NATURESERVE_ID",all.x=T)



cpcMerge[Collected=="Yes" & In_National_Collection==0][order(Taxon)]



