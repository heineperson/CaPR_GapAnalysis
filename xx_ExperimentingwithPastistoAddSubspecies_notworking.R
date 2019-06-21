## TO SET UP THIS CODE RUN FIRST TWO CHUNKS IN "02_Phylogenetic Tree Building"
## YOU MUST INSTALL MR BAYES THROUGH TERMINAL (UNIX)
# Goal: To use mr bayes to add subspecies to species clades
## THIS IS NOT WORKING

# Filtering Data
SppCopy <- SppVas1
SppCopy[,binomial:=word(species,1,2)]
SppCopy[,SppCount:=.N,by="binomial"]
SppCopy[,SppID:=1:.N,by="binomial"]
SppOnly <- SppCopy[SppID==1]
SppOnly[SppCount==1,species:=binomial]

# Set up species only dataset to bind to phylogeny
SppOnly <- SppOnly[,.(species=binomial, genus=genus,family=family, genus.relative=genus.relative,species.relative=species.relative)]

# Bind abies species to phylogeny
abiesBoundSppOnly <- bind.relative(as.data.frame(SppOnly[genus=="Abies"]))

# More Data prep
abiesList = SppCopy[genus=="Abies",.(taxon=species,clade=binomial)]
abiesList[,clade:=gsub(" ", "_",clade)]
abiesList[,taxon:=gsub(" ", "_",taxon)]
abiesList[taxon=="Abies_lasiocarpa_var._lasiocarpa",taxon:="Abies_lasiocarpa"]

# Run pastis function that creates appropriate Mr Bayes file
pastis_main(constraint_tree=abiesBoundSppOnly$phylo,taxa_list=as.data.frame(abiesList),output_file="MrBayesData/ABIES.nexus")

# Execute file through mr bayes (through terminal)
system("mb MrBayesData/ABIES.nexus")
execute <- system("execute MrBayesData/ABIES.nexus")

# Read in the consensus tree created in MrBayes
abiesTreeMCMC <- read.nexus("MrBayesData/ABIES.nexus.con.tre")

# Plot consenssu Tree - seems to not have added subspecies
plot(abiesTreeMCMC)

# Evaluate consensus tree (very slow)
 conch(constraint_tree=abiesBoundSppOnly$phylo,
 mrbayes_output='ABIES.nexus.t',
 simple_edge_scaling=TRUE)

# EXAMPLE CODE
# data(accipitridaeBasicPastis)
# pastis_main(accipitridaeBasicPastis, output_file="MrBayesData/AccipitridaeBasic.nexus")

