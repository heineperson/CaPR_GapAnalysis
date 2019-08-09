library(data.table)

# Read in Data
cnddb <- fread('caprPrioritizationApp/AppData/cnddbwithJepson.csv')
capr <- fread('caprPrioritizationApp/AppData/CaPRwithJepson.csv')
capr <- capr[Deaccession==FALSE  & decimalLatitude>0]

# Calculate Area Per Group For Federal Lands Layer


# Questions I want to answer

# DO I NEED A STATE PARKS LAYER? YES
# From the Federal Lands Layer, ilmcaPub_1 = "ST" is for state entities
# ilmcaPub_6 show these options for state bodies:  
#"Parks and Recreation" "Lands Commission" "Fish and Wildlife" "Wildlife Conservation Board"  "Other State Department"       "Forestry and Fire Protection"
cnddb[PRESENCE=="Presumed Extant",.(OWNERMGT,ilmcaPub_1,ilmcaPub_6,ilmcaPub19 ,ilmcaPub20)]

# DO I NEED A UC RESERVE LAYER? Yes


## What federal lands are best represented in our collection?
# Fixing ilmcaPub20 so that it is not NA for groups without lower subdivision
capr[is.na(ilmcaPub20),ilmcaPub20:=ilmcaPub_1]
capr[,.(Count=.N),by="ilmcaPub_1"][order(-Count)]
capr[,.(Count=.N),by="ilmcaPub20"][order(-Count)]
capr[,.(Count=.N),by="ilmcaPub19"][order(-Count)]

## What federal lands are best represented in our collection per unit land area?

## What proprotion of presumed extant rare plant occurrences have been collected on each federal land type? 
## What proportion of presumed extant rare plant occurrences have been colelcted on each management unit?

# What specific management units have the most uncollected species (most bang for buck in permissions)?